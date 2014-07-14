-------------------------------------------------------------------------------
--
-- A VGA line-doubler for an Apple ][
--
-- Stephen A. Edwards, sedwards.cs.columbia.edu
--
-- The Apple ][ uses a 14.31818 MHz master clock.  It outputs a new
-- horizontal line every 65 * 14 + 2 = 912 14M cycles.  The extra two
-- are from the "extended cycle" used to keep the 3.579545 MHz
-- colorburst signal in sync.  Of these, 40 * 14 = 560 are active video.
--
-- In graphics mode, the Apple effectively generates 140 four-bit pixels
-- output serially (i.e., with 3.579545 MHz pixel clock).  In text mode,
-- it generates 280 one-bit pixels (i.e., with a 7.15909 MHz pixel clock).
--
-- We capture 140 four-bit nibbles for each line and interpret them in
-- one of the two modes.  In graphics mode, each is displayed as a
-- single pixel of one of 16 colors.  In text mode, each is displayed
-- as two black or white pixels.
-- 
-- The VGA display is nominally 640 X 480, but we use a 14.31818 MHz
-- dot clock.  To stay in sync with the Apple, we generate a new line
-- every 912 / 2 = 456 14M cycles= 31.8 us, a 31.4 kHz horizontal
-- refresh rate.  Of these, 280 will be active video.
--
-- One set of suggested VGA timings:
--
--          ______________________          ________
-- ________|        VIDEO         |________| VIDEO
--     |-C-|----------D-----------|-E-|
-- __   ______________________________   ___________
--   |_|                              |_|
--   |B|
--   |---------------A----------------|
--
-- A = 31.77 us	 Scanline time
-- B =  3.77 us  Horizontal sync time
-- C =  1.89 us  Back porch
-- D = 25.17 us  Active video
-- E =  0.94 us  Front porch
--
-- We use A = 456 / 14.31818 MHz = 31.84 us
--        B =  54 / 14.31818 MHz =  3.77 us
--        C = 106 / 14.31818 MHz =  7.40 us
--        D = 280 / 14.31818 MHz = 19.56 us
--        E =  16 / 14.31818 MHz =  1.12 us
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity vga_controller is
  
  port (
    CLK_14M    : in  std_logic;	     -- 14.31818 MHz master clock

    VIDEO      : in std_logic;         -- from the Apple video generator
    COLOR_LINE : in std_logic;
    HBL        : in std_logic;
    VBL        : in std_logic;
    LD194      : in std_logic;
    
    VGA_CLK    : out std_logic;
    VGA_HS     : out std_logic;             -- Active low
    VGA_VS     : out std_logic;             -- Active low
    VGA_BLANK  : out std_logic;
    VGA_R      : out unsigned(9 downto 0);
    VGA_G      : out unsigned(9 downto 0);
    VGA_B      : out unsigned(9 downto 0)
    );
  
end vga_controller;

architecture rtl of vga_controller is

  -- Double-ported RAM (one read port, one write port)
  -- that holds two lines of 140 pixels, 16 colors each
  type line_memory_t is array (0 to 511) of unsigned(3 downto 0);
  signal line_memory : line_memory_t;

  signal ram_write_addr : unsigned(8 downto 0);
  signal ram_we : std_logic;
  signal ram_read_addr : unsigned(8 downto 0);
  signal ram_data_out : unsigned(3 downto 0);

  -- Serial-to-parallel shift register that reads the incoming video
  signal shift_reg : unsigned(3 downto 0);

  signal last_hbl : std_logic;
  signal hcount : unsigned(9 downto 0);
  signal hcount2 : unsigned(9 downto 0);
  signal vcount : unsigned(5 downto 0);
  signal even_line : std_logic;
  signal hactive : std_logic;

  constant VGA_SCANLINE : integer := 456;  -- Must be 456 (set by the Apple)
  
  constant VGA_HSYNC : integer := 54;
  constant VGA_BACK_PORCH : integer := 66;
  constant VGA_ACTIVE : integer := 282;  -- Must be 280 (set by the Apple)
  constant VGA_FRONT_PORCH : integer := 54;

  -- VGA_HSYNC + VGA_BACK_PORCH + VGA_ACTIVE + VGA_FRONT_PORCH = VGA_SCANLINE

  constant VBL_TO_VSYNC : integer := 33;
  constant VGA_VSYNC_LINES : integer := 3;

  signal VGA_VS_I, VGA_HS_I : std_logic;

  signal video_active : std_logic;
  signal vbl_delayed : std_logic;
  signal hbl_delayed : std_logic;
  signal color_line_delayed_1, color_line_delayed_2 : std_logic;

begin

  delay_hbl : process (CLK_14M)
  begin
    if rising_edge(CLK_14M) then
      if LD194 = '0' then
        hbl_delayed <= HBL;
      end if;
    end if;
  end process;

  hcount_vcount_control : process (CLK_14M)
  begin
    if rising_edge(CLK_14M) then
      if last_hbl = '1' and hbl_delayed = '0' then  -- Falling edge
        color_line_delayed_2 <= color_line_delayed_1;
        color_line_delayed_1 <= COLOR_LINE;
        hcount <= (others => '0');
        vbl_delayed <= VBL;
        if VBL = '1' then
          even_line <= '0';
          vcount <= vcount + 1;
        else
          vcount <= (others => '0');
          even_line <= not even_line;
        end if;
      else
        hcount <= hcount + 1;
      end if;
      last_hbl <= hbl_delayed;
    end if;
  end process hcount_vcount_control;

  hsync_gen : process (CLK_14M)
  begin
    if rising_edge(CLK_14M) then
      if hcount = VGA_ACTIVE + VGA_FRONT_PORCH or
        hcount = VGA_SCANLINE + VGA_ACTIVE + VGA_FRONT_PORCH then
        VGA_HS_I <= '0';
      elsif hcount = VGA_ACTIVE + VGA_FRONT_PORCH + VGA_HSYNC or
        hcount = VGA_SCANLINE + VGA_ACTIVE + VGA_FRONT_PORCH + VGA_HSYNC then
        VGA_HS_I <= '1';
      end if;

      if hcount = VGA_SCANLINE - 1 or
        hcount = VGA_SCANLINE + VGA_SCANLINE - 1 then
        hactive <= '1';
      elsif hcount = VGA_ACTIVE or
        hcount = VGA_ACTIVE + VGA_SCANLINE then
        hactive <= '0';
      end if;
    end if;
  end process hsync_gen;

  VGA_HS <= VGA_HS_I;

  vsync_gen : process (CLK_14M)
  begin
    if rising_edge(CLK_14M) then
      if vcount = VBL_TO_VSYNC then
        VGA_VS_I <= '0';
      elsif vcount = VBL_TO_VSYNC + VGA_VSYNC_LINES then
        VGA_VS_I <= '1';
      end if;
    end if;
  end process vsync_gen;

  VGA_VS <= VGA_VS_I;

  -- Shift in the incoming bits to reconstruct four-bit groups
  input_shift_register : process (CLK_14M)
  begin
    if rising_edge(CLK_14M) then
      shift_reg <= VIDEO & shift_reg(3 downto 1);
    end if;
  end process input_shift_register;

  hcount2 <= hcount - VGA_SCANLINE;

  ram_read_addr <=
    even_line & hcount(8 downto 1) when hcount < VGA_SCANLINE else
    even_line & hcount2(8 downto 1);
  
  ram_write_addr <= (not even_line) & hcount(9 downto 2);
  ram_we <= '1' when hcount(1 downto 0) = "00" else '0';

  video_active <= hactive and not vbl_delayed;

  -- RGB values from Linards Ticmanis,
  -- http://newsgroups.derkeiler.com/Archive/Comp/comp.sys.apple2/2005-09/msg00534.html

  pixel_generator: process (CLK_14M)
  begin
    if rising_edge(CLK_14M) then
      if video_active = '1' then
        if color_line_delayed_2 = '0' then
          if (hcount(0) = '1' and ram_data_out(0) = '1')
            or (hcount(0) = '0' and ram_data_out(2) = '1') then
            -- White text pixel
            VGA_R <= (others => '1');
            VGA_G <= (others => '1');
            VGA_B <= (others => '1');
          else
            -- Black text pixel
            VGA_R <= (others => '0');
            VGA_G <= (others => '0');
            VGA_B <= (others => '0');
          end if;
        else
          VGA_R(0) <= '0';
          VGA_G(0) <= '0';
          VGA_B(0) <= '0';
          case ram_data_out is
            when "0000" =>              -- Black
              VGA_R(8 downto 1) <= X"00";
              VGA_G(8 downto 1) <= X"00";
              VGA_B(8 downto 1) <= X"00";
            when "0001" =>              -- Magenta
              VGA_R(8 downto 1) <= X"8a";
              VGA_G(8 downto 1) <= X"21";
              VGA_B(8 downto 1) <= X"40";
            when "0010" =>              -- Dark Blue
              VGA_R(8 downto 1) <= X"3C";
              VGA_G(8 downto 1) <= X"22";
              VGA_B(8 downto 1) <= X"A5";
            when "0011" =>              -- Light Purple
              VGA_R(8 downto 1) <= X"c8";
              VGA_G(8 downto 1) <= X"47";
              VGA_B(8 downto 1) <= X"e4";
            when "0100" =>              -- Dark Green
              VGA_R(8 downto 1) <= X"07";
              VGA_G(8 downto 1) <= X"65";
              VGA_B(8 downto 1) <= X"3E";
            when "0101" | "1010" =>     -- Grey
              VGA_R(8 downto 1) <= X"7B";
              VGA_G(8 downto 1) <= X"7E";
              VGA_B(8 downto 1) <= X"80";
            when "0110" =>              -- Medium Blue
              VGA_R(8 downto 1) <= X"30";
              VGA_G(8 downto 1) <= X"8F";
              VGA_B(8 downto 1) <= X"E3";
            when "0111" =>              -- Light Blue
              VGA_R(8 downto 1) <= X"B9";
              VGA_G(8 downto 1) <= X"A9";
              VGA_B(8 downto 1) <= X"FD";
            when "1000" =>              -- Brown
              VGA_R(8 downto 1) <= X"3B";
              VGA_G(8 downto 1) <= X"51";
              VGA_B(8 downto 1) <= X"07";
            when "1001" =>              -- Orange
              VGA_R(8 downto 1) <= X"C7";
              VGA_G(8 downto 1) <= X"70";
              VGA_B(8 downto 1) <= X"28";
            when "1011" =>              -- Pink
              VGA_R(8 downto 1) <= X"F3";
              VGA_G(8 downto 1) <= X"9A";
              VGA_B(8 downto 1) <= X"C2";
            when "1100" =>              -- Green
              VGA_R(8 downto 1) <= X"2F";
              VGA_G(8 downto 1) <= X"B8";
              VGA_B(8 downto 1) <= X"1F";
            when "1101" =>              -- Yellow
              VGA_R(8 downto 1) <= X"b9";
              VGA_G(8 downto 1) <= X"d0";
              VGA_B(8 downto 1) <= X"60";
            when "1110" =>              -- Blue/Green
              VGA_R(8 downto 1) <= X"6e";
              VGA_G(8 downto 1) <= X"e1";
              VGA_B(8 downto 1) <= X"c0";
            when "1111" =>              -- White
              VGA_R(8 downto 1) <= X"ff";
              VGA_G(8 downto 1) <= X"ff";
              VGA_B(8 downto 1) <= X"ff";
            when others =>
              VGA_R(8 downto 1) <= (others => 'X');
              VGA_G(8 downto 1) <= (others => 'X');
              VGA_B(8 downto 1) <= (others => 'X');
          end case;
        end if;
      else
        -- Blanking
        VGA_R <= (others => '0');
        VGA_G <= (others => '0');
        VGA_B <= (others => '0');           
      end if;
    end if;
  end process pixel_generator;

  -- The two-port RAM that stores the line data
  line_storage : process (CLK_14M)
  begin
    if rising_edge(CLK_14M) then
      if ram_we = '1' then
        line_memory(to_integer(ram_write_addr)) <= shift_reg;
      end if;
      ram_data_out <= line_memory(to_integer(ram_read_addr));
    end if;
  end process line_storage;

  VGA_CLK <= CLK_14M;

  VGA_BLANK <= video_active;

end rtl;
