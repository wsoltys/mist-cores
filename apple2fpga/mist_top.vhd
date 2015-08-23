--
-- mist_top.vhd.vhd
--
-- Apple II+ toplevel for the MiST board
-- https://github.com/wsoltys/mist_apple2
--
-- Copyright (c) 2014 W. Soltys <wsoltys@gmail.com>
--
-- This source file is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published
-- by the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This source file is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity mist_top is

  port (
    -- Clocks
    
    CLOCK_27    : in std_logic_vector(1 downto 0); -- 27 MHz


    -- SDRAM
    SDRAM_nCS : out std_logic; -- Chip Select
    SDRAM_DQ : inout std_logic_vector(15 downto 0); -- SDRAM Data bus 16 Bits
    SDRAM_A : out std_logic_vector(12 downto 0); -- SDRAM Address bus 13 Bits
    SDRAM_DQMH : out std_logic; -- SDRAM High Data Mask
    SDRAM_DQML : out std_logic; -- SDRAM Low-byte Data Mask
    SDRAM_nWE : out std_logic; -- SDRAM Write Enable
    SDRAM_nCAS : out std_logic; -- SDRAM Column Address Strobe
    SDRAM_nRAS : out std_logic; -- SDRAM Row Address Strobe
    SDRAM_BA : out std_logic_vector(1 downto 0); -- SDRAM Bank Address
    SDRAM_CLK : out std_logic; -- SDRAM Clock
    SDRAM_CKE: out std_logic; -- SDRAM Clock Enable
    
    -- SPI
    SPI_SCK : in std_logic;
    SPI_DI : in std_logic;
    SPI_DO : out std_logic;
    SPI_SS2 : in std_logic;
    SPI_SS3 : in std_logic;
    CONF_DATA0 : in std_logic;

    -- VGA output
    

    VGA_HS,                                             -- H_SYNC
    VGA_VS : out std_logic;                             -- V_SYNC
    VGA_R,                                              -- Red[5:0]
    VGA_G,                                              -- Green[5:0]
    VGA_B : out std_logic_vector(5 downto 0);           -- Blue[5:0]
    
    -- Audio
    AUDIO_L,
    AUDIO_R : out std_logic;
    
    -- LEDG
    LED : out std_logic

    );
  
end mist_top;

architecture datapath of mist_top is

  constant CONF_STR : string := "AppleII+;NIB;S1,NIB;O2,Monitor Type,Color,Monochrome;O3,Monitor Mode,Main,Alt;O4,Enable Scanlines,off,on;O5,Joysticks,Normal,Swapped;";

  function to_slv(s: string) return std_logic_vector is 
    constant ss: string(1 to s'length) := s; 
    variable rval: std_logic_vector(1 to 8 * s'length); 
    variable p: integer; 
    variable c: integer; 
  
  begin 
    for i in ss'range loop
      p := 8 * i;
      c := character'pos(ss(i));
      rval(p - 7 to p) := std_logic_vector(to_unsigned(c,8)); 
    end loop; 
    return rval; 

  end function; 



  component user_io 
    generic ( STRLEN : integer := 0 );
     port (
            SPI_CLK, SPI_SS_IO, SPI_MOSI :in std_logic;
            SPI_MISO : out std_logic;
            conf_str : in std_logic_vector(8*STRLEN-1 downto 0);
            joystick_0 : out std_logic_vector(5 downto 0);
            joystick_1 : out std_logic_vector(5 downto 0);
            joystick_analog_0 : out std_logic_vector(15 downto 0);
            joystick_analog_1 : out std_logic_vector(15 downto 0);
            status: out std_logic_vector(7 downto 0);
            switches : out std_logic_vector(1 downto 0);
            buttons : out std_logic_vector(1 downto 0);
            sd_lba : in std_logic_vector(31 downto 0);
            sd_rd : in std_logic;
            sd_wr : in std_logic;
            sd_ack : out std_logic;
            sd_conf : in std_logic;
            sd_sdhc : in std_logic;
            sd_dout : out std_logic_vector(7 downto 0);
            sd_dout_strobe : out std_logic;
            sd_din : in std_logic_vector(7 downto 0);
            sd_din_strobe : out std_logic;
            sd_change : out std_logic;
            ps2_clk : in std_logic;
            ps2_kbd_clk : out std_logic;
            ps2_kbd_data : out std_logic
        );
  end component user_io;
  
  component sd_card
    port (io_lba : out std_logic_vector(31 downto 0);
          io_rd : out std_logic;
          io_wr : out std_logic;
          io_ack : in std_logic;
          io_sdhc : out std_logic;
          io_conf : out std_logic;
          io_din : in std_logic_vector(7 downto 0);
          io_din_strobe : in std_logic;
          io_dout : out std_logic_vector(7 downto 0);
          io_dout_strobe : in std_logic;
          allow_sdhc : in std_logic;
          sd_cs : in std_logic;
          sd_sck : in std_logic;
          sd_sdi : in std_logic;
          sd_sdo : out std_logic
    );
  end component sd_card;
  
  component data_io is
    port ( sck: in std_logic;
           ss: in std_logic;
           sdi: in std_logic;
           downloading: out std_logic;
           index: out std_logic_vector(4 downto 0);
           size: out std_logic_vector(24 downto 0);
           clk: in std_logic;
           wr: out std_logic;
           a: out std_logic_vector(24 downto 0);
           d: out std_logic_vector(7 downto 0));
  end component;
  
  component sdram is
    port( sd_data : inout std_logic_vector(15 downto 0);
          sd_addr : out std_logic_vector(12 downto 0);
          sd_dqm : out std_logic_vector(1 downto 0);
          sd_ba : out std_logic_vector(1 downto 0);
          sd_cs : out std_logic;
          sd_we : out std_logic;
          sd_ras : out std_logic;
          sd_cas : out std_logic;
          init : in std_logic;
          clk : in std_logic;
          clkref : in std_logic;
          din : in std_logic_vector(7 downto 0);
          dout : out std_logic_vector(7 downto 0);
          addr : in std_logic_vector(24 downto 0);
          oe : in std_logic;
          we : in std_logic
    );
  end component;
  
  component osd
    port ( pclk, sck, ss, sdi, hs_in, vs_in, scanline_ena_h : in std_logic;
           red_in, blue_in, green_in : in std_logic_vector(5 downto 0);
           red_out, blue_out, green_out : out std_logic_vector(5 downto 0);
           hs_out, vs_out : out std_logic
         );
  end component osd;

  signal CLK_114M, CLK_28M, CLK_14M, CLK_2M, PRE_PHASE_ZERO, CLK_12k : std_logic;
  signal clk_div : unsigned(1 downto 0);
  signal IO_SELECT, DEVICE_SELECT : std_logic_vector(7 downto 0);
  signal ADDR : unsigned(15 downto 0);
  signal D, PD, ram_do, ram_di : unsigned(7 downto 0);
  signal DO : std_logic_vector(7 downto 0);

  signal ram_we : std_logic;
  signal VIDEO, HBL, VBL, LD194 : std_logic;
  signal COLOR_LINE : std_logic;
  signal COLOR_LINE_CONTROL : std_logic;
  signal SCREEN_MODE : std_logic_vector(1 downto 0);
  signal GAMEPORT : std_logic_vector(7 downto 0);
  signal cpu_pc : unsigned(15 downto 0);

  signal K : unsigned(7 downto 0);
  signal read_key : std_logic;

  signal flash_clk : unsigned(22 downto 0) := (others => '0');
  signal power_on_reset : std_logic := '1';
  signal force_reset : std_logic := '0';
  signal reset : std_logic;

  signal D1_ACTIVE, D2_ACTIVE : std_logic;
  signal track_addr : unsigned(13 downto 0);
  signal TRACK_RAM_ADDR : unsigned(13 downto 0);
  signal tra : unsigned(15 downto 0);
  signal TRACK_RAM_DI : unsigned(7 downto 0);
  signal TRACK_RAM_WE : std_logic;
  signal track : unsigned(5 downto 0);
  signal image : unsigned(9 downto 0);

  signal CS_N, MOSI, MISO, SCLK : std_logic;
  
  signal downl : std_logic := '0';
  signal io_index : std_logic_vector(4 downto 0);
  signal size : std_logic_vector(24 downto 0) := (others=>'0');
  signal a_ram: unsigned(15 downto 0);
  signal r : unsigned(9 downto 0);
  signal g : unsigned(9 downto 0);
  signal b : unsigned(9 downto 0);
  signal hsync : std_logic;
  signal vsync : std_logic;
  signal sd_we : std_logic;
  signal sd_oe : std_logic;
  signal sd_addr : std_logic_vector(18 downto 0);
  signal sd_di : std_logic_vector(7 downto 0);
  signal sd_do : std_logic_vector(7 downto 0);
  signal io_we : std_logic;
  signal io_addr : std_logic_vector(24 downto 0);
  signal io_do : std_logic_vector(7 downto 0);
  signal io_ram_we : std_logic;
  signal io_ram_d : std_logic_vector(7 downto 0);
  signal io_ram_addr : std_logic_vector(18 downto 0);
  
  signal switches   : std_logic_vector(1 downto 0);
  signal buttons    : std_logic_vector(1 downto 0);
  signal joy        : std_logic_vector(5 downto 0);
  signal joy0       : std_logic_vector(5 downto 0);
  signal joy1       : std_logic_vector(5 downto 0);
  signal joy_an0    : std_logic_vector(15 downto 0);
  signal joy_an1    : std_logic_vector(15 downto 0);
  signal joy_an     : std_logic_vector(15 downto 0);
  signal status     : std_logic_vector(7 downto 0);
  signal ps2Clk     : std_logic;
  signal ps2Data    : std_logic;
  signal audio      : std_logic;
  
  -- signals to connect sd card emulation with io controller
  signal sd_lba:  std_logic_vector(31 downto 0);
  signal sd_rd:   std_logic;
  signal sd_wr:   std_logic;
  signal sd_ack:  std_logic;
  signal sd_conf: std_logic;
  signal sd_sdhc: std_logic;
  
  -- data from io controller to sd card emulation
  signal sd_data_in: std_logic_vector(7 downto 0);
  signal sd_data_in_strobe:  std_logic;
  signal sd_data_out: std_logic_vector(7 downto 0);
  signal sd_data_out_strobe:  std_logic;
  
  -- sd card emulation
  signal sd_cs:	std_logic;
  signal sd_sck:	std_logic;
  signal sd_sdi:	std_logic;
  signal sd_sdo:	std_logic;
  
  signal pll_locked : std_logic;
  signal sdram_dqm: std_logic_vector(1 downto 0);
  signal joyx       : std_logic;
  signal joyy       : std_logic;
  signal pdl_strobe : std_logic;

begin

  reset <= status(0) or buttons(1) or power_on_reset or force_reset;

  power_on : process(CLK_14M)
  begin
    if rising_edge(CLK_14M) then
      if flash_clk(22) = '1' then
        power_on_reset <= '0';
      end if;
    end if;
  end process;
  

  -- In the Apple ][, this was a 555 timer
  flash_clkgen : process (CLK_14M)
  begin
    if rising_edge(CLK_14M) then
      flash_clk <= flash_clk + 1;
    end if;     
  end process;

  pll : entity work.mist_clk 
  port map (
    areset => '0',
    inclk0 => CLOCK_27(0),
    c0     => CLK_114M,
    c1     => SDRAM_CLK,
    c2     => CLK_12k,
    locked => pll_locked
    );
    
  -- generate 28.6MHz video clock from 114.4MHz main clock by dividing it by 4
  process(CLK_114M)
  begin
    if rising_edge(CLK_114M) then
      clk_div <= clk_div + 1;
    end if;
     
    CLK_28M <= clk_div(1);
  end process;
  
  -- generate 14.3MHz system clock from 28.6MHz video clock
  process(CLK_28M)
  begin
    if rising_edge(CLK_28M) then
      CLK_14M <= not CLK_14M;
    end if;
  end process;

  -- Paddle buttons
  -- GAMEPORT input bits:
  --  7    6    5    4    3   2   1    0
  -- pdl3 pdl2 pdl1 pdl0 pb3 pb2 pb1 casette
  GAMEPORT <=  "00" & joyy & joyx & "0" & joy(5) & joy(4) & "0";
  
  joy_an <= joy_an0 when status(5)='0' else joy_an1;
  joy <= joy0 when status(5)='0' else joy1;
  
  process(CLK_2M, pdl_strobe)
    variable cx, cy : integer range -100 to 5800 := 0;
  begin
    if rising_edge(CLK_2M) then
      if cx > 0 then
        cx := cx -1;
        joyx <= '1';
      else
        joyx <= '0';
      end if;
      if cy > 0 then
        cy := cy -1;
        joyy <= '1';
      else
        joyy <= '0';
      end if;
      if pdl_strobe = '1' then
        cx := 2800+(22*to_integer(signed(joy_an(15 downto 8))));
        cy := 2800+(22*to_integer(signed(joy_an(7 downto 0)))); -- max 5650
        if cx < 0 then
          cx := 0;
        elsif cx >= 5590 then
          cx := 5650;
        end if;
        if cy < 0 then
          cy := 0;
        elsif cy >= 5590 then
          cy := 5650;
        end if;
      end if;
    end if;
  end process;

  COLOR_LINE_CONTROL <= COLOR_LINE and not (status(2) or status(3));  -- Color or B&W mode
  SCREEN_MODE <= status(2) & status(3); -- 00: Color, 01: B&W, 10:Green, 11: Amber
  
  -- sdram interface
  SDRAM_CKE <= '1';
  SDRAM_DQMH <= sdram_dqm(1);
  SDRAM_DQML <= sdram_dqm(0);

  sdram_inst : sdram
    port map( sd_data => SDRAM_DQ,
              sd_addr => SDRAM_A,
              sd_dqm => sdram_dqm,
              sd_cs => SDRAM_nCS,
              sd_ba => SDRAM_BA,
              sd_we => SDRAM_nWE,
              sd_ras => SDRAM_nRAS,
              sd_cas => SDRAM_nCAS,
              clk => CLK_114M,
              clkref => CLK_14M,
              init => not pll_locked,
              din => sd_di,
              addr => "000000" & sd_addr,
              we => sd_we,
              oe => sd_oe,
              dout => sd_do
    );
  
--  data_io_inst: data_io
--    port map(SPI_SCK, SPI_SS2, SPI_DI, downl, io_index, size, CLK_14M, io_we, io_addr, io_do);
--    
--  sd_addr <= io_ram_addr when downl = '1' else std_logic_vector(TRACK_RAM_ADDR);
--  sd_di <= io_ram_d;
--  sd_oe <= '0' when downl = '1' else TRACK_RAM_OE;
--  sd_we <= '1' when io_ram_we = '1' else '0';
--    
--  process (CLK_14M)
--  begin
--    if falling_edge(CLK_14M) then
--      if io_we = '1' then
--        io_ram_we <= '1';
--        if unsigned(io_index) = 1 then
--          io_ram_addr <= '0' & io_addr(17 downto 0);
--        elsif unsigned(io_index) = 2 then
--          io_ram_addr <= '1' & io_addr(17 downto 0);
--        end if;
--        io_ram_d <= io_do;
--      else
--        io_ram_we <= '0';
--      end if;
--    end if;
--  end process;
  
  ram_inst : entity work.spram
    generic map
    (
      widthad_a	=> 16
    )
    port map
    (
      clock	=> CLK_14M,
      address	=> std_logic_vector(a_ram),
      wren	=> ram_we,
      data	=> std_logic_vector(D),
      q	=> DO
    );
  
  core : entity work.apple2 port map (
    CLK_14M        => CLK_14M,
    CLK_2M         => CLK_2M,
    PRE_PHASE_ZERO => PRE_PHASE_ZERO,
    FLASH_CLK      => flash_clk(22),
    reset          => reset,
    ADDR           => ADDR,
    ram_addr       => a_ram,
    D              => D,
    ram_do         => unsigned(DO),
    PD             => PD,
    ram_we         => ram_we,
    VIDEO          => VIDEO,
    COLOR_LINE     => COLOR_LINE,
    HBL            => HBL,
    VBL            => VBL,
    LD194          => LD194,
    K              => K,
    read_key       => read_key,
    AN             => open,
    GAMEPORT       => GAMEPORT,
    PDL_strobe     => pdl_strobe,
    IO_SELECT      => IO_SELECT,
    DEVICE_SELECT  => DEVICE_SELECT,
    pcDebugOut     => cpu_pc,
    speaker        => audio
    );
    
  AUDIO_L <= audio;
  AUDIO_R <= audio;

  vga : entity work.vga_controller port map (
    CLK_28M    => CLK_28M,
    VIDEO      => VIDEO,
    COLOR_LINE => COLOR_LINE_CONTROL,
	 SCREEN_MODE => SCREEN_MODE,
    HBL        => HBL,
    VBL        => VBL,
    LD194      => LD194,
    VGA_CLK    => open,
    VGA_HS     => hsync,
    VGA_VS     => vsync,
    VGA_BLANK  => open,
    VGA_R      => r,
    VGA_G      => g,
    VGA_B      => b
    );

  keyboard : entity work.keyboard port map (
    PS2_Clk  => ps2Clk,
    PS2_Data => ps2Data,
    CLK_14M  => CLK_14M,
    reset    => reset,
    reads     => read_key,
    K        => K
    );

  disk : entity work.disk_ii port map (
    CLK_14M        => CLK_14M,
    CLK_2M         => CLK_2M,
    PRE_PHASE_ZERO => PRE_PHASE_ZERO,
    IO_SELECT      => IO_SELECT(6),
    DEVICE_SELECT  => DEVICE_SELECT(6),
    RESET          => reset,
    A              => ADDR,
    D_IN           => D,
    D_OUT          => PD,
    TRACK          => TRACK,
    TRACK_ADDR     => TRACK_ADDR,
    D1_ACTIVE      => D1_ACTIVE,
    D2_ACTIVE      => D2_ACTIVE,
    ram_write_addr => TRACK_RAM_ADDR,
    ram_di         => TRACK_RAM_DI,
    ram_we         => TRACK_RAM_WE
    );

  sdcard_interface : entity work.spi_controller port map (
    CLK_14M        => CLK_14M,
    RESET          => RESET,

    CS_N           => sd_cs,
    MOSI           => sd_sdi,
    MISO           => sd_sdo,
    SCLK           => sd_sck,
    
    track          => TRACK,
    image          => (others=>'0'),
    
    ram_write_addr => TRACK_RAM_ADDR,
    ram_di         => TRACK_RAM_DI,
    ram_we         => TRACK_RAM_WE
    );
    
  LED <= not D1_ACTIVE;
  
  user_io_d : user_io
    generic map (STRLEN => CONF_STR'length)
    
    port map ( 
      SPI_CLK => SPI_SCK,
      SPI_SS_IO => CONF_DATA0,    
      SPI_MISO => SPI_DO,    
      SPI_MOSI => SPI_DI,       
      conf_str => to_slv(CONF_STR),
      status => status,   
      joystick_0 => joy0,   
      joystick_1 => joy1,
      joystick_analog_0 => joy_an0,
      joystick_analog_1 => joy_an1,
      SWITCHES => switches,   
      BUTTONS => buttons,
      -- connection to io controller
      sd_lba  => sd_lba,
      sd_rd   => sd_rd,
      sd_wr   => sd_wr,
      sd_ack  => sd_ack,
      sd_sdhc => sd_sdhc,
      sd_conf => sd_conf,
      sd_dout => sd_data_in,
      sd_dout_strobe => sd_data_in_strobe,
      sd_din => sd_data_out,
      sd_din_strobe => sd_data_out_strobe,
      ps2_clk => CLK_12k,
      ps2_kbd_clk => ps2Clk,
      ps2_kbd_data => ps2Data
    );
    
  sd_card_d: component sd_card
    port map
    (
      -- connection to io controller
      io_lba => sd_lba,
      io_rd  => sd_rd,
      io_wr  => sd_wr,
      io_ack => sd_ack,
      io_conf => sd_conf,
      io_sdhc => sd_sdhc,
      io_din => sd_data_in,
      io_din_strobe => sd_data_in_strobe,
      io_dout => sd_data_out,
      io_dout_strobe => sd_data_out_strobe,
   
      allow_sdhc  => '1',
      
      -- connection to host
      sd_cs  => sd_cs,
      sd_sck => sd_sck,
      sd_sdi => sd_sdi,
      sd_sdo => sd_sdo		
    );
    
  osd_inst : osd
    port map (
      pclk => CLK_14M,
      sdi => SPI_DI,
      sck => SPI_SCK,
      ss => SPI_SS3,
      red_in => std_logic_vector(r(9 downto 4)),
      green_in => std_logic_vector(g(9 downto 4)),
      blue_in => std_logic_vector(b(9 downto 4)),
      hs_in => not hsync,
      vs_in => not vsync,
      scanline_ena_h => status(4),
      red_out => VGA_R,
      green_out => VGA_G,
      blue_out => VGA_B,
      hs_out => VGA_HS,
      vs_out => VGA_VS
    );

end datapath;