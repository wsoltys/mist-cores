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

  constant CONF_STR : string := "AppleII+;NIB;O1,Video mode,Color,B&W;";

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
  
    port ( SPI_CLK, SPI_SS_IO, SPI_MOSI :in std_logic;
           SPI_MISO : out std_logic;
           conf_str : in std_logic_vector(8*STRLEN-1 downto 0);
           JOY0 :     out std_logic_vector(5 downto 0);
           JOY1 :     out std_logic_vector(5 downto 0);
           status:    out std_logic_vector(7 downto 0);
           SWITCHES : out std_logic_vector(1 downto 0);
           BUTTONS : out std_logic_vector(1 downto 0);
           clk : in std_logic;
           ps2_clk : out std_logic;
           ps2_data : out std_logic
         );

  end component user_io;
  
  component data_io is
    port ( sck: in std_logic;
           ss: in std_logic;
           sdi: in std_logic;
           downloading: out std_logic;
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
    port ( pclk, sck, ss, sdi, hs_in, vs_in : in std_logic;
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
  signal TRACK_RAM_ADDR : unsigned(17 downto 0);
  signal tra : unsigned(15 downto 0);
  signal TRACK_RAM_DI : unsigned(7 downto 0);
  signal TRACK_RAM_OE : std_logic;

  signal CS_N, MOSI, MISO, SCLK : std_logic;
  
  signal downl : std_logic := '0';
  signal size : std_logic_vector(24 downto 0) := (others=>'0');
  signal a_ram: unsigned(15 downto 0);
  signal r : unsigned(9 downto 0);
  signal g : unsigned(9 downto 0);
  signal b : unsigned(9 downto 0);
  signal hsync : std_logic;
  signal vsync : std_logic;
  signal sd_we : std_logic;
  signal sd_oe : std_logic;
  signal sd_addr : std_logic_vector(17 downto 0);
  signal sd_di : std_logic_vector(7 downto 0);
  signal sd_do : std_logic_vector(7 downto 0);
  signal io_we : std_logic;
  signal io_addr : std_logic_vector(24 downto 0);
  signal io_do : std_logic_vector(7 downto 0);
  signal io_ram_we : std_logic;
  signal io_ram_d : std_logic_vector(7 downto 0);
  signal io_ram_addr : std_logic_vector(17 downto 0);
  
  signal switches   : std_logic_vector(1 downto 0);
  signal buttons    : std_logic_vector(1 downto 0);
  signal joy0       : std_logic_vector(5 downto 0);
  signal joy1       : std_logic_vector(5 downto 0);
  signal status     : std_logic_vector(7 downto 0);
  signal ps2Clk     : std_logic;
  signal ps2Data    : std_logic;
  signal audio      : std_logic;
  
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
  GAMEPORT <=  "00" & joyy & joyx & "0" & joy0(5) & joy0(4) & "0";
  
  process(CLK_2M, pdl_strobe, joy0)
    variable cx, cy : integer := 0;
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
        if joy0(0) = '1' then     -- right
          cx := 4500;
        elsif joy0(1) = '1' then  -- left
          cx := 1400;
        else                      -- center
          cx := 2900;
        end if;
      end if;
      if pdl_strobe = '1' then
        if joy0(2) = '1' then     -- down
          cy := 4500;
        elsif joy0(3) = '1' then  -- up
          cy := 1400;
        else                      -- center
          cy := 2900;
        end if;
      end if;
    end if;
  end process;

  COLOR_LINE_CONTROL <= COLOR_LINE and not status(1);  -- Color or B&W mode
  
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
              addr => "0000000" & sd_addr,
              we => sd_we,
              oe => sd_oe,
              dout => sd_do
    );
  
  data_io_inst: data_io
    port map(SPI_SCK, SPI_SS2, SPI_DI, downl, size, CLK_14M, io_we, io_addr, io_do);
    
  sd_addr <= io_ram_addr when downl = '1' else std_logic_vector(TRACK_RAM_ADDR);
  sd_di <= io_ram_d;
  sd_oe <= '0' when downl = '1' else TRACK_RAM_OE;
  sd_we <= '1' when io_ram_we = '1' else '0';
    
  process (CLK_14M)
  begin
    if falling_edge(CLK_14M) then
      if io_we = '1' then
        io_ram_we <= '1';
        io_ram_addr <= io_addr(17 downto 0);
        io_ram_d <= io_do;
      else
        io_ram_we <= '0';
      end if;
    end if;
  end process;
  
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
    TRACK          => open,
    TRACK_ADDR     => TRACK_ADDR,
    D1_ACTIVE      => D1_ACTIVE,
    D2_ACTIVE      => D2_ACTIVE,
    ram_write_addr => TRACK_RAM_ADDR,
    ram_di         => unsigned(sd_do),
    ram_oe         => TRACK_RAM_OE
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
      JOY0 => joy0,   
      JOY1 => joy1,  
      SWITCHES => switches,   
      BUTTONS => buttons,
      clk => CLK_12k,
      ps2_clk => ps2Clk,
      ps2_data => ps2Data
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
      red_out => VGA_R,
      green_out => VGA_G,
      blue_out => VGA_B,
      hs_out => VGA_HS,
      vs_out => VGA_VS
    );

--  SRAM_DQ(7 downto 0) <= D when ram_we = '1' else (others => 'Z');
--  SRAM_ADDR(17) <= '0';
--  SRAM_ADDR(16) <= '0';
--  SRAM_UB_N <= '1';
--  SRAM_LB_N <= '0';
--  SRAM_CE_N <= '0';
--  SRAM_WE_N <= not ram_we;
--  SRAM_OE_N <= ram_we;
--
--  LEDR(17) <= D1_ACTIVE;
--  LEDR(16) <= D2_ACTIVE;
--  LEDR(15) <= TRACK_RAM_WE;
--
--  LEDG(8 downto 1) <= (others => '0');
--  LEDR(14 downto 4) <= (others => '0');
--
--  UART_TXD <= '0';
--  FL_ADDR <= (others => '0');
--  FL_WE_N <= '1';
--  FL_RST_N <= '0';
--  FL_OE_N <= '1';
--  FL_CE_N <= '1';
--  OTG_ADDR <= (others => '0');
--  OTG_CS_N <= '1';
--  OTG_RD_N <= '1';
--  OTG_RD_N <= '1';
--  OTG_WR_N <= '1';
--  OTG_RST_N <= '1';
--  OTG_FSPEED <= '1';
--  OTG_LSPEED <= '1';
--  OTG_DACK0_N <= '1';
--  OTG_DACK1_N <= '1';
--
--  LCD_ON <= '0';
--  LCD_BLON <= '0';
--  LCD_RW <= '1';
--  LCD_EN <= '0';
--  LCD_RS <= '0';
--
--  TDO <= '0';
--
--  I2C_SCLK <= '0';
--
--  ENET_CMD <= '0';
--  ENET_CS_N <= '1';
--  ENET_WR_N <= '1';
--  ENET_RD_N <= '1';
--  ENET_RST_N <= '1';
--  ENET_CLK <= '0';
--
--  AUD_DACDAT <= '0';
--  AUD_XCK <= '0';
--
--  TD_RESET <= '0';
--
--  -- Set all bidirectional ports to tri-state
--  FL_DQ       <= (others => 'Z');
--  SRAM_DQ(15 downto 8) <= (others => 'Z');
--  OTG_DATA    <= (others => 'Z');
--  LCD_DATA    <= (others => 'Z');
--  I2C_SDAT    <= 'Z';
--  ENET_DATA   <= (others => 'Z');
--  AUD_ADCLRCK <= 'Z';
--  AUD_DACLRCK <= 'Z';
--  AUD_BCLK    <= 'Z';
--  GPIO_0      <= (others => 'Z');
--  GPIO_1      <= (others => 'Z');

end datapath;