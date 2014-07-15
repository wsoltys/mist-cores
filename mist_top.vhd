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
    
    SDRAM_nCS : out std_logic;                     -- Chip Select
    
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
    VGA_B : out unsigned(5 downto 0)                    -- Blue[5:0]

    );
  
end mist_top;

architecture datapath of mist_top is

  constant CONF_STR : string := "apple2;apl;";

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
      port(sck: in std_logic;
           ss: in std_logic;
           sdi: in std_logic;
           downloading: out std_logic;
           size: out std_logic_vector(15 downto 0);
           clk: in std_logic;
           we: in std_logic;
           a: in std_logic_vector(15 downto 0);
           din: in std_logic_vector(7 downto 0);
           dout: out std_logic_vector(7 downto 0));
  end component;

  signal CLK_14M, CLK_2M, PRE_PHASE_ZERO, CLK_12k : std_logic;
  signal IO_SELECT, DEVICE_SELECT : std_logic_vector(7 downto 0);
  signal ADDR : unsigned(15 downto 0);
  signal D, PD : unsigned(7 downto 0);

  signal ram_we : std_logic;
  signal VIDEO, HBL, VBL, LD194 : std_logic;
  signal COLOR_LINE : std_logic;
  signal COLOR_LINE_CONTROL : std_logic;
  signal GAMEPORT : std_logic_vector(7 downto 0);
  signal cpu_pc : unsigned(15 downto 0);

  signal K : unsigned(7 downto 0);
  signal read_key : std_logic;

  signal flash_clk : unsigned(22 downto 0);
  signal reset : std_logic;

  signal track : unsigned(5 downto 0);
  signal trackmsb : unsigned(3 downto 0);
  signal D1_ACTIVE, D2_ACTIVE : std_logic;
  signal track_addr : unsigned(13 downto 0);
  signal TRACK_RAM_ADDR : unsigned(13 downto 0);
  signal tra : unsigned(15 downto 0);
  signal TRACK_RAM_DI : unsigned(7 downto 0);
  signal TRACK_RAM_WE : std_logic;

  signal CS_N, MOSI, MISO, SCLK : std_logic;
  
  signal downl : std_logic := '0';
  signal size : std_logic_vector(15 downto 0) := (others=>'0');
  signal d_ram: std_logic_vector(7 downto 0);
  signal di_ram: std_logic_vector(7 downto 0);
  signal a_ram: unsigned(15 downto 0);
  signal r : unsigned(9 downto 0);
  signal g : unsigned(9 downto 0);
  signal b : unsigned(9 downto 0);
  
  signal switches   : std_logic_vector(1 downto 0);
  signal buttons    : std_logic_vector(1 downto 0);
  signal joy0       : std_logic_vector(5 downto 0);
  signal joy1       : std_logic_vector(5 downto 0);
  signal status     : std_logic_vector(7 downto 0);
  signal ps2Clk     : std_logic;
  signal ps2Data    : std_logic;

begin

  reset <= status(0) or buttons(0);
  
  SDRAM_nCS <= '1'; -- disable ram

  -- In the Apple ][, this was a 555 timer
  flash_clkgen : process (CLK_14M)
  begin
    if rising_edge(CLK_14M) then
      flash_clk <= flash_clk + 1;
    end if;     
  end process;

  pll : entity work.mist_clk 
  port map (
    inclk0 => CLOCK_27(0),
    c0     => CLK_14M,
    c1     => CLK_12k
    );

  -- Paddle buttons
  --GAMEPORT <=  "0000" & (not KEY(2 downto 0)) & "0";

  COLOR_LINE_CONTROL <= COLOR_LINE;-- and SW(17);  -- Color or B&W mode
  
  data_io_inst: data_io
    port map(SPI_SCK, SPI_SS2, SPI_DI, downl, size, CLK_14M, ram_we, std_logic_vector(a_ram), di_ram, d_ram);
    
  di_ram <= std_logic_vector(D) when ram_we = '1' else (others => 'Z');
  
  core : entity work.apple2 port map (
    CLK_14M        => CLK_14M,
    CLK_2M         => CLK_2M,
    PRE_PHASE_ZERO => PRE_PHASE_ZERO,
    FLASH_CLK      => flash_clk(22),
    reset          => reset,
    ADDR           => ADDR,
    ram_addr       => a_ram,
    D              => D,
    ram_do         => unsigned(d_ram),
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
    IO_SELECT      => IO_SELECT,
    DEVICE_SELECT  => DEVICE_SELECT,
    pcDebugOut     => cpu_pc,
    speaker        => open
    );

  vga : entity work.vga_controller port map (
    CLK_14M    => CLK_14M,
    VIDEO      => VIDEO,
    COLOR_LINE => COLOR_LINE_CONTROL,
    HBL        => HBL,
    VBL        => VBL,
    LD194      => LD194,
    VGA_CLK    => open,
    VGA_HS     => VGA_HS,
    VGA_VS     => VGA_VS,
    VGA_BLANK  => open,
    VGA_R      => r,
    VGA_G      => g,
    VGA_B      => b
    );
    
  VGA_R <= r(5 downto 0);
  VGA_G <= g(5 downto 0);
  VGA_B <= b(5 downto 0);

  keyboard : entity work.kbd_intf port map (
    PS2_Clk  => ps2Clk,
    PS2_Data => ps2Data,
    CLK_14M  => CLK_14M,
    reset    => reset,
    read_kb  => read_key,
    K => K
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

    CS_N           => CS_N,
    MOSI           => MOSI,
    MISO           => MISO,
    SCLK           => SCLK,
    
    track          => TRACK,
    
    ram_write_addr => TRACK_RAM_ADDR,
    ram_di         => TRACK_RAM_DI,
    ram_we         => TRACK_RAM_WE
    );


  trackmsb <= "00" & track(5 downto 4);
  
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
