--
-- mist_top.vhd.vhd
--
-- toplevel for the MiST board
-- https://github.com/wsoltys/mist-cores
--
-- Copyright (c) 2015 W. Soltys <wsoltys@gmail.com>
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

architecture rtl of mist_top is

  constant CONF_STR : string := "VGA;;";

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
           joystick_0 : out std_logic_vector(5 downto 0);
           joystick_1 : out std_logic_vector(5 downto 0);
           joystick_analog_0 : out std_logic_vector(15 downto 0);
           joystick_analog_1 : out std_logic_vector(15 downto 0);
           status:    out std_logic_vector(7 downto 0);
           SWITCHES : out std_logic_vector(1 downto 0);
           BUTTONS : out std_logic_vector(1 downto 0);
           sd_sdhc : in std_logic;
           ps2_clk : in std_logic;
           ps2_kbd_clk : out std_logic;
           ps2_kbd_data : out std_logic
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
  
  component osd
    port ( pclk, sck, ss, sdi, hs_in, vs_in, scanline_ena_h : in std_logic;
           red_in, blue_in, green_in : in std_logic_vector(5 downto 0);
           red_out, blue_out, green_out : out std_logic_vector(5 downto 0);
           hs_out, vs_out : out std_logic
         );
  end component osd;

  signal clk27m, pixel_clk, clk12k  : std_logic;

  signal flash_clk : unsigned(22 downto 0) := (others => '0');
  signal power_on_reset : std_logic := '1';
  signal force_reset : std_logic := '0';
  signal reset : std_logic;
  
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
  
  signal pll_locked : std_logic;

begin

  reset <= status(0) or buttons(1) or power_on_reset or force_reset;

  power_on : process(CLK)
  begin
    if rising_edge(CLK) then
      if flash_clk(22) = '1' then
        power_on_reset <= '0';
      else
        flash_clk <= flash_clk + 1;
      end if;
    end if;
  end process;
  
  clk27m <= CLOCK_27(0);

  pll : entity work.mist_pll
    port map (
      areset => buttons(1),
      inclk0 => clk27m,
      c0     => pixel_clk,
      c1     => clk12k,
      locked => pll_locked
      );
    
    
-- MiST interfaces
  
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
      sd_sdhc => '1',
      ps2_clk => CLK_12k,
      ps2_kbd_clk => ps2Clk,
      ps2_kbd_data => ps2Data
    );
    
  osd_inst : osd
    port map (
      pclk => CLK_14M,
      sdi => SPI_DI,
      sck => SPI_SCK,
      ss => SPI_SS3,
      red_in => ,
      green_in => ,
      blue_in => ,
      hs_in => not hsync,
      vs_in => not vsync,
      scanline_ena_h => '0',
      red_out => VGA_R,
      green_out => VGA_G,
      blue_out => VGA_B,
      hs_out => VGA_HS,
      vs_out => VGA_VS
    );
    
  SDRAM_nCAS  <= '1'; -- disable sdram
  AUDIO_L     <= '0';
  AUDIO_R     <= '0';

end rtl;