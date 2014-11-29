--
-- MA2601.vhd
--
-- Atari VCS 2600 toplevel for the MiST board
-- https://github.com/wsoltys/tca2601
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

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.ALL;

-- -----------------------------------------------------------------------

entity vic20_mist is
    port (
    
-- Clock
      CLOCK_27 : in std_logic_vector(1 downto 0);

-- SPI
      SPI_SCK : in std_logic;
      SPI_DI : in std_logic;
      SPI_DO : out std_logic;
      SPI_SS2 : in std_logic;
      SPI_SS3 : in std_logic;
      CONF_DATA0 : in std_logic;

-- LED
      LED : out std_logic;

-- Video
      VGA_R : out std_logic_vector(5 downto 0);
      VGA_G : out std_logic_vector(5 downto 0);
      VGA_B : out std_logic_vector(5 downto 0);
      VGA_HS : out std_logic;
      VGA_VS : out std_logic;

-- Audio
      AUDIO_L : out std_logic;
      AUDIO_R : out std_logic;

-- SDRAM
      SDRAM_nCAS : out std_logic
    );
end entity;

-- -----------------------------------------------------------------------

architecture rtl of vic20_mist is

-- System clocks
  signal clk40m: std_logic := '0';
  signal osd_clk : std_logic := '0';
  
  signal reset    : std_logic;
  signal audio    : std_logic;
  signal VGA_R_O  : std_logic_vector(3 downto 0);
  signal VGA_G_O  : std_logic_vector(3 downto 0);
  signal VGA_B_O  : std_logic_vector(3 downto 0);
  signal VGA_HS_O : std_logic;
  signal VGA_VS_O : std_logic;

-- User IO
  signal switches   : std_logic_vector(1 downto 0);
  signal buttons    : std_logic_vector(1 downto 0);
  signal joy0       : std_logic_vector(7 downto 0);
  signal joy1       : std_logic_vector(7 downto 0);
  signal joy_a_0    : std_logic_vector(15 downto 0);
  signal joy_a_1    : std_logic_vector(15 downto 0);
  signal status     : std_logic_vector(7 downto 0);
  signal ascii_new  : std_logic;
  signal ascii_code : STD_LOGIC_VECTOR(6 DOWNTO 0);
  signal clk12k     : std_logic;
  signal ps2Clk     : std_logic;
  signal ps2Data    : std_logic;
  signal ps2_scancode : std_logic_vector(7 downto 0);

  -- config string used by the io controller to fill the OSD
  constant CONF_STR : string := "vic20;";

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
      switches : out std_logic_vector(1 downto 0);
      buttons : out std_logic_vector(1 downto 0);
      joystick_0 : out std_logic_vector(7 downto 0);
      joystick_1 : out std_logic_vector(7 downto 0);
      joystick_analog_0 : out std_logic_vector(15 downto 0);
      joystick_analog_1 : out std_logic_vector(15 downto 0);
      status : out std_logic_vector(7 downto 0);
      ps2_clk : in std_logic;
      ps2_kbd_clk : out std_logic;
      ps2_kbd_data : out std_logic
    );
  end component user_io;

  component osd
    port (
      pclk, sck, ss, sdi, hs_in, vs_in : in std_logic;
      red_in, blue_in, green_in : in std_logic_vector(5 downto 0);
      red_out, blue_out, green_out : out std_logic_vector(5 downto 0);
      hs_out, vs_out : out std_logic
    );
  end component osd;

begin

-- -----------------------------------------------------------------------
-- MiST
-- -----------------------------------------------------------------------

  SDRAM_nCAS <= '1'; -- disable ram
  reset <= status(0) or buttons(1);
  
  
  vic20_inst : entity work.VIC20
    port map (I_PS2_CLK     => ps2Clk,
              I_PS2_DATA    => ps2Data,
              --AUDIO_OUT   => audio,
              VIDEO_R_OUT => VGA_R_O,
              VIDEO_G_OUT => VGA_G_O,
              VIDEO_B_OUT => VGA_B_O,
              HSYNC_OUT   => VGA_HS_O,
              VSYNC_OUT   => VGA_VS_O,
              
              CART_SWITCH => '0',
              SWITCH      => "00",
              
              RESET_L     => reset,
              CLK_40   => clk40m
    );
              


  --  OSD
  osd_inst : osd
    port map (
      pclk => osd_clk,
      sdi => SPI_DI,
      sck => SPI_SCK,
      ss => SPI_SS3,
      red_in => VGA_R_O & "00",
      green_in => VGA_G_O & "00",
      blue_in => VGA_B_O & "00",
      hs_in => VGA_HS_O,
      vs_in => VGA_VS_O,
      red_out => VGA_R,
      green_out => VGA_G,
      blue_out => VGA_B,
      hs_out => VGA_HS,
      vs_out => VGA_VS
    );

  AUDIO_L <= audio;
  AUDIO_R <= audio;

 

-- -----------------------------------------------------------------------
-- Clocks and PLL
-- -----------------------------------------------------------------------
  pllInstance : entity work.pll27
    port map (
      inclk0 => CLOCK_27(0),
      c0 => clk40m,
      c1 => clk12k,
      locked => open
    );

  pllosd : entity work.clk_div
    generic map (
      DIVISOR => 4
    )
    port map (
      clk    => clk40m,
      reset  => '0',
      clk_en => osd_clk
    );

-- ------------------------------------------------------------------------
-- User IO
-- ------------------------------------------------------------------------

  user_io_inst : user_io
 	generic map (STRLEN => CONF_STR'length)
   port map (
      SPI_CLK => SPI_SCK,
      SPI_SS_IO => CONF_DATA0,
      SPI_MOSI => SPI_DI,
      SPI_MISO => SPI_DO,
		conf_str => to_slv(CONF_STR),
      switches => switches,
      buttons  => buttons,
      joystick_1 => joy0,
      joystick_0 => joy1,
      joystick_analog_1 => joy_a_0,
      joystick_analog_0 => joy_a_1,
      status => status,
      ps2_clk => clk12k,
      ps2_kbd_clk => ps2Clk,
      ps2_kbd_data => ps2Data
    );



 -- LED <= not p_color; -- yellow led is bright when color mode is selected

end architecture;
