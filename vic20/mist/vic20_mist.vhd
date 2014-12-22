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
    SDRAM_CKE: out std_logic -- SDRAM Clock Enable
    
    );
end entity;

-- -----------------------------------------------------------------------

architecture rtl of vic20_mist is

-- System clocks
  signal clk16m: std_logic := '0';
  signal clk8m : std_logic := '0';
  signal pll_locked : std_logic := '0';
  
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
  
-- DataIO handling
  signal forceReset : std_logic := '0';
  signal downl : std_logic := '0';
  signal size : std_logic_vector(25 downto 0) := (others=>'0');
  signal io_dout: std_logic_vector(7 downto 0);
  signal io_addr: std_logic_vector(25 downto 0);
  signal io_we: std_logic := '0';
  signal vic_joy: std_logic_vector(4 downto 0);
  signal io_is_prg : std_logic := '1';
  
  signal vic_audio : std_logic_vector( 3 downto 0);
  signal audio_pwm : std_logic;

  -- config string used by the io controller to fill the OSD
  constant CONF_STR : string := "VIC20;PRG;O1,Program type,prg,rom;O2,Enable Scanlines,off,on;O3,Enable Scandoubler,on,off;O4,Enable 8K+ Expansion,on,off;O5,Enable 3K Expansion,off,on;";

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
  
  component data_io is
    port ( sck: in std_logic;
           ss: in std_logic;
           sdi: in std_logic;
           downloading: out std_logic;
           size: out std_logic_vector(25 downto 0);
           clk: in std_logic;
           wr: out std_logic;
           a: out std_logic_vector(25 downto 0);
           d: out std_logic_vector(7 downto 0));
  end component;

  component osd
    port (
      pclk, sck, ss, sdi, hs_in, vs_in, scanline_ena_h : in std_logic;
      red_in, blue_in, green_in : in std_logic_vector(5 downto 0);
      red_out, blue_out, green_out : out std_logic_vector(5 downto 0);
      hs_out, vs_out : out std_logic
    );
  end component osd;

begin

-- -----------------------------------------------------------------------
-- MiST
-- -----------------------------------------------------------------------

  SDRAM_nCAS <= '1'; -- disable sdram
  reset <= status(0) or buttons(1) or forceReset or not pll_locked;
  
--  io_is_prg <= status(1);
  
  
  vic20_inst : entity work.VIC20
    port map (I_PS2_CLK     => ps2Clk,
              I_PS2_DATA    => ps2Data,
              AUDIO_OUT   => vic_audio,
              VIDEO_R_OUT => VGA_R_O,
              VIDEO_G_OUT => VGA_G_O,
              VIDEO_B_OUT => VGA_B_O,
              HSYNC_OUT   => VGA_HS_O,
              VSYNC_OUT   => VGA_VS_O,
              
              
              IO_WE      => io_we,
              IO_ADDR    => io_addr(15 downto 0),
              IO_DOUT    => io_dout,
              IO_DOWNL   => downl,
              FORCERESET => ForceReset,
              IO_IS_PRG  => not status(1),
              SCANDOUBLER=> not status(3),
              EXP8KP     => not status(4),
              EXP3K      => '0', --status(5),
              RESET_B    => buttons(1),
              
              JOYSTICK    => vic_joy,
              
              RESET_L     => not reset,
              CLK_40   => clk8m
    );

  --  OSD
  osd_inst : osd
    port map (
      pclk => clk16m,
      sdi => SPI_DI,
      sck => SPI_SCK,
      ss => SPI_SS3,
      red_in => VGA_R_O & "00",
      green_in => VGA_G_O & "00",
      blue_in => VGA_B_O & "00",
      hs_in => VGA_HS_O,
      vs_in => VGA_VS_O,
      scanline_ena_h => status(2),
      red_out => VGA_R,
      green_out => VGA_G,
      blue_out => VGA_B,
      hs_out => VGA_HS,
      vs_out => VGA_VS
    );
    
  data_io_inst: data_io
    port map(SPI_SCK, SPI_SS2, SPI_DI, downl, size, clk8m, io_we, io_addr, io_dout);
  
 

-- -----------------------------------------------------------------------
-- Clocks and PLL
-- -----------------------------------------------------------------------
  pllInstance : entity work.pll27
    port map (
      inclk0 => CLOCK_27(0),
      c0 => clk16m,
      c1 => clk12k,
      locked => pll_locked
    );

  pllosd : entity work.clk_div
    generic map (
      DIVISOR => 2
    )
    port map (
      clk    => clk16m,
      reset  => '0',
      clk_en => clk8m
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
      joystick_1 => joy1,
      joystick_0 => joy0,
      joystick_analog_1 => joy_a_0,
      joystick_analog_0 => joy_a_1,
      status => status,
      ps2_clk => clk12k,
      ps2_kbd_clk => ps2Clk,
      ps2_kbd_data => ps2Data
    );
    
  -- Joystick
  --  "11111"; -- 0 up, 1 down, 2 left,  3 right, 4 fire
  vic_joy(0) <= not joy0(3);
  vic_joy(1) <= not joy0(2);
  vic_joy(2) <= not joy0(1);
  vic_joy(3) <= not joy0(0);
  vic_joy(4) <= not joy0(4);

  --
  -- Audio
  --
  u_dac : entity work.dac
    port  map(
      clk     => clk8m,
      reset   => not reset,
      dac_in  => vic_audio,
      dac_out => audio_pwm
    );
    
  AUDIO_L <= audio_pwm;
  AUDIO_R <= audio_pwm;

end architecture;
