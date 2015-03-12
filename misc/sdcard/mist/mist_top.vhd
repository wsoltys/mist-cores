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

  signal clk100m, clk25m, clk12m, clk12k  : std_logic;

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
  
  signal VGA_R_O  : std_logic_vector(5 downto 0);
  signal VGA_G_O  : std_logic_vector(5 downto 0);
  signal VGA_B_O  : std_logic_vector(5 downto 0);
  signal VGA_HS_O : std_logic;
  signal VGA_VS_O : std_logic;
  
  -- signals to connect sd card emulation with io controller
  signal sd_lba:  std_logic_vector(31 downto 0);
  signal sd_rd:   std_logic;
  signal sd_wr:   std_logic;
  signal sd_ack:  std_logic;
  signal sd_conf: std_logic;
  signal sd_sdhc: std_logic;
  signal allow_sdhc: std_logic;
  
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

begin

  reset <= status(0) or buttons(1) or not pll_locked;


  pll : entity work.mist_pll
    port map (
      inclk0 => CLOCK_27(0),
      c0     => clk100m,
      c1     => clk12k,
      locked => pll_locked
      );
      
  divvga : entity work.clk_div
    generic map (
      DIVISOR => 4
    )
    port map (
      clk    => clk100m,
      reset  => '0',
      clk_en => clk25m
    );
    
  divosd : entity work.clk_div
    generic map (
      DIVISOR => 2
    )
    port map (
      clk    => clk25m,
      reset  => '0',
      clk_en => clk12m
    );
      
      
-- VGA Output

  vga : entity work.VGASYNC
    port map (
      CLK   => clk25m,
      HSYNC => VGA_HS_O,
      VSYNC => VGA_VS_O,
      R     => VGA_R_O,
      G     => VGA_G_O,
      B     => VGA_B_O
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
      
      joystick_0 => joy0,   
      joystick_1 => joy1,
      joystick_analog_0 => joy_an0,
      joystick_analog_1 => joy_an1,
      SWITCHES => switches,   
      BUTTONS => buttons,
      ps2_clk => clk12k,
      ps2_kbd_clk => ps2Clk,
      ps2_kbd_data => ps2Data
    );
    
  osd_inst : osd
    port map (
      pclk => clk12m,
      sdi => SPI_DI,
      sck => SPI_SCK,
      ss => SPI_SS3,
      red_in => VGA_R_O,
      green_in => VGA_G_O,
      blue_in => VGA_B_O,
      hs_in => not VGA_HS_O,
      vs_in => not VGA_VS_O,
      scanline_ena_h => '0',
      red_out => VGA_R,
      green_out => VGA_G,
      blue_out => VGA_B,
      hs_out => VGA_HS,
      vs_out => VGA_VS
    );
    
  sd_card_d: sd_card
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
   
      allow_sdhc  => allow_sdhc,
      
      -- connection to host
      sd_cs  => sd_cs,
      sd_sck => sd_sck,
      sd_sdi => sd_sdi,
      sd_sdo => sd_sdo		
    );
    
  sdfat : entity work.sdfat32
    port map(
      clk100  => clk100m,
      reset   => reset,
      
      sd_cs   => sd_cs,
      sclk_o  => sd_sck,
      miso_i  => sd_sdo,
      mosi_o  => sd_sdi,
      
      filename_i => x"414D53444F532020524F4D",
      file_ram_a => x"004000"
    );
    
    
  SDRAM_nCAS  <= '1'; -- disable sdram
  AUDIO_L     <= '0';
  AUDIO_R     <= '0';
  allow_sdhc  <= '1';

end rtl;