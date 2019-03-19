--
-- bally_top.vhd
--
-- Bally Astrocade toplevel for the MiST board
-- This file is a *derivative* work of the source cited below and from pacedev.net
-- The original source can be downloaded from <http://www.fpgaarcade.com>
-- https://github.com/wsoltys/mist-cores
--
-------------------------------------------------------------------------------
-- A simulation model of Bally Astrocade hardware
-- Copyright (c) MikeJ - Nov 2004
--
-- All rights reserved
--
-- Redistribution and use in source and synthezised forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- Redistributions of source code must retain the above copyright notice,
-- this list of conditions and the following disclaimer.
--
-- Redistributions in synthesized form must reproduce the above copyright
-- notice, this list of conditions and the following disclaimer in the
-- documentation and/or other materials provided with the distribution.
--
-- Neither the name of the author nor the names of other contributors may
-- be used to endorse or promote products derived from this software without
-- specific prior written permission.
--
-- THIS CODE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
-- THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
-- PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.
--
-- You are responsible for any legal issues arising from your use of this code.
--
-- The latest version of this file can be found at: www.fpgaarcade.com
--
-- Email support@fpgaarcade.com
--
-- Revision list
--
-- version 004 spartan3e hires release
-- version 003 spartan3e release
-- version 001 initial release
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity mist_ba is

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
  
end mist_ba;

architecture rtl of mist_ba is

  constant CONF_STR : string := "ASTROCADE;BIN;O1,Enable Scanlines,no,yes;T0,Reset;";

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

  component user_io generic(STRLEN : integer := 0);
  port
  (
        clk_sys : in std_logic;
        SPI_CLK, SPI_SS_IO, SPI_MOSI :in std_logic;
        SPI_MISO : out std_logic;
        conf_str : in std_logic_vector(8*STRLEN-1 downto 0);
        joystick_0 : out std_logic_vector(31 downto 0);
        joystick_1 : out std_logic_vector(31 downto 0);
        joystick_2 : out std_logic_vector(31 downto 0);
        joystick_3 : out std_logic_vector(31 downto 0);
        joystick_4 : out std_logic_vector(31 downto 0);
        joystick_analog_0 : out std_logic_vector(15 downto 0);
        joystick_analog_1 : out std_logic_vector(15 downto 0);
        status: out std_logic_vector(31 downto 0);
        switches : out std_logic_vector(1 downto 0);
        buttons : out std_logic_vector(1 downto 0);
        scandoubler_disable : out std_logic;
        ypbpr : out std_logic;
        ps2_kbd_clk       : out std_logic;
        ps2_kbd_data      : out std_logic
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
         a: in std_logic_vector(12 downto 0);
         din: in std_logic_vector(7 downto 0);
         dout: out std_logic_vector(7 downto 0));
  end component;

  component osd
  generic ( OSD_COLOR : integer := 1 );  -- blue
  port (
        clk_sys     : in std_logic;
        ce_pix      : in std_logic;
        R_in        : in std_logic_vector(5 downto 0);
        G_in        : in std_logic_vector(5 downto 0);
        B_in        : in std_logic_vector(5 downto 0);
        HSync       : in std_logic;
        VSync       : in std_logic;

        R_out       : out std_logic_vector(5 downto 0);
        G_out       : out std_logic_vector(5 downto 0);
        B_out       : out std_logic_vector(5 downto 0);

        SPI_SCK     : in std_logic;
        SPI_SS3     : in std_logic;
        SPI_DI      : in std_logic
  );
  end component osd;

  COMPONENT rgb2ypbpr
  PORT (
        red     :        IN std_logic_vector(5 DOWNTO 0);
        green   :        IN std_logic_vector(5 DOWNTO 0);
        blue    :        IN std_logic_vector(5 DOWNTO 0);
        y       :        OUT std_logic_vector(5 DOWNTO 0);
        pb      :        OUT std_logic_vector(5 DOWNTO 0);
        pr      :        OUT std_logic_vector(5 DOWNTO 0)
        );
  END COMPONENT;  

  signal force_reset : std_logic := '0';
  signal reset_n_s : std_logic;
  
  signal switches   : std_logic_vector(1 downto 0);
  signal buttons    : std_logic_vector(1 downto 0);
  signal joy        : std_logic_vector(5 downto 0);
  signal joy0       : std_logic_vector(31 downto 0);
  signal joy1       : std_logic_vector(31 downto 0);
  signal joy_an0    : std_logic_vector(15 downto 0);
  signal joy_an1    : std_logic_vector(15 downto 0);
  signal joy_an     : std_logic_vector(15 downto 0);
  signal status     : std_logic_vector(31 downto 0);
  signal scandoubler_disable : std_logic;
  signal ypbpr      : std_logic;
  signal ps2Clk     : std_logic;
  signal ps2Data    : std_logic;
  
  signal pll_locked : std_logic;
  
  signal VGA_R_O  : std_logic_vector(5 downto 0);
  signal VGA_G_O  : std_logic_vector(5 downto 0);
  signal VGA_B_O  : std_logic_vector(5 downto 0);
  signal VGA_HS_O : std_logic;
  signal VGA_VS_O : std_logic;
  signal osd_ce   : std_logic;
  signal hsync_out : std_logic;
  signal vsync_out : std_logic;
  signal csync_out : std_logic;
  signal osd_red_o      : std_logic_vector(5 downto 0);
  signal osd_green_o    : std_logic_vector(5 downto 0);
  signal osd_blue_o     : std_logic_vector(5 downto 0);
  signal vga_y_o        : std_logic_vector(5 downto 0);
  signal vga_pb_o       : std_logic_vector(5 downto 0);
  signal vga_pr_o       : std_logic_vector(5 downto 0);
  
  signal downl          : std_logic := '0';
  signal size           : std_logic_vector(15 downto 0) := (others=>'0');
  signal cart_a         : std_logic_vector(12 downto 0);
  signal cart_d         : std_logic_vector(7 downto 0);
  
  ---
  signal I_RESET_L        : std_logic;
  signal reset            : std_logic;
  signal reset_l          : std_logic;
  signal sw_reg           : std_logic_vector(3 downto 0);
  --
  signal ena_x2           : std_logic;
  signal ena              : std_logic;
  signal clk_14           : std_logic;
  signal clk_ref          : std_logic;
  --
  signal switch_col       : std_logic_vector(7 downto 0);
  signal switch_row       : std_logic_vector(7 downto 0);
  signal ps2_1mhz_ena     : std_logic;
  signal ps2_1mhz_cnt     : std_logic_vector(5 downto 0);
  --
  signal video_r          : std_logic_vector(3 downto 0);
  signal video_g          : std_logic_vector(3 downto 0);
  signal video_b          : std_logic_vector(3 downto 0);
  signal hsync            : std_logic;
  signal vsync            : std_logic;
  signal fpsync           : std_logic;
  --
  signal video_r_x2       : std_logic_vector(3 downto 0);
  signal video_g_x2       : std_logic_vector(3 downto 0);
  signal video_b_x2       : std_logic_vector(3 downto 0);
  signal hsync_x2         : std_logic;
  signal vsync_x2         : std_logic;
  --
  signal audio            : std_logic_vector(7 downto 0);
  signal audio_pwm        : std_logic;
  
  signal exp_addr         : std_logic_vector(15 downto 0);
  signal exp_data_out     : std_logic_vector(7 downto 0);
  signal exp_data_in      : std_logic_vector(7 downto 0);
  signal exp_oe_l         : std_logic;
  
  signal exp_m1_l         : std_logic;
  signal exp_mreq_l       : std_logic;
  signal exp_iorq_l       : std_logic;
  signal exp_wr_l         : std_logic;
  signal exp_rd_l         : std_logic;
  --
  signal check_cart_msb   : std_logic_vector(3 downto 0);
  signal check_cart_lsb   : std_logic_vector(7 downto 4);
  --
  signal cas_addr         : std_logic_vector(12 downto 0);
  signal cas_data         : std_logic_vector( 7 downto 0);
  signal cas_cs_l         : std_logic;

begin

  I_RESET_L <= not(status(0) or buttons(1) or force_reset);

  u_clocks : entity work.BALLY_CLOCKS
    port map (
       I_CLK_REF  => CLOCK_27(0),
       I_RESET_L  => I_RESET_L,
       --
       O_CLK_REF  => clk_ref,
       --
       O_ENA_X2   => ena_x2,
       O_ENA      => ena,
       O_CLK      => clk_14, -- ~14 MHz
       O_RESET    => reset
     );

  p_ena1mhz : process
  begin
    wait until rising_edge(clk_14);
    -- divide by 14
    ps2_1mhz_ena <= '0';
    if (ps2_1mhz_cnt = "001101") then
      ps2_1mhz_cnt <= "000000";
      ps2_1mhz_ena <= '1';
    else
      ps2_1mhz_cnt <= ps2_1mhz_cnt + '1';
    end if;
  end process;

  reset_l <= not reset;

  u_bally : entity work.BALLY
    port map (
      O_AUDIO        => audio,
      --
      O_VIDEO_R      => video_r,
      O_VIDEO_G      => video_g,
      O_VIDEO_B      => video_b,

      O_HSYNC        => hsync,
      O_VSYNC        => vsync,
      O_COMP_SYNC_L  => open,
      O_FPSYNC       => fpsync,
      --
      -- cart slot
      O_CAS_ADDR     => cas_addr,
      O_CAS_DATA     => open,
      I_CAS_DATA     => cas_data,
      O_CAS_CS_L     => cas_cs_l,

      -- exp slot (subset for now)
      O_EXP_ADDR     => exp_addr,
      O_EXP_DATA     => exp_data_out,
      I_EXP_DATA     => exp_data_in,
      I_EXP_OE_L     => exp_oe_l,

      O_EXP_M1_L     => exp_m1_l,
      O_EXP_MREQ_L   => exp_mreq_l,
      O_EXP_IORQ_L   => exp_iorq_l,
      O_EXP_WR_L     => exp_wr_l,
      O_EXP_RD_L     => exp_rd_l,
      --
      O_SWITCH_COL   => switch_col,
      I_SWITCH_ROW   => switch_row,
      I_RESET_L      => reset_l,
      ENA            => ena,
      CLK            => clk_14
      );

  u_ps2 : entity work.BALLY_PS2_IF
    port map (

      I_PS2_CLK         => ps2Clk,
      I_PS2_DATA        => ps2Data,
--      I_JOY1            => joy1,
--      I_JOY2            => joy0,

      I_COL             => switch_col,
      O_ROW             => switch_row,

      I_RESET_L         => reset_l,
      I_1MHZ_ENA        => ps2_1mhz_ena,
      CLK               => clk_14
      );

  --u_check_cart : entity work.BALLY_CHECK_CART
    --port map (
      --I_EXP_ADDR         => exp_addr,
      --I_EXP_DATA         => exp_data_out,
      --O_EXP_DATA         => exp_data_in,
      --O_EXP_OE_L         => exp_oe_l,

      --I_EXP_M1_L         => exp_m1_l,
      --I_EXP_MREQ_L       => exp_mreq_l,
      --I_EXP_IORQ_L       => exp_iorq_l,
      --I_EXP_WR_L         => exp_wr_l,
      --I_EXP_RD_L         => exp_rd_l,
      ----
      --O_CHAR_MSB         => check_cart_msb,
      --O_CHAR_LSB         => check_cart_lsb,
      ----
      --I_RESET_L          => reset_l,
      --ENA                => ena,
      --CLK                => clk_14
      --);

  -- if no expansion cart
  exp_data_in <= x"ff";
  exp_oe_l <= '1';
  --
  -- scan doubler
  --
  u_dblscan : entity work.BALLY_DBLSCAN
    port map (
      I_R               => video_r,
      I_G               => video_g,
      I_B               => video_b,
      I_HSYNC           => hsync,
      I_VSYNC           => vsync,
      --
      I_FPSYNC          => fpsync,
      --
      O_R               => video_r_x2,
      O_G               => video_g_x2,
      O_B               => video_b_x2,
      O_HSYNC           => hsync_x2,
      O_VSYNC           => vsync_x2,
      --
      I_RESET           => reset,
      ENA_X2            => ena_x2,
      ENA               => ena,
      CLK               => clk_14,
      ENA_SCANLINES     => status(1)
    );
  --
  p_video_ouput : process
  begin
    wait until rising_edge(clk_14);

    if (scandoubler_disable = '0') then
      VGA_R_O <= video_r_x2 & "00";
      VGA_G_O <= video_g_x2 & "00";
      VGA_B_O <= video_b_x2 & "00";
      VGA_HS_O   <= hSync_X2;
      VGA_VS_O   <= vSync_X2;
    else
      VGA_R_O <= video_r & "00";
      VGA_G_O <= video_g & "00";
      VGA_B_O <= video_b & "00";
      VGA_HS_O   <= hSync;
      VGA_VS_O   <= vSync;
    end if;
  end process;

  --
  -- Audio
  --
  u_dac : entity work.dac
    generic map(
      msbi_g => 7
    )
    port  map(
      clk_i   => clk_ref,
      res_n_i => reset_l,
      dac_i   => audio,
      dac_o   => audio_pwm
    );

  AUDIO_L <= audio_pwm;
  AUDIO_R <= audio_pwm;

  --
  -- a minimig vga->scart cable expects a composite sync signal on the VGA_HS output 
  -- and VCC on VGA_VS (to switch into rgb mode)
  csync_out <= not (VGA_HS_O xor VGA_VS_O);

  VGA_HS <= csync_out when scandoubler_disable='1' or ypbpr = '1' else VGA_HS_O;
  VGA_VS <= '1' when scandoubler_disable='1' or ypbpr='1' else VGA_VS_O;
  VGA_R <= vga_pr_o when ypbpr='1' else osd_red_o;
  VGA_G <= vga_y_o  when ypbpr='1' else osd_green_o;
  VGA_B <= vga_pb_o when ypbpr='1' else osd_blue_o;

-----------------------------------------------------------------------------
-- MiST interfaces
  
  user_io_d : user_io
    generic map (STRLEN => CONF_STR'length)
    
    port map (
      clk_sys => clk_14,
      SPI_CLK => SPI_SCK,
      SPI_SS_IO => CONF_DATA0,    
      SPI_MISO => SPI_DO,    
      SPI_MOSI => SPI_DI,       
      conf_str => to_slv(CONF_STR),
      status => status,
--      joystick_0 => joy0,   
--      joystick_1 => joy1,
      joystick_analog_0 => joy_an0,
      joystick_analog_1 => joy_an1,
      scandoubler_disable => scandoubler_disable,
      ypbpr => ypbpr,
      SWITCHES => switches,   
      BUTTONS => buttons,
      ps2_kbd_clk => ps2Clk,
      ps2_kbd_data => ps2Data
    );

  osd_ce <= '1' when scandoubler_disable='0' else ena;

  osd_inst : osd
    port map (
      clk_sys => clk_14,
	  ce_pix => osd_ce,
      SPI_DI => SPI_DI,
      SPI_SCK => SPI_SCK,
      SPI_SS3 => SPI_SS3,
      R_in => VGA_R_O,
      G_in => VGA_G_O,
      B_in => VGA_B_O,
      HSync => not VGA_HS_O,
      VSync => not VGA_VS_O,
      R_out => osd_red_o,
      G_out => osd_green_o,
      B_out => osd_blue_o
    );

  rgb2component: component rgb2ypbpr
    port map (
      red => osd_red_o,
      green => osd_green_o,
      blue => osd_blue_o,
      y => vga_y_o,
      pb => vga_pb_o,
      pr => vga_pr_o
    );

  data_io_inst: data_io
    port map(SPI_SCK, SPI_SS2, SPI_DI, downl, size, clk_14, '0', cart_a, (others=>'0'), cart_d);
    
  process(downl)
  begin
    if(downl = '0') then
      cart_a <= cas_addr;
      cas_data <= cart_d;
      force_reset <= '0';
    else
      cart_a <= cas_addr;
      cas_data <= x"FF";
      force_reset <= '1';
    end if;
  end process;
    
  SDRAM_nCAS  <= '1'; -- disable sdram

end rtl;