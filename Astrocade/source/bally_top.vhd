--
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
  use ieee.std_logic_arith.all;
  use ieee.std_logic_unsigned.all;

library UNISIM;
  use UNISIM.Vcomponents.all;
    -- SW0 on (up) use scan converter (31K) and light led
    -- SW1 on (up) Disable flash memory ("unplug cart")
    -- SW3 and SW2 cart bank select (00 default)
    --
    -- Video out DAC's. Values here give 0.7 Volt peek video output
    -- reduce resistor values for old arcade monitors
    --
    -- Use the following resistors for Red, Green and Blue Video DACs
    --
    -- video_out(3) 510
    -- video_out(2) 1k
    -- video_out(1) 2k
    -- video_out(0) 4k
    --
    -- Audio out requires a filter, suggested values are
    -- 3k3 resistor and then a 4.7nF capacitor to ground.
    --
entity BALLY_TOP is
  port (
    O_STRATAFLASH_ADDR    : out   std_logic_vector(23 downto 0);
    B_STRATAFLASH_DATA    : inout std_logic_vector(7 downto 0);
    O_STRATAFLASH_CE_L    : out   std_logic;
    O_STRATAFLASH_OE_L    : out   std_logic;
    O_STRATAFLASH_WE_L    : out   std_logic;
    O_STRATAFLASH_BYTE    : out   std_logic;
    -- disable other onboard devices
    O_LCD_RW              : out   std_logic;
    O_LCD_E               : out   std_logic;
    O_SPI_ROM_CS          : out   std_logic;
    O_SPI_ADC_CONV        : out   std_logic;
    O_SPI_DAC_CS          : out   std_logic;
    O_PLATFORMFLASH_OE    : out   std_logic;
    --
    I_PS2_CLK             : in    std_logic;
    I_PS2_DATA            : in    std_logic;
    --
    O_VIDEO_R             : out   std_logic_vector(3 downto 0);
    O_VIDEO_G             : out   std_logic_vector(3 downto 0);
    O_VIDEO_B             : out   std_logic_vector(3 downto 0);
    O_HSYNC               : out   std_logic;
    O_VSYNC               : out   std_logic;
    --
    O_AUDIO_L             : out   std_logic;
    O_AUDIO_R             : out   std_logic;
    --
    I_SW                  : in    std_logic_vector(3 downto 0);
    O_LED                 : out   std_logic_vector(3 downto 0);
    --
    I_RESET               : in    std_logic;
    I_CLK_REF             : in    std_logic
    );
end;

architecture RTL of BALLY_TOP is

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
  -- disable unused bits of board
  O_LCD_RW <= '0';                --Always writing to display prevents display driving out.
  O_LCD_E  <= '0';                --No enable pulses to the display ensures that display contents do not change.
  O_SPI_ROM_CS <= '1';            --Disable SPI FLASH device used in SPI configuration.
  O_SPI_ADC_CONV <= '0';          --Prevent SPI based A/D converter from generating sample data.
  O_SPI_DAC_CS <= '1';            --Disable SPI based D/A converter interface.
  O_PLATFORMFLASH_OE <= '0';      --Disable (reset) Platform FLASH device used in master serial configuration.
  --
  I_RESET_L <= not I_RESET;
  --
  u_clocks : entity work.BALLY_CLOCKS
    port map (
       I_CLK_REF  => I_CLK_REF,
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

      I_PS2_CLK         => I_PS2_CLK,
      I_PS2_DATA        => I_PS2_DATA,

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
      CLK               => clk_14
    );
  --
  p_video_ouput : process
  begin
    wait until rising_edge(clk_14);
    -- switch is on (up) use scan converter and light led
    sw_reg <= I_SW;

    if (sw_reg(0) = '1') then
      O_LED(0) <= '1';
      O_VIDEO_R <= video_r_x2;
      O_VIDEO_G <= video_g_x2;
      O_VIDEO_B <= video_b_x2;
      O_HSYNC   <= hSync_X2;
      O_VSYNC   <= vSync_X2;
    else
      O_LED(0) <= '0';
      O_VIDEO_R <= video_r;
      O_VIDEO_G <= video_g;
      O_VIDEO_B <= video_b;
      O_HSYNC   <= hSync;
      O_VSYNC   <= vSync;
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

  O_AUDIO_L <= audio_pwm;
  O_AUDIO_R <= audio_pwm;
  --
  -- cart slot
  --
  p_flash : process
  begin
    wait until rising_edge(clk_14);
    O_LED(3 downto 1) <= sw_reg(3 downto 1);

    O_STRATAFLASH_CE_L <= '1';
    if (sw_reg(1) = '0') then -- unplug card
      O_STRATAFLASH_CE_L <= cas_cs_l;
    end if;
    O_STRATAFLASH_OE_L <= '0';
    O_STRATAFLASH_WE_L <= '1';
    O_STRATAFLASH_BYTE <= '0';

    O_STRATAFLASH_ADDR(23 downto 15) <= (others => '0');

    O_STRATAFLASH_ADDR(14 downto 13) <= sw_reg(3 downto 2);
    O_STRATAFLASH_ADDR(12 downto  0) <= cas_addr(12 downto 0);
    B_STRATAFLASH_DATA <= (others => 'Z');
    -- should really sample and latch this at the correct point, but it seems to work
    cas_data <= B_STRATAFLASH_DATA;
  end process;

end RTL;
