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
use std.textio.ALL;
library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_arith.all;
  use ieee.std_logic_unsigned.all;


entity BALLY_TOP_TB is
end;

architecture Sim of BALLY_TOP_TB is

  signal clk_50 : std_logic;
  signal reset_l : std_logic;
  signal reset_h : std_logic;

  constant CLKPERIOD_50 : time := 20 ns;

begin
  u0 : entity work.BALLY_TOP
    port map (
      O_STRATAFLASH_ADDR    => open,
      B_STRATAFLASH_DATA    => "HHHHHHHH",
      O_STRATAFLASH_CE_L    => open,
      O_STRATAFLASH_OE_L    => open,
      O_STRATAFLASH_WE_L    => open,
      O_STRATAFLASH_BYTE    => open,
      -- disable other onboard devices
      O_LCD_RW              => open,
      O_LCD_E               => open,
      O_SPI_ROM_CS          => open,
      O_SPI_ADC_CONV        => open,
      O_SPI_DAC_CS          => open,
      O_PLATFORMFLASH_OE    => open,
      --
      I_PS2_CLK             => '1',
      I_PS2_DATA            => '1',
      --
      O_VIDEO_R             => open,
      O_VIDEO_G             => open,
      O_VIDEO_B             => open,
      O_HSYNC               => open,
      O_VSYNC               => open,
      --
      O_AUDIO_L             => open,
      O_AUDIO_R             => open,
      --
      I_SW0                 => "0001",
      O_LED0                => open,
      --
      I_RESET               => reset_h,
      I_CLK_REF             => clk_50
      );

  p_clk_50  : process
  begin
    CLK_50 <= '0';
    wait for CLKPERIOD_50 / 2;
    CLK_50 <= '1';
    wait for CLKPERIOD_50 - (CLKPERIOD_50 / 2);
  end process;

  p_rst : process
  begin
    reset_l <= '0';
    reset_h <= '1';
    wait for 100 ns;
    reset_l <= '1';
    reset_h <= '0';
    wait;
  end process;

end Sim;

