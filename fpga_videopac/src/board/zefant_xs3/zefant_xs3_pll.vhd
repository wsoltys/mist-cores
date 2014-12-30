-------------------------------------------------------------------------------
--
-- FPGA Videopac
--
-- $Id: zefant_xs3_pll.vhd,v 1.6 2007/02/05 21:53:30 arnim Exp $
--
-- Clock generator for Zefant-XS3 board.
--
-------------------------------------------------------------------------------
--
-- Copyright (c) 2007, Arnim Laeuger (arnim.laeuger@gmx.net)
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
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
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
-- Please report bugs to the author, but before you do so, please
-- make sure that this is not a derivative work and that
-- you have the latest version of this file.
--
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity zefant_xs3_pll is

  port (
    clkin_i    : in  std_logic;
    locked_o   : out std_logic;
    clk_43m_o  : out std_logic;
    clk_21m5_o : out std_logic
  );

end zefant_xs3_pll;


library unisim;
use unisim.vcomponents.all;

architecture struct of zefant_xs3_pll is

  signal clkin_ibuf_s  : std_logic;
  signal clkfx_43m_s   : std_logic;
  signal locked_43m_s  : std_logic;
  signal clk_43m_s     : std_logic;

  signal clk_21m5_q    : std_logic;

  signal gnd_s        : std_logic;

begin

  gnd_s <= '0';

  clkin_ibufg_b : IBUFG
    port map (
      I => clkin_i,
      O => clkin_ibuf_s
    );

  -----------------------------------------------------------------------------
  -- Target frequency is 21477270 Hz (2x3 3.579545 MHz) -> 21.5 MHz
  -- The VGA scan doubler requires twice the frequency -> 43 MHz
  -- Derive this from 66 * 13/20 = 43 MHz
  -----------------------------------------------------------------------------
  dcm_43m_b : dcm
    generic map (
      CLK_FEEDBACK          => "NONE",
      CLKDV_DIVIDE          => 2.000000,
      CLKFX_DIVIDE          => 20,
      CLKFX_MULTIPLY        => 13,
      CLKIN_DIVIDE_BY_2     => FALSE,
      CLKIN_PERIOD          => 15.151515,
      CLKOUT_PHASE_SHIFT    => "NONE",
      DESKEW_ADJUST         => "SYSTEM_SYNCHRONOUS",
      DFS_FREQUENCY_MODE    => "LOW",
      DLL_FREQUENCY_MODE    => "LOW",
      DUTY_CYCLE_CORRECTION => TRUE,
      FACTORY_JF            => x"C080",
      PHASE_SHIFT           => 0,
      STARTUP_WAIT          => FALSE
    )
    port map (
      CLKIN          => clkin_ibuf_s,
      CLKFB          => gnd_s,
      RST            => gnd_s,
      PSEN           => gnd_s,
      PSINCDEC       => gnd_s,
      PSCLK          => gnd_s,
      CLK0           => open,
      CLK90          => open,
      CLK180         => open,
      CLK270         => open,
      CLK2X          => open,
      CLK2X180       => open,
      CLKDV          => open,
      CLKFX          => clkfx_43m_s,
      CLKFX180       => open,
      STATUS         => open,
      LOCKED         => locked_43m_s,
      PSDONE         => open
    );
  --
  clk_43m_bufg_b : BUFG
    port map (
      I => clkfx_43m_s,
      O => clk_43m_s
    );
  clk_43m_o <= clk_43m_s;

  clk_div: process (clk_43m_s, locked_43m_s)
  begin
    if locked_43m_s = '0' then
      clk_21m5_q <= '0';
    elsif rising_edge(clk_43m_s) then
      clk_21m5_q <= not clk_21m5_q;
    end if;
  end process clk_div;

  clk_21m5_bufg_b : BUFGCE
    port map (
      I  => clkfx_43m_s,
      CE => clk_21m5_q,
      O  => clk_21m5_o
    );
  --
  locked_o <= locked_43m_s;

end struct;
