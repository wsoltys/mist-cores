-------------------------------------------------------------------------------
--
-- FPGA Videopac
--
-- $Id: vp_glue.vhd,v 1.7 2007/03/17 15:41:20 arnim Exp $
--
-- General glue logic found on the G7000 / Odyssey2 PCB
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

entity vp_glue is

  port (
    clk_i       : in  std_logic;
    res_n_i     : in  std_logic;
    -- Address Interface ------------------------------------------------------
    cpu_db_i    : in  std_logic_vector(7 downto 0);
    ale_i       : in  std_logic;
    rd_n_i      : in  std_logic;
    wr_n_i      : in  std_logic;
    p13_i       : in  std_logic;
    p14_i       : in  std_logic;
    p16_i       : in  std_logic;
    a_low_o     : out std_logic_vector(7 downto 0);
    cart_cs_o   : out std_logic;
    cart_cs_n_o : out std_logic;
    cart_wr_n_o : out std_logic;
    ram_cs_o    : out std_logic;
    ram_cs_n_o  : out std_logic;
    ram_wr_n_o  : out std_logic;
    vdc_cs_n_o  : out std_logic;
    vdc_rd_n_o  : out std_logic;
    vdc_wr_n_o  : out std_logic;
    -- Video Interface --------------------------------------------------------
    hbl_i       : in  std_logic;
    vbl_i       : in  std_logic;
    l_i         : in  std_logic;
    p17_i       : in  std_logic;
    blank_o     : out std_logic;
    l_o         : out std_logic
  );

end vp_glue;


architecture rtl of vp_glue is

  signal a_q : std_logic_vector(a_low_o'range);

  signal ic668d_s  : std_logic;
  signal ic670a_s,
         ic670b_s  : std_logic;

begin

  -----------------------------------------------------------------------------
  -- Process seq
  --
  -- Purpose:
  --   Implements the sequential elements.
  --
  seq: process (clk_i, res_n_i)
  begin
    if res_n_i = '0' then
      a_q <= (others => '0');
    elsif rising_edge(clk_i) then
      -- latch address while ALE is 1
      if ale_i = '1' then
        a_q <= cpu_db_i;
      end if;
    end if;
  end process seq;
  --
  -----------------------------------------------------------------------------


  -- glue logic
  ic668d_s <= ic670a_s nand rd_n_i;
  ic670a_s <= p16_i or wr_n_i;
  ic670b_s <= p16_i or rd_n_i;


  -----------------------------------------------------------------------------
  -- Output mapping
  -----------------------------------------------------------------------------
  a_low_o <= a_q;

  -- cartridge control
  cart_cs_o   <= p14_i;
  cart_cs_n_o <= ic668d_s;
  cart_wr_n_o <= ic670a_s;
  -- RAM control
  ram_cs_o    <= ic668d_s;
  ram_cs_n_o  <= p14_i or a_q(7);       -- add A7 for CS# here
  ram_wr_n_o  <= ic670a_s;
  -- VDC control
  vdc_cs_n_o  <= p13_i;
  vdc_rd_n_o  <= ic670b_s;
  vdc_wr_n_o  <= wr_n_i;

  blank_o     <= hbl_i or vbl_i;
  l_o         <= not l_i nand p17_i;

end rtl;
