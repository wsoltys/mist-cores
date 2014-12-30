-------------------------------------------------------------------------------
--
-- FPGA Videopac
--
-- $Id: vp_tb_pack-p.vhd,v 1.3 2007/02/05 21:49:43 arnim Exp $
--
-- Copyright (c) 2007, Arnim Laeuger (arnim.laeuger@gmx.net)
--
-- All rights reserved
--
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

package vp_tb_pack is

  signal tb_clk_s         : std_logic := '0';
  signal tb_ale_s         : std_logic := '0';
  signal tb_vdc_cs_n_s    : std_logic := '1';
  signal tb_vdc_wr_n_s    : std_logic := '1';
  signal tb_vdc_rd_n_s    : std_logic := '1';
  signal tb_db_from_cpu_s : std_logic_vector(7 downto 0) := (others => '0');
  signal tb_d_from_vdc_s  : std_logic_vector(7 downto 0) := (others => '0');

  signal tb_vbl_s         : std_logic := '0';

end;
