-------------------------------------------------------------------------------
--
-- FPGA Videopac
--
-- $Id: tech_comp_pack-p.vhd,v 1.2 2007/02/05 21:50:09 arnim Exp $
--
-- Copyright (c) 2007, Arnim Laeuger (arnim.laeuger@gmx.net)
--
-- All rights reserved
--
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

package tech_comp_pack is

  component vp_por
    generic (
      delay_g     : integer := 4;
      cnt_width_g : integer := 2
    );
    port (
      clk_i   : in  std_logic;
      por_n_o : out std_logic
    );
  end component;

  component dpram
    generic (
      addr_width_g : integer := 8;
      data_width_g : integer := 8
    );
    port (
      clk_a_i  : in  std_logic;
      we_i     : in  std_logic;
      addr_a_i : in  std_logic_vector(addr_width_g-1 downto 0);
      data_a_i : in  std_logic_vector(data_width_g-1 downto 0);
      data_a_o : out std_logic_vector(data_width_g-1 downto 0);
      clk_b_i  : in  std_logic;
      addr_b_i : in  std_logic_vector(addr_width_g-1 downto 0);
      data_b_o : out std_logic_vector(data_width_g-1 downto 0)
    );
  end component;

end tech_comp_pack;
