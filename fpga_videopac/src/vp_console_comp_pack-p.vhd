-------------------------------------------------------------------------------
--
-- FPGA Videopac
--
-- $Id: vp_console_comp_pack-p.vhd,v 1.6 2007/03/20 23:23:12 arnim Exp $
--
-- Copyright (c) 2007, Arnim Laeuger (arnim.laeuger@gmx.net)
--
-- All rights reserved
--
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

package vp_console_comp_pack is

  component vp_console
    generic (
      is_pal_g : integer := 1
    );
    port (
      -- System Interface -----------------------------------------------------
      clk_i          : in  std_logic;
      clk_cpu_en_i   : in  std_logic;
      clk_vdc_en_i   : in  std_logic;
      res_n_i        : in  std_logic;
      -- Cartridge Interface --------------------------------------------------
      cart_cs_o      : out std_logic;
      cart_cs_n_o    : out std_logic;
      cart_wr_n_o    : out std_logic;
      cart_a_o       : out std_logic_vector(11 downto 0);
      cart_d_i       : in  std_logic_vector( 7 downto 0);
      cart_d_o       : out std_logic_vector( 7 downto 0);
      cart_bs0_o     : out std_logic;
      cart_bs1_o     : out std_logic;
      cart_psen_n_o  : out std_logic;
      cart_t0_i      : in  std_logic;
      cart_t0_o      : out std_logic;
      cart_t0_dir_o  : out std_logic;
      -- Joystick Interface ---------------------------------------------------
      -- idx = 0 : left joystick
      -- idx = 1 : right joystick
      joy_up_n_i     : in  std_logic_vector( 1 downto 0);
      joy_down_n_i   : in  std_logic_vector( 1 downto 0);
      joy_left_n_i   : in  std_logic_vector( 1 downto 0);
      joy_right_n_i  : in  std_logic_vector( 1 downto 0);
      joy_action_n_i : in  std_logic_vector( 1 downto 0);
      -- Keyboard Interface ---------------------------------------------------
      keyb_dec_o     : out std_logic_vector( 6 downto 1);
      keyb_enc_i     : in  std_logic_vector(14 downto 7);
      -- Video Interface ------------------------------------------------------
      r_o            : out std_logic;
      g_o            : out std_logic;
      b_o            : out std_logic;
      l_o            : out std_logic;
      hsync_n_o      : out std_logic;
      vsync_n_o      : out std_logic;
      hbl_o          : out std_logic;
      vbl_o          : out std_logic;
      -- Sound Interface ------------------------------------------------------
      snd_o          : out std_logic;
      snd_vec_o      : out std_logic_vector(3 downto 0)
    );
  end component;

end;
