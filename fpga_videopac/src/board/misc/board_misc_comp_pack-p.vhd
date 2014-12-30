-------------------------------------------------------------------------------
--
-- $Id: board_misc_comp_pack-p.vhd,v 1.6 2007/03/26 22:22:42 arnim Exp $
--
-- Copyright (c) 2006, Arnim Laeuger (arnim.laeuger@gmx.net)
--
-- All rights reserved
--
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package board_misc_comp_pack is

  component dac
    generic (
      msbi_g : integer := 7
    );
    port (
      clk_i   : in  std_logic;
      res_n_i : in  std_logic;
      dac_i   : in  std_logic_vector(msbi_g downto 0);
      dac_o   : out std_logic
    );
  end component;

  component dblscan
    port (
      RGB_R_IN      : in  std_logic;
      RGB_G_IN      : in  std_logic;
      RGB_B_IN      : in  std_logic;
      RGB_L_IN      : in  std_logic;

      HSYNC_IN      : in  std_logic;
      VSYNC_IN      : in  std_logic;

      VGA_R_OUT     : out std_logic;
      VGA_G_OUT     : out std_logic;
      VGA_B_OUT     : out std_logic;
      VGA_L_OUT     : out std_logic;

      HSYNC_OUT     : out std_logic;
      VSYNC_OUT     : out std_logic;
      BLANK_OUT     : out std_logic;
      --  NOTE CLOCKS MUST BE PHASE LOCKED !!
      CLK_RGB       : in  std_logic; -- input pixel clock
      CLK_EN_RGB    : in  std_logic;
      CLK_VGA       : in  std_logic; -- output clock
      CLK_EN_VGA    : in  std_logic;
      RESET_N_I     : in  std_logic
    );
  end component;

  component pcm_sound
    port (
      clk_i              : in  std_logic;
      reset_n_i          : in  std_logic;
      pcm_left_i         : in  signed(8 downto 0);
      pcm_right_i        : in  signed(8 downto 0);
      bit_clk_pad_i      : in  std_logic;
      sync_pad_o         : out std_logic;
      sdata_pad_o        : out std_logic;
      sdata_pad_i        : in  std_logic;
      ac97_reset_pad_n_o : out std_logic;
      led_o              : out std_logic_vector(5 downto 0);
      dpy0_a_o           : out std_logic;
      dpy0_b_o           : out std_logic;
      dpy0_c_o           : out std_logic;
      dpy0_d_o           : out std_logic;
      dpy0_e_o           : out std_logic;
      dpy0_f_o           : out std_logic;
      dpy0_g_o           : out std_logic;
      dpy1_a_o           : out std_logic;
      dpy1_b_o           : out std_logic;
      dpy1_c_o           : out std_logic;
      dpy1_d_o           : out std_logic;
      dpy1_e_o           : out std_logic;
      dpy1_f_o           : out std_logic;
      dpy1_g_o           : out std_logic
    );
  end component;

  component vp_keymap
    port (
      -- System Interface -----------------------------------------------------
      clk_i           : in  std_logic;
      res_n_i         : in  std_logic;
      -- Videopac Interface ---------------------------------------------------
      keyb_dec_i      : in  std_logic_vector( 6 downto 1);
      keyb_enc_o      : out std_logic_vector(14 downto 7);
      -- Keyboard Interface ---------------------------------------------------
      rx_data_ready_i : in  std_logic;    -- new RX ASCII data availble
      rx_ascii_i      : in  std_logic_vector( 7 downto 0); -- RX ASCII data
      rx_released_i   : in  std_logic;    -- last key was released
      rx_read_o       : out std_logic     -- RX ASCII data acknowledge
    );
  end component;

  component mc_ctrl
    port (
      clk_i       : in  std_logic;
      reset_n_i   : in  std_logic;
      cart_a_i    : in  std_logic_vector(11 downto 0);
      cart_d_i    : in  std_logic_vector( 7 downto 0);
      cart_cs_i   : in  std_logic;
      cart_cs_n_i : in  std_logic;
      cart_wr_n_i : in  std_logic;
      cart_bs0_i  : in  std_logic;
      cart_bs1_i  : in  std_logic;
      extmem_a_o  : out std_logic_vector(18 downto 0)
    );
  end component;

end board_misc_comp_pack;
