-------------------------------------------------------------------------------
--
-- FPGA Videopac
--
-- $Id: zefant_xs3_vp.vhd,v 1.18 2007/04/07 10:49:05 arnim Exp $
-- $Name: videopac_rel_1_0 $
--
-- Toplevel of the Spartan3 port for Simple Solutions' Zefant-XS3 board.
--   http://zefant.de/
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

library IEEE;
use ieee.std_logic_1164.all;

entity zefant_xs3_vp is
  port (
    -- Zefant-DDR FPGA Module Peripherals -----------------------------------
    -- Clock oscillator
    osc1                     : in    std_logic;
    
    -- Flash Memory
    fl_a                     : out   std_logic_vector(24 downto 0);
    fl_d                     : inout std_logic_vector(15 downto 0);
    fl_ce_n                  : out   std_logic;
    fl_oe_n                  : out   std_logic;
    fl_we_n                  : out   std_logic;
    fl_byte_n                : out   std_logic;
    fl_rp_n                  : out   std_logic;
    fl_sts                   : in    std_logic;

    -- FPGA dedicated/dual purpose pins 
    fpga_cs_b                : inout std_logic;
    fpga_dout_busy           : inout std_logic;
    fpga_init_b              : inout std_logic;
    fpga_rdwr_b              : inout std_logic;

    fpga_cpld_io             : inout std_logic_vector(7 downto 0);

    -- SRAM 0
    sr0_a                    : out   std_logic_vector(17 downto 0);
    sr0_d                    : inout std_logic_vector(15 downto 0);
    sr0_ce_n                 : out   std_logic;
    sr0_lb_n                 : out   std_logic;
    sr0_oe_n                 : out   std_logic;
    sr0_ub_n                 : out   std_logic;
    sr0_we_n                 : out   std_logic;
    -- SRAM 1
    sr1_a                    : out   std_logic_vector(17 downto 0);
    sr1_d                    : inout std_logic_vector(15 downto 0);
    sr1_ce_n                 : out   std_logic;
    sr1_lb_n                 : out   std_logic;
    sr1_oe_n                 : out   std_logic;
    sr1_ub_n                 : out   std_logic;
    sr1_we_n                 : out   std_logic;

    -- Zefant-XS3 Baseboard Peripherals -----------------------------------
    -- EEPROM
    ee_cs_n                  : out   std_logic;
    ee_sck                   : out   std_logic;
    ee_si                    : out   std_logic;
    ee_so                    : in    std_logic;

    -- User Interface
    button                   : in    std_logic_vector(5 downto 0);
    led                      : out   std_logic_vector(5 downto 0);

    -- Audio Codec
    aud_sdata_in             : in    std_logic;
    aud_sdata_out            : out   std_logic;
    aud_bit_clk              : in    std_logic;
    aud_cin                  : out   std_logic;
    aud_reset_n              : out   std_logic;
    aud_sync                 : out   std_logic;

    -- Video DAC
    vid_blank                : out   std_logic;
    vid_clk                  : out   std_logic;
    vid_r                    : out   std_logic_vector(7 downto 0);
    vid_g                    : out   std_logic_vector(7 downto 0);
    vid_b                    : out   std_logic_vector(7 downto 0);
    vid_hsync                : out   std_logic;
    vid_psave_n              : out   std_logic;
    vid_sync_n               : out   std_logic;
    vid_vsync                : out   std_logic;
                                     
    -- Extension Connectors          
    x301                     : inout std_logic_vector(19 downto 2);
    x303                     : inout std_logic_vector(30 downto 1);

    -- RS 232
    rs232_rxd                : in    std_logic_vector(1 downto 0);
    rs232_txd                : out   std_logic_vector(1 downto 0);
    rs232_cts                : in    std_logic_vector(1 downto 0);
    rs232_rts                : out   std_logic_vector(1 downto 0);

    -- USB
    usb_rcv                  : in    std_logic;
    usb_vp                   : in    std_logic;
    usb_vm                   : in    std_logic;
    usb_vbus                 : in    std_logic;
    usb_oe_n                 : out   std_logic;
    usb_softcon              : out   std_logic;
    usb_suspnd               : out   std_logic;
    usb_vmo                  : out   std_logic;
    usb_vpo                  : out   std_logic
  );
end zefant_xs3_vp;


library ieee;
use ieee.numeric_std.all;

library unisim;
use unisim.vcomponents.all;

use work.tech_comp_pack.vp_por;
use work.vp_console_comp_pack.vp_console;
use work.board_misc_comp_pack.mc_ctrl;
use work.board_misc_comp_pack.dblscan;
use work.board_misc_comp_pack.pcm_sound;
use work.snespad_comp.snespad;
use work.i8244_col_pack.all;
use work.board_misc_comp_pack.vp_keymap;
use work.ps2_keyboard_comp_pack.ps2_keyboard_interface;

architecture struct of zefant_xs3_vp is

  -----------------------------------------------------------------------------
  -- Include multicard controller
  --
  -- Settings the following constant to true, includes the multicard
  -- controller. It supports selecting several cartridge images on SR1 by
  -- software.
  -- If it's set to false, one single cartridge image will be expected at SR1.
  --
  constant multi_card_c : boolean := true;
  --
  -----------------------------------------------------------------------------

  component zefant_xs3_pll
    port (
      clkin_i    : in  std_logic;
      locked_o   : out std_logic;
      clk_43m_o  : out std_logic;
      clk_21m5_o : out std_logic
    );
  end component;

  signal dcm_locked_s   : std_logic;
  signal reset_n_s,
         reset_s        : std_logic;
  signal por_n_s        : std_logic;
  signal clk_43m_s      : std_logic;
  signal clk_21m5_s     : std_logic;

  signal glob_res_n_s   : std_logic;

  -- CPU clock = PLL clock 21.5 MHz / 4
  constant cnt_cpu_c    : unsigned(1 downto 0) := to_unsigned(3, 2);
  -- VDC clock = PLL clock 21.5 MHz / 3
  -- note: VDC core runs with double frequency than compared with 8244 chip
  constant cnt_vdc_c    : unsigned(1 downto 0) := to_unsigned(2, 2);
  -- VGA clock = PLL clock 43 MHz / 3 (2x VDC clock)
  constant cnt_vga_c    : unsigned(1 downto 0) := to_unsigned(2, 2);
  --
  signal cnt_cpu_q      : unsigned(1 downto 0);
  signal cnt_vdc_q      : unsigned(1 downto 0);
  signal cnt_vga_q      : unsigned(1 downto 0);
  signal clk_cpu_en_s,
         clk_vdc_en_s,
         clk_vga_en_q   : std_logic;

  signal cart_a_s       : std_logic_vector(11 downto 0);
  signal rom_a_s        : std_logic_vector(12 downto 0);
  signal cart_d_s,
         cart_d_from_vp_s,
         rom_d_s        : std_logic_vector( 7 downto 0);
  signal cart_bs0_s,
         cart_bs1_s,
         cart_psen_n_s  : std_logic;

  signal pad_clk_s      : std_logic;
  signal pad_latch_s    : std_logic;
  signal pad_data_s     : std_logic_vector( 1 downto 0);
  signal but_a_s,
         but_b_s,
         but_x_s,
         but_y_s,
         but_start_s,
         but_sel_s,
         but_tl_s,
         but_tr_s       : std_logic_vector( 1 downto 0);
  signal but_up_s,
         but_down_s,
         but_left_s,
         but_right_s    : std_logic_vector( 1 downto 0);

  signal joy_up_n_s,
         joy_down_n_s,
         joy_left_n_s,
         joy_right_n_s,
         joy_action_n_s : std_logic_vector( 1 downto 0);

  signal rgb_r_s,
         rgb_g_s,
         rgb_b_s,
         rgb_l_s        : std_logic;
  signal rgb_hsync_n_s,
         rgb_hsync_s,
         rgb_vsync_n_s,
         rgb_vsync_s    : std_logic;
  signal vga_r_s,
         vga_g_s,
         vga_b_s,
         vga_l_s        : std_logic;
  signal vga_hsync_s,
         vga_vsync_s    : std_logic;
  signal blank_s        : std_logic;

  signal snd_vec_s      : std_logic_vector( 3 downto 0);
  signal pcm_audio_s    : signed(8 downto 0);

  signal aud_bit_clk_s  : std_logic;

  signal keyb_dec_s      : std_logic_vector( 6 downto 1);
  signal keyb_enc_s      : std_logic_vector(14 downto 7);
  signal rx_data_ready_s : std_logic;
  signal rx_ascii_s      : std_logic_vector( 7 downto 0);
  signal rx_released_s   : std_logic;
  signal rx_read_s       : std_logic;

  signal cart_cs_s,
         cart_cs_n_s,
         cart_wr_n_s     : std_logic;
  signal extmem_a_s      : std_logic_vector(18 downto 0);

  signal gnd8_s : std_logic_vector(7 downto 0);

begin

  gnd8_s <= (others => '0');

  glob_res_n_s <= por_n_s and dcm_locked_s and button(0);
  reset_n_s <= glob_res_n_s and dcm_locked_s and
               (but_tl_s(0) or but_tr_s(0));
  reset_s   <= not reset_n_s;

  -----------------------------------------------------------------------------
  -- Power-on reset module
  -----------------------------------------------------------------------------
  por_b : vp_por
    generic map (
      delay_g     => 3,
      cnt_width_g => 2
    )
    port map (
      clk_i   => clk_21m5_s,
      por_n_o => por_n_s
    );


  -----------------------------------------------------------------------------
  -- The PLL
  -----------------------------------------------------------------------------
  pll_b : zefant_xs3_pll
    port map (
      clkin_i    => osc1,
      locked_o   => dcm_locked_s,
      clk_43m_o  => clk_43m_s,
      clk_21m5_o => clk_21m5_s
    );


  -----------------------------------------------------------------------------
  -- Process clk_en
  --
  -- Purpose:
  --   Generates the CPU and VDC clock enables.
  --
  clk_en: process (clk_21m5_s, reset_n_s)
  begin
    if reset_n_s = '0' then
      cnt_cpu_q    <= cnt_cpu_c;
      cnt_vdc_q    <= cnt_vdc_c;

    elsif rising_edge(clk_21m5_s) then
      if clk_cpu_en_s = '1' then
        cnt_cpu_q <= cnt_cpu_c;
      else
        cnt_cpu_q <= cnt_cpu_q - 1;
      end if;
      --
      if clk_vdc_en_s = '1' then
        cnt_vdc_q <= cnt_vdc_c;
      else
        cnt_vdc_q <= cnt_vdc_q - 1;
      end if;
    end if;
  end process clk_en;
  --
  clk_cpu_en_s <= '1' when cnt_cpu_q = 0 else '0';
  clk_vdc_en_s <= '1' when cnt_vdc_q = 0 else '0';
  --
  -----------------------------------------------------------------------------


  -----------------------------------------------------------------------------
  -- Process vga_clk_en
  --
  -- Purpose:
  --   Generates the VGA clock enable.
  --
  vga_clk_en: process (clk_43m_s, reset_n_s)
  begin
    if reset_n_s = '0' then
      cnt_vga_q    <= cnt_vga_c;
      clk_vga_en_q <= '0';
    elsif rising_edge(clk_43m_s) then
      if cnt_vga_q = 0 then
        cnt_vga_q    <= cnt_vga_c;
        clk_vga_en_q <= '1';
      else
        cnt_vga_q    <= cnt_vga_q - 1;
        clk_vga_en_q <= '0';
      end if;
    end if;
  end process vga_clk_en;
  --
  -----------------------------------------------------------------------------


  -----------------------------------------------------------------------------
  -- The Videopac console
  -----------------------------------------------------------------------------
  vp_console_b : vp_console
    generic map (
      is_pal_g => 0
    )
    port map (
      clk_i          => clk_21m5_s,
      clk_cpu_en_i   => clk_cpu_en_s,
      clk_vdc_en_i   => clk_vdc_en_s,
      res_n_i        => reset_n_s,
      cart_cs_o      => cart_cs_s,
      cart_cs_n_o    => cart_cs_n_s,
      cart_wr_n_o    => cart_wr_n_s,
      cart_a_o       => cart_a_s,
      cart_d_i       => cart_d_s,
      cart_d_o       => cart_d_from_vp_s,
      cart_bs0_o     => cart_bs0_s,
      cart_bs1_o     => cart_bs1_s,
      cart_psen_n_o  => cart_psen_n_s,
      cart_t0_i      => gnd8_s(0),
      cart_t0_o      => open,
      cart_t0_dir_o  => open,
      -- idx = 0 : left joystick
      -- idx = 1 : right joystick
      joy_up_n_i     => joy_up_n_s,
      joy_down_n_i   => joy_down_n_s,
      joy_left_n_i   => joy_left_n_s,
      joy_right_n_i  => joy_right_n_s,
      joy_action_n_i => joy_action_n_s,
      keyb_dec_o     => keyb_dec_s,
      keyb_enc_i     => keyb_enc_s,
      r_o            => rgb_r_s,
      g_o            => rgb_g_s,
      b_o            => rgb_b_s,
      l_o            => rgb_l_s,
      hsync_n_o      => rgb_hsync_n_s,
      vsync_n_o      => rgb_vsync_n_s,
      hbl_o          => open,
      vbl_o          => open,
      snd_o          => open,
      snd_vec_o      => snd_vec_s
    );


  -----------------------------------------------------------------------------
  -- Multicard controller
  -----------------------------------------------------------------------------
  use_mc: if multi_card_c generate
    mc_ctrl_b : mc_ctrl
      port map (
        clk_i       => clk_21m5_s,
        reset_n_i   => glob_res_n_s,
        cart_a_i    => cart_a_s,
        cart_d_i    => cart_d_from_vp_s,
        cart_cs_i   => cart_cs_s,
        cart_cs_n_i => cart_cs_n_s,
        cart_wr_n_i => cart_wr_n_s,
        cart_bs0_i  => cart_bs0_s,
        cart_bs1_i  => cart_bs1_s,
        extmem_a_o  => extmem_a_s
      );
  end generate;
  no_mc: if not multi_card_c generate
    extmem_a_s <= (others => '0');
  end generate;


  -----------------------------------------------------------------------------
  -- Assemble the standard cartridge ROM address bus
  -----------------------------------------------------------------------------
  rom_a_s <= ( 0 => cart_a_s( 0),
               1 => cart_a_s( 1),
               2 => cart_a_s( 2),
               3 => cart_a_s( 3),
               4 => cart_a_s( 4),
               5 => cart_a_s( 5),
               6 => cart_a_s( 6),
               7 => cart_a_s( 7),
               8 => cart_a_s( 8),
               9 => cart_a_s( 9),
              10 => cart_a_s(11),
              11 => cart_bs0_s,
              12 => cart_bs1_s);


  -----------------------------------------------------------------------------
  -- Process sram_ctrl
  --
  -- Purpose:
  --   Maps the external SRAM to the cartridge interface.
  --
  sram_ctrl: process (rom_a_s,
                      cart_psen_n_s,
                      sr1_d,
                      extmem_a_s)
    variable sr1_a_v : std_logic_vector(17 downto 0);
    variable a_low_v : std_logic;
  begin
    sr1_lb_n <= '1';
    sr1_ub_n <= '1';
    sr1_a_v := (others => '0');
    rom_d_s  <= (others => '-');

    if multi_card_c then
      sr1_a_v := extmem_a_s(18 downto 1);
      a_low_v := extmem_a_s(0);
    else
      sr1_a_v(11 downto 0) := rom_a_s(12 downto 1);
      a_low_v := rom_a_s(0);
    end if;

    sr1_a <= sr1_a_v;

    if cart_psen_n_s = '0' then
      if a_low_v = '0' then
        sr1_lb_n <= '0';
        rom_d_s  <= sr1_d( 7 downto 0);
      else
        sr1_ub_n <= '0';
        rom_d_s  <= sr1_d(15 downto 8);
      end if;
    end if;
  end process sram_ctrl;
  --
  cart_d_s <=   rom_d_s
              when cart_psen_n_s = '0' else
                (others => '1');
  --
  -----------------------------------------------------------------------------


  -----------------------------------------------------------------------------
  -- VGA Scan Doubler
  -----------------------------------------------------------------------------
  rgb_hsync_s <= not rgb_hsync_n_s;
  rgb_vsync_s <= not rgb_vsync_n_s;
  --
  dblscan_b : dblscan
    port map (
      RGB_R_IN   => rgb_r_s,
      RGB_G_IN   => rgb_g_s,
      RGB_B_IN   => rgb_b_s,
      RGB_L_IN   => rgb_l_s,
      HSYNC_IN   => rgb_hsync_s,
      VSYNC_IN   => rgb_vsync_s,
      VGA_R_OUT  => vga_r_s,
      VGA_G_OUT  => vga_g_s,
      VGA_B_OUT  => vga_b_s,
      VGA_L_OUT  => vga_l_s,
      HSYNC_OUT  => vga_hsync_s,
      VSYNC_OUT  => vga_vsync_s,
      BLANK_OUT  => blank_s,
      CLK_RGB    => clk_21m5_s,
      CLK_EN_RGB => clk_vdc_en_s,
      CLK_VGA    => clk_43m_s,
      CLK_EN_VGA => clk_vga_en_q,
      RESET_N_I  => reset_n_s
    );
  --
  vid_clk   <= clk_vga_en_q;
  vga_rgb: process (clk_43m_s, reset_n_s)
    variable col_v : natural range 0 to 15;
  begin
    if reset_n_s ='0' then
      vid_r <= (others => '0');
      vid_g <= (others => '0');
      vid_b <= (others => '0');
      vid_hsync <= '1';
      vid_vsync <= '1';
      vid_blank <= '1';
    elsif rising_edge(clk_43m_s) then
      if clk_vga_en_q = '1' then
        col_v := to_integer(unsigned'(vga_l_s & vga_r_s & vga_g_s & vga_b_s));
        vid_r <= std_logic_vector(to_unsigned(full_rgb_table_c(col_v)(r_c), 8));
        vid_g <= std_logic_vector(to_unsigned(full_rgb_table_c(col_v)(g_c), 8));
        vid_b <= std_logic_vector(to_unsigned(full_rgb_table_c(col_v)(b_c), 8));

        vid_hsync <= not vga_hsync_s;
        vid_vsync <= not vga_vsync_s;
        vid_blank <= not blank_s;
      end if;
    end if;
  end process vga_rgb;


  -----------------------------------------------------------------------------
  -- PCM Sound Interface to AC97 Codec
  -----------------------------------------------------------------------------
  -- instantiate an IBUFG here to enhance timing path
  --   aud_bit_clk -> aud_sdata_out
  aud_bit_clk_ibufg_b : IBUFG
    port map (
      I => aud_bit_clk,
      O => aud_bit_clk_s
    );
  --
  pcm_audio_s <= signed('0' & snd_vec_s & "0000");
  pcm_sound_b : pcm_sound
    port map (
      clk_i              => clk_21m5_s,
      reset_n_i          => reset_n_s,
      pcm_left_i         => pcm_audio_s,
      pcm_right_i        => pcm_audio_s,
      bit_clk_pad_i      => aud_bit_clk_s,
      sync_pad_o         => aud_sync,
      sdata_pad_o        => aud_sdata_out,
      sdata_pad_i        => aud_sdata_in,
      ac97_reset_pad_n_o => aud_reset_n,
      led_o              => open,
      dpy0_a_o           => open,
      dpy0_b_o           => open,
      dpy0_c_o           => open,
      dpy0_d_o           => open,
      dpy0_e_o           => open,
      dpy0_f_o           => open,
      dpy0_g_o           => open,
      dpy1_a_o           => open,
      dpy1_b_o           => open,
      dpy1_c_o           => open,
      dpy1_d_o           => open,
      dpy1_e_o           => open,
      dpy1_f_o           => open,
      dpy1_g_o           => open
    );


  -----------------------------------------------------------------------------
  -- SNES Gamepads
  -----------------------------------------------------------------------------
  pad_data_s(0) <= x303(19);
  pad_data_s(1) <= x303(18);
  snespads_b : snespad
    generic map (
      num_pads_g       => 2,
      reset_level_g    => 0,
      button_level_g   => 0,
      clocks_per_6us_g => 129
    )
    port map (
      clk_i            => clk_21m5_s,
      reset_i          => por_n_s,
      pad_clk_o        => pad_clk_s,
      pad_latch_o      => pad_latch_s,
      pad_data_i       => pad_data_s,
      but_a_o          => but_a_s,
      but_b_o          => but_b_s,
      but_x_o          => but_x_s,
      but_y_o          => but_y_s,
      but_start_o      => but_start_s,
      but_sel_o        => but_sel_s,
      but_tl_o         => but_tl_s,
      but_tr_o         => but_tr_s,
      but_up_o         => but_up_s,
      but_down_o       => but_down_s,
      but_left_o       => but_left_s,
      but_right_o      => but_right_s
    );
  x303(21) <= pad_latch_s;
  x303(20) <= pad_clk_s;
  -- just connect the single gamepad to both joysticks
  joy_up_n_s     <= (0 => but_up_s(0),
                     1 => but_up_s(0));
  joy_down_n_s   <= (0 => but_down_s(0),
                     1 => but_down_s(0));
  joy_left_n_s   <= (0 => but_left_s(0),
                     1 => but_left_s(0));
  joy_right_n_s  <= (0 => but_right_s(0),
                     1 => but_right_s(0));
  joy_action_n_s <= (0 => but_a_s(0),
                     1 => but_a_s(0));


  -----------------------------------------------------------------------------
  -- Keyboard components
  -----------------------------------------------------------------------------
  vp_keymap_b : vp_keymap
    port map (
      clk_i           => clk_21m5_s,
      res_n_i         => reset_n_s,
      keyb_dec_i      => keyb_dec_s,
      keyb_enc_o      => keyb_enc_s,
      rx_data_ready_i => rx_data_ready_s,
      rx_ascii_i      => rx_ascii_s,
      rx_released_i   => rx_released_s,
      rx_read_o       => rx_read_s
    );
  --
  ps2_keyboard_b : ps2_keyboard_interface
    generic map (
      TIMER_60USEC_VALUE_PP => 1290, -- Number of sys_clks for 60usec
      TIMER_60USEC_BITS_PP  =>   11, -- Number of bits needed for timer
      TIMER_5USEC_VALUE_PP  =>  107, -- Number of sys_clks for debounce
      TIMER_5USEC_BITS_PP   =>    7  -- Number of bits needed for timer
    )
    port map (
      clk             => clk_21m5_s,
      reset           => reset_s,
      ps2_clk         => x303(1),
      ps2_data        => x303(2),
      rx_extended     => open,
      rx_released     => rx_released_s,
      rx_shift_key_on => open,
      rx_ascii        => rx_ascii_s,
      rx_data_ready   => rx_data_ready_s,
      rx_read         => rx_read_s,
      tx_data         => gnd8_s,
      tx_write        => gnd8_s(0),
      tx_write_ack    => open,
      tx_error_no_keyboard_ack => open
    );


  -----------------------------------------------------------------------------
  -- Default values for unused ports
  -----------------------------------------------------------------------------

  -- Flash Memory -------------------------------------------------------------
  fl_a      <= (others => '0');
  fl_d      <= (others => 'Z');
  fl_ce_n   <= '1';
  fl_oe_n   <= '1';
  fl_we_n   <= '1';
  fl_byte_n <= '1';
  fl_rp_n   <= '1';

  fpga_cs_b      <= 'Z';
  fpga_dout_busy <= 'Z';
  fpga_init_b    <= 'Z';
  fpga_rdwr_b    <= 'Z';

  fpga_cpld_io(7 downto 2) <= (others => 'Z');

--  cpld_clk       <= '0';
  -- same pin assigned clkd_clk <=> x303(30)
  x303(30) <= '0';

  -- SRAMs in SO-DIMM Socket --------------------------------------------------
  sr0_a <= (others => '0');
  sr0_d <= (others => 'Z');
  sr0_ce_n <= '1';
  sr0_lb_n <= '1';
  sr0_oe_n <= '1';
  sr0_ub_n <= '1';
  sr0_we_n <= '1';
--  sr1_a <= (others => '0');
  sr1_d <= (others => 'Z');
  sr1_ce_n <= '0';
--  sr1_lb_n <= '1';
  sr1_oe_n <= '0';
--  sr1_ub_n <= '1';
  sr1_we_n <= '1';

  -- Baseboard EEPROM ---------------------------------------------------------
  ee_cs_n <= '1';
  ee_sck  <= '0';
  ee_si   <= '0';

  -- User Interface -----------------------------------------------------------
  led <= (others => '0');

  -- Audio Codec --------------------------------------------------------------
--  aud_sdata_out <= '0';
  aud_cin       <= '0';
--  aud_reset_n   <= '0';
--  aud_sync      <= '0';

  -- Video DAC ----------------------------------------------------------------
--  vid_blank   <= '1';
--  vid_clk     <= '0';
--  vid_r       <= (others => '0');
--  vid_g       <= (others => '0');
--  vid_b       <= (others => '0');
--  vid_hsync   <= '0';
  vid_psave_n <= '1';
  vid_sync_n  <= '0';
--  vid_vsync   <= '0';

  -- Extension Connectors -----------------------------------------------------
  x301 <= (others => 'Z');
  x303 <= (others => 'Z');

  -- RS 232 -------------------------------------------------------------------
  rs232_txd <= (others => '1');
  rs232_rts <= (others => '1');

  -- USB ----------------------------------------------------------------------
  usb_oe_n    <= '1';
  usb_softcon <= '0';
  usb_suspnd  <= '1';
  usb_vmo     <= '0';
  usb_vpo     <= '1';

end struct;
