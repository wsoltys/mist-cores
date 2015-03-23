-------------------------------------------------------------------------------
--
-- FPGA Colecovision
--
-- $Id: zefant_xs3_cv.vhd,v 1.4 2006/02/28 22:30:30 arnim Exp $
--
-- Toplevel of the Spartan3 port for Simple Solutions' Zefant-XS3 board.
--   http://zefant.de/
--
-------------------------------------------------------------------------------
--
-- Copyright (c) 2006, Arnim Laeuger (arnim.laeuger@gmx.net)
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

entity zefant_xs3_cv is
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
    x303                     : inout std_logic_vector(34 downto 1);

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
end zefant_xs3_cv;


library ieee;
use ieee.numeric_std.all;

use work.cv_console_comp_pack.cv_console;
use work.tech_comp_pack.generic_ram;
use work.board_misc_comp_pack.pcm_sound;
use work.board_misc_comp_pack.dblscan;
use work.cv_keys_pack.all;
use work.vdp18_col_pack.all;

architecture struct of zefant_xs3_cv is

  component zefant_xs3_clk
    port (
      clkin_i    : in  std_logic;
      locked_o   : out std_logic;
      clk_21m3_o : out std_logic
    );
  end component;

  component coleco_bios
    port (
      Clk : in  std_logic;
      A   : in  std_logic_vector(12 downto 0);
      D   : out std_logic_vector( 7 downto 0)
    );
  end component;

  signal dcm_locked_s        : std_logic;
  signal clk_21m3_s          : std_logic;
  signal clk_cnt_q           : unsigned(1 downto 0);
  signal clk_en_10m7_q       : std_logic;
  signal clk_en_5m37_q       : std_logic;
  signal reset_sync_n_q      : std_logic_vector(1 downto 0);
  signal reset_n_s           : std_logic;

  signal but_a_s,
         but_b_s,
         but_x_s,
         but_y_s,
         but_start_s,
         but_sel_s,
         but_tl_s,
         but_tr_s            : std_logic_vector( 1 downto 0);
  signal but_up_s,
         but_down_s,
         but_left_s,
         but_right_s         : std_logic_vector( 1 downto 0);

  signal ctrl_p1_s,
         ctrl_p2_s,
         ctrl_p3_s,
         ctrl_p4_s,
         ctrl_p5_s,
         ctrl_p6_s,
         ctrl_p7_s,
         ctrl_p8_s,
         ctrl_p9_s           : std_logic_vector( 2 downto 1);

  signal bios_rom_a_s        : std_logic_vector(12 downto 0);
  signal bios_rom_ce_n_s     : std_logic;
  signal bios_rom_d_s        : std_logic_vector( 7 downto 0);

  signal cpu_ram_a_s         : std_logic_vector( 9 downto 0);
  signal cpu_ram_ce_n_s      : std_logic;
  signal cpu_ram_we_n_s      : std_logic;
  signal cpu_ram_d_to_cv_s,
         cpu_ram_d_from_cv_s : std_logic_vector( 7 downto 0);
  signal cpu_ram_we_s        : std_logic;

  signal vram_a_s            : std_logic_vector(13 downto 0);
  signal vram_we_s           : std_logic;
  signal vram_d_to_cv_s,
         vram_d_from_cv_s    : std_logic_vector( 7 downto 0);

  signal cart_a_s            : std_logic_vector(14 downto 0);
  signal cart_d_s            : std_logic_vector( 7 downto 0);
  signal cart_en_80_n_s,
         cart_en_a0_n_s,
         cart_en_c0_n_s,
         cart_en_e0_n_s      : std_logic;

  signal rgb_col_s           : std_logic_vector( 3 downto 0);
  signal rgb_hsync_n_s,
         rgb_vsync_n_s       : std_logic;
  signal rgb_hsync_s,
         rgb_vsync_s         : std_logic;

  signal vga_col_s           : std_logic_vector( 3 downto 0);
  signal vga_hsync_s,
         vga_vsync_s         : std_logic;

  signal audio_s             : signed(8 downto 0);

begin

  -----------------------------------------------------------------------------
  -- Clock Generator
  -----------------------------------------------------------------------------
  zefant_xs3_clk_b : zefant_xs3_clk
    port map (
      clkin_i    => osc1,
      locked_o   => dcm_locked_s,
      clk_21m3_o => clk_21m3_s
    );


  -----------------------------------------------------------------------------
  -- Process reset_sync
  --
  -- Purpose:
  --   Synchronizes the dcm_locked signal for generating the external reset.
  --
  reset_sync: process (clk_21m3_s, dcm_locked_s, button)
  begin
    if dcm_locked_s = '0' or button(0) = '0' then
      reset_sync_n_q <= (others => '0');
    elsif clk_21m3_s'event and clk_21m3_s = '1' then
      reset_sync_n_q(0) <= '1';
      reset_sync_n_q(1) <= reset_sync_n_q(0);
    end if;
  end process reset_sync;
  --
  reset_n_s <= reset_sync_n_q(1);
  --
  -----------------------------------------------------------------------------


  -----------------------------------------------------------------------------
  -- Process clk_cnt
  --
  -- Purpose:
  --   Counts the base clock and derives the clock enables.
  --
  clk_cnt: process (clk_21m3_s, reset_n_s)
  begin
    if reset_n_s = '0' then
      clk_cnt_q     <= (others => '0');
      clk_en_10m7_q <= '0';
      clk_en_5m37_q <= '0';

    elsif clk_21m3_s'event and clk_21m3_s = '1' then
      -- Clock counter --------------------------------------------------------
      if clk_cnt_q = 3 then
        clk_cnt_q <= (others => '0');
      else
        clk_cnt_q <= clk_cnt_q + 1;
      end if;

      -- 10.7 MHz clock enable ------------------------------------------------
      case clk_cnt_q is
        when "01" | "11" =>
          clk_en_10m7_q <= '1';
        when others =>
          clk_en_10m7_q <= '0';
      end case;

      -- 5.37 MHz clock enable ------------------------------------------------
      case clk_cnt_q is
        when "11" =>
          clk_en_5m37_q <= '1';
        when others =>
          clk_en_5m37_q <= '0';
      end case;

    end if;
  end process clk_cnt;
  --
  -----------------------------------------------------------------------------


  -----------------------------------------------------------------------------
  -- The Colecovision console
  -----------------------------------------------------------------------------
  cv_console_b : cv_console
    generic map (
      is_pal_g        => 0,
      compat_rgb_g    => 0
    )
    port map (
      clk_i           => clk_21m3_s,
      clk_en_10m7_i   => clk_en_10m7_q,
      reset_n_i       => reset_n_s,
      por_n_o         => open,
      ctrl_p1_i       => ctrl_p1_s,
      ctrl_p2_i       => ctrl_p2_s,
      ctrl_p3_i       => ctrl_p3_s,
      ctrl_p4_i       => ctrl_p4_s,
      ctrl_p5_o       => ctrl_p5_s,
      ctrl_p6_i       => ctrl_p6_s,
      ctrl_p7_i       => ctrl_p7_s,
      ctrl_p8_o       => ctrl_p8_s,
      ctrl_p9_i       => ctrl_p9_s,
      bios_rom_a_o    => bios_rom_a_s,
      bios_rom_ce_n_o => bios_rom_ce_n_s,
      bios_rom_d_i    => bios_rom_d_s,
      cpu_ram_a_o     => cpu_ram_a_s,
      cpu_ram_ce_n_o  => cpu_ram_ce_n_s,
      cpu_ram_we_n_o  => cpu_ram_we_n_s,
      cpu_ram_d_i     => cpu_ram_d_to_cv_s,
      cpu_ram_d_o     => cpu_ram_d_from_cv_s,
      vram_a_o        => vram_a_s,
      vram_we_o       => vram_we_s,
      vram_d_o        => vram_d_from_cv_s,
      vram_d_i        => vram_d_to_cv_s,
      cart_a_o        => cart_a_s,
      cart_en_80_n_o  => cart_en_80_n_s,
      cart_en_a0_n_o  => cart_en_a0_n_s,
      cart_en_c0_n_o  => cart_en_c0_n_s,
      cart_en_e0_n_o  => cart_en_e0_n_s,
      cart_d_i        => cart_d_s,
      col_o           => rgb_col_s,
      rgb_r_o         => open,
      rgb_g_o         => open,
      rgb_b_o         => open,
      hsync_n_o       => rgb_hsync_n_s,
      vsync_n_o       => rgb_vsync_n_s,
      comp_sync_n_o   => open,
      audio_o         => audio_s(8 downto 1)
    );

  rgb_hsync_s <= not rgb_hsync_n_s;
  rgb_vsync_s <= not rgb_vsync_n_s;
  audio_s(0)  <= '0';


  -----------------------------------------------------------------------------
  -- BIOS ROM
  -----------------------------------------------------------------------------
  coleco_bios_b : coleco_bios
    port map (
      Clk => clk_21m3_s,
      A   => bios_rom_a_s,
      D   => bios_rom_d_s
    );


  -----------------------------------------------------------------------------
  -- CPU RAM
  -----------------------------------------------------------------------------
  cpu_ram_we_s <= clk_en_10m7_q and
                  not (cpu_ram_we_n_s or cpu_ram_ce_n_s);
  cpu_ram_b : generic_ram
    generic map (
      addr_width_g => 10,
      data_width_g => 8
    )
    port map (
      clk_i => clk_21m3_s,
      a_i   => cpu_ram_a_s,
      we_i  => cpu_ram_we_s,
      d_i   => cpu_ram_d_from_cv_s,
      d_o   => cpu_ram_d_to_cv_s
    );


  -----------------------------------------------------------------------------
  -- VRAM
  -----------------------------------------------------------------------------
  vram_b : generic_ram
    generic map (
      addr_width_g => 14,
      data_width_g => 8
    )
    port map (
      clk_i => clk_21m3_s,
      a_i   => vram_a_s,
      we_i  => vram_we_s,
      d_i   => vram_d_from_cv_s,
      d_o   => vram_d_to_cv_s
    );


  -----------------------------------------------------------------------------
  -- Process cart_if
  --
  -- Purpose:
  --   Manages the cartridge interface.
  --
  cart_if: process (cart_a_s,
                    cart_en_80_n_s, cart_en_a0_n_s,
                    cart_en_c0_n_s, cart_en_e0_n_s,
                    sr1_d)
  begin
    sr1_we_n            <= '1';
    sr1_oe_n            <= '0';
    sr1_lb_n            <= '0';
    sr1_ub_n            <= '0';
    sr1_a(17 downto 14) <= (others => '0');
    sr1_a(13 downto  0) <= cart_a_s(14 downto 1);

    if (cart_en_80_n_s and cart_en_a0_n_s and
        cart_en_c0_n_s and cart_en_e0_n_s) = '0' then
      sr1_ce_n <= '0';
    else
      sr1_ce_n <= '1';
    end if;

    if cart_a_s(0) = '0' then
      cart_d_s <= sr1_d( 7 downto 0);
    else
      cart_d_s <= sr1_d(15 downto 8);
    end if;

  end process cart_if;
  --
  -----------------------------------------------------------------------------


  -----------------------------------------------------------------------------
  -- Process pad_ctrl
  --
  -- Purpose:
  --   Maps the gamepad signals to the controller buses of the console.
  --
  pad_ctrl: process (ctrl_p5_s, ctrl_p8_s,
                     but_a_s, but_b_s,
                     but_up_s, but_down_s, but_left_s, but_right_s,
                     but_x_s, but_y_s,
                     but_sel_s, but_start_s,
                     but_tl_s, but_tr_s)
    variable key_v : natural range cv_keys_t'range;
  begin
    -- quadrature device not implemented
    ctrl_p7_s          <= "11";
    ctrl_p9_s          <= "11";

    for idx in 1 to 2 loop
      if    ctrl_p5_s(idx) = '0' and ctrl_p8_s(idx) = '1' then
        -- keys and right button enabled --------------------------------------
        -- keys not fully implemented

        key_v := cv_key_none_c;

        if but_tl_s(idx-1) = '0' then
          if    but_a_s(idx-1) = '0' then
            -- KEY 1
            key_v := cv_key_1_c;
          elsif but_b_s(idx-1) = '0' then
            -- KEY 2
            key_v := cv_key_2_c;
          elsif but_x_s(idx-1) = '0' then
            -- KEY 3
            key_v := cv_key_3_c;
          elsif but_y_s(idx-1) = '0' then
            -- KEY 4
            key_v := cv_key_4_c;
          elsif but_sel_s(idx-1) = '0' then
            -- KEY *
            key_v := cv_key_asterisk_c;
          elsif but_start_s(idx-1) = '0' then
            -- KEY #
            key_v := cv_key_number_c;
          end if;
        end if;

        ctrl_p1_s(idx) <= cv_keys_c(key_v)(1);
        ctrl_p2_s(idx) <= cv_keys_c(key_v)(2);
        ctrl_p3_s(idx) <= cv_keys_c(key_v)(3);
        ctrl_p4_s(idx) <= cv_keys_c(key_v)(4);

        if but_tl_s(idx-1) = '1' then
          ctrl_p6_s(idx) <= but_b_s(idx-1);
        else
          ctrl_p6_s(idx) <= '1';
        end if;

      elsif ctrl_p5_s(idx) = '1' and ctrl_p8_s(idx) = '0' then
        -- joystick and left button enabled -----------------------------------
        ctrl_p1_s(idx) <= but_up_s(idx-1);
        ctrl_p2_s(idx) <= but_down_s(idx-1);
        ctrl_p3_s(idx) <= but_left_s(idx-1);
        ctrl_p4_s(idx) <= but_right_s(idx-1);
        ctrl_p6_s(idx) <= but_a_s(idx-1);

      else
        -- nothing active -----------------------------------------------------
        ctrl_p1_s(idx) <= '1';
        ctrl_p2_s(idx) <= '1';
        ctrl_p3_s(idx) <= '1';
        ctrl_p4_s(idx) <= '1';
        ctrl_p6_s(idx) <= '1';
        ctrl_p7_s(idx) <= '1';
      end if;
    end loop;
  end process pad_ctrl;
  --
  -----------------------------------------------------------------------------


  -----------------------------------------------------------------------------
  -- VGA Scan Doubler
  -----------------------------------------------------------------------------
  dblscan_b : dblscan
    port map (
      COL_IN     => rgb_col_s,
      HSYNC_IN   => rgb_hsync_s,
      VSYNC_IN   => rgb_vsync_s,
      COL_OUT    => vga_col_s,
      HSYNC_OUT  => vga_hsync_s,
      VSYNC_OUT  => vga_vsync_s,
      BLANK_OUT  => open,
      CLK_6      => clk_21m3_s,
      CLK_EN_6M  => clk_en_5m37_q,
      CLK_12     => clk_21m3_s,
      CLK_EN_12M => clk_en_10m7_q
    );


  -----------------------------------------------------------------------------
  -- PCM Sound Interface to AC97 Codec
  -----------------------------------------------------------------------------
  pcm_sound_b : pcm_sound
    port map (
      clk_i              => clk_21m3_s,
      reset_n_i          => reset_n_s,
      pcm_left_i         => audio_s,
      pcm_right_i        => audio_s,
      bit_clk_pad_i      => aud_bit_clk,
      sync_pad_o         => aud_sync,
      sdata_pad_o        => aud_sdata_out,
      sdata_pad_i        => aud_sdata_in,
      ac97_reset_pad_n_o => aud_reset_n,
      led_o              => open,
      dpy0_a_o           => fpga_cpld_io(0),
      dpy0_b_o           => fpga_cpld_io(1),
      dpy0_c_o           => fpga_cpld_io(2),
      dpy0_d_o           => fpga_cpld_io(3),
      dpy0_e_o           => fpga_cpld_io(4),
      dpy0_f_o           => fpga_cpld_io(5),
      dpy0_g_o           => fpga_cpld_io(6),
      dpy1_a_o           => open,
      dpy1_b_o           => open,
      dpy1_c_o           => open,
      dpy1_d_o           => open,
      dpy1_e_o           => open,
      dpy1_f_o           => open,
      dpy1_g_o           => open
    );
  fpga_cpld_io(7) <= 'Z';


  -----------------------------------------------------------------------------
  -- VGA Output
  -----------------------------------------------------------------------------
  -- Process vga_col
  --
  -- Purpose:
  --   Converts the color information (doubled to VGA scan) to RGB values.
  --
  vga_col: process (clk_21m3_s, reset_n_s)
    variable vga_col_v : natural range 0 to 15;
    variable vga_r_v,
             vga_g_v,
             vga_b_v   : rgb_val_t;
  begin
    if reset_n_s = '0' then
      vid_r <= (others => '0');
      vid_g <= (others => '0');
      vid_b <= (others => '0');

    elsif clk_21m3_s'event and clk_21m3_s = '1' then
      if clk_en_10m7_q = '1' then
        vga_col_v := to_integer(unsigned(vga_col_s));
        vga_r_v   := full_rgb_table_c(vga_col_v)(r_c);
        vga_g_v   := full_rgb_table_c(vga_col_v)(g_c);
        vga_b_v   := full_rgb_table_c(vga_col_v)(b_c);
        --
        vid_r     <= std_logic_vector(to_unsigned(vga_r_v, 8));
        vid_g     <= std_logic_vector(to_unsigned(vga_g_v, 8));
        vid_b     <= std_logic_vector(to_unsigned(vga_b_v, 8));
      end if;

    end if;
  end process vga_col;
  --
  vid_hsync         <= not vga_hsync_s;
  vid_vsync         <= not vga_vsync_s;
  vid_blank         <= '1';
  vid_clk           <= clk_en_10m7_q;
  --
  -----------------------------------------------------------------------------


  -----------------------------------------------------------------------------
  -- Buttons and Lights
  -----------------------------------------------------------------------------
  x303(34 downto 29) <= (others => 'Z');
  x303(          28) <= '0';
  x303(          26) <= '0';
  x303(          24) <= '0';
  x303(          22) <= '0';
  x303(20 downto 19) <= (others => 'Z');
  x303(          18) <= '0';
  x303(          16) <= '0';
  x303(14 downto 13) <= (others => 'Z');
  x303(          12) <= '0';
  x303(          10) <= '0';
  x303(           8) <= '0';
  x303(           6) <= '0';
  x303(           4) <= '0';
  x303(           2) <= '0';

  but_up_s(0)    <= x303( 5);
  but_down_s(0)  <= x303( 7);
  but_left_s(0)  <= x303( 1);
  but_right_s(0) <= x303( 3);
  but_a_s(0)     <= x303(15);
  but_b_s(0)     <= x303( 9);
  but_x_s(0)     <= x303(17);
  but_y_s(0)     <= x303(11);
  but_start_s(0) <= x303(27);
  but_sel_s(0)   <= x303(25);
  but_tl_s(0)    <= x303(21);
  but_tr_s(0)    <= x303(23);
  -----------------------------------------------------------------------------
  but_up_s(1)    <= '1';
  but_down_s(1)  <= '1';
  but_left_s(1)  <= '1';
  but_right_s(1) <= '1';
  but_a_s(1)     <= '1';
  but_b_s(1)     <= '1';
  but_x_s(1)     <= '1';
  but_y_s(1)     <= '1';
  but_start_s(1) <= '1';
  but_sel_s(1)   <= '1';
  but_tl_s(1)    <= '1';
  but_tr_s(1)    <= '1';

  led(0)           <= reset_n_s;


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

--  fpga_cpld_io   <= (others => 'Z');

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
--  sr1_d <= (others => 'Z');
--  sr1_ce_n <= '1';
--  sr1_lb_n <= '1';
--  sr1_oe_n <= '1';
--  sr1_ub_n <= '1';
--  sr1_we_n <= '1';

  -- Baseboard EEPROM ---------------------------------------------------------
  ee_cs_n <= '1';
  ee_sck  <= '0';
  ee_si   <= '0';

  -- User Interface -----------------------------------------------------------
  led(5 downto 1) <= (others => '0');

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
--  x303 <= (others => 'Z');

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
