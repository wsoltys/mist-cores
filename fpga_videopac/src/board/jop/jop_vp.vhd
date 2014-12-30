-------------------------------------------------------------------------------
--
-- FPGA Videopac
--
-- $Id: jop_vp.vhd,v 1.11 2007/04/10 21:29:02 arnim Exp $
-- $Name: videopac_rel_1_0 $
--
-- Toplevel of the Cyclone port for JOP.design's cycore board.
--   http://jopdesign.com/
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

entity jop_vp is

  port (
    ext_clk_i     : in    std_logic;
    rgb_r_o       : out   std_logic_vector( 2 downto 0);
    rgb_g_o       : out   std_logic_vector( 2 downto 0);
    rgb_b_o       : out   std_logic_vector( 2 downto 0);
    comp_sync_n_o : out   std_logic;
    audio_l_o     : out   std_logic;
    audio_r_o     : out   std_logic;
    audio_o       : out   std_logic_vector( 7 downto 0);
    pad_clk_o     : out   std_logic;
    pad_latch_o   : out   std_logic;
    pad_data_i    : in    std_logic_vector( 1 downto 0);
    rxd_i         : in    std_logic;
    txd_o         : out   std_logic;
    cts_i         : in    std_logic;
    rts_o         : out   std_logic;
    rama_a_o      : out   std_logic_vector(17 downto 0);
    rama_d_b      : inout std_logic_vector(15 downto 0);
    rama_cs_n_o   : out   std_logic;
    rama_oe_n_o   : out   std_logic;
    rama_we_n_o   : out   std_logic;
    rama_lb_n_o   : out   std_logic;
    rama_ub_n_o   : out   std_logic;
    ramb_a_o      : out   std_logic_vector(17 downto 0);
    ramb_d_b      : inout std_logic_vector(15 downto 0);
    ramb_cs_n_o   : out   std_logic;
    ramb_oe_n_o   : out   std_logic;
    ramb_we_n_o   : out   std_logic;
    ramb_lb_n_o   : out   std_logic;
    ramb_ub_n_o   : out   std_logic;
    fl_a_o        : out   std_logic_vector(18 downto 0);
    fl_d_b        : inout std_logic_vector( 7 downto 0);
    fl_we_n_o     : out   std_logic;
    fl_oe_n_o     : out   std_logic;
    fl_cs_n_o     : out   std_logic;
    fl_cs2_n_o    : out   std_logic;
    fl_rdy_i      : in    std_logic
  );

end jop_vp;


library ieee;
use ieee.numeric_std.all;

library altera_mf;

use work.board_misc_comp_pack.dac;
use work.vp_console_comp_pack.vp_console;
use work.snespad_comp.snespad;
use work.i8244_col_pack.all;

architecture struct of jop_vp is

  component vp_pll
    PORT (
      inclk0 : IN STD_LOGIC  := '0';
      c0     : OUT STD_LOGIC ;
      locked : OUT STD_LOGIC 
    );
  end component;

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

  component lpm_rom
    generic (
      LPM_WIDTH           : positive;
      LPM_WIDTHAD         : positive;
      LPM_NUMWORDS        : natural := 0;
      LPM_ADDRESS_CONTROL : string  := "REGISTERED";
      LPM_OUTDATA         : string  := "REGISTERED";
      LPM_FILE            : string;
      LPM_TYPE            : string  := "LPM_ROM";
      LPM_HINT            : string  := "UNUSED"
    );
    port (
      ADDRESS  : in STD_LOGIC_VECTOR(LPM_WIDTHAD-1 downto 0);
      INCLOCK  : in STD_LOGIC := '0';
      MEMENAB  : in STD_LOGIC := '1';
      Q        : out STD_LOGIC_VECTOR(LPM_WIDTH-1 downto 0)
    );
  end component;

  signal clk_s          : std_logic;

  -- CPU clock = PLL clock / 6
  constant cnt_cpu_c    : unsigned(2 downto 0) := to_unsigned(5, 3);
  -- VDC clock = PLL clock / 5
  constant cnt_vdc_c    : unsigned(2 downto 0) := to_unsigned(4, 3);
  signal cnt_cpu_q      : unsigned(2 downto 0);
  signal cnt_vdc_q      : unsigned(2 downto 0);
  signal clk_cpu_en_s,
         clk_vdc_en_s   : std_logic;

  signal pll_locked_s   : std_logic;
  signal reset_n_s      : std_logic;
  signal por_n_s        : std_logic;

  signal cart_a_s       : std_logic_vector(11 downto 0);
  signal rom_a_s        : std_logic_vector(12 downto 0);
  signal cart_d_s,
         rom_d_s        : std_logic_vector( 7 downto 0);
  signal cart_bs0_s,
         cart_bs1_s,
         cart_psen_n_s  : std_logic;

  signal keyb_dec_s     : std_logic_vector( 6 downto 1);
  signal keyb_enc_s     : std_logic_vector(14 downto 7);

  signal r_s,
         g_s,
         b_s,
         l_s            : std_logic;
  signal hsync_n_s,
         vsync_n_s      : std_logic;

  signal snd_s          : std_logic;
  signal snd_vec_s      : std_logic_vector(3 downto 0);

  signal joy_up_n_s,
         joy_down_n_s,
         joy_left_n_s,
         joy_right_n_s,
         joy_action_n_s : std_logic_vector( 1 downto 0);
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

  signal dac_audio_s    : std_logic_vector( 7 downto 0);
  signal audio_s        : std_logic;

  signal vdd_s          : std_logic;
  signal gnd_s          : std_logic;

begin

  vdd_s <= '1';
  gnd_s <= '0';


  por_b : vp_por
    generic map (
       delay_g     => 4,
       cnt_width_g => 2
    )
    port map (
       clk_i   => clk_s,
       por_n_o => por_n_s
    );


  reset_n_s <= (but_tl_s(0) or but_tr_s(0)) and pll_locked_s and por_n_s;


  -----------------------------------------------------------------------------
  -- The PLL
  -----------------------------------------------------------------------------
  pll_b : vp_pll
    port map (
      inclk0 => ext_clk_i,
      c0     => clk_s,
      locked => pll_locked_s
    );


  -----------------------------------------------------------------------------
  -- Process clk_en
  --
  -- Purpose:
  --   Generates the CPU and VDC clock enables.
  --
  clk_en: process (clk_s, reset_n_s)
  begin
    if reset_n_s = '0' then
      cnt_cpu_q <= cnt_cpu_c;
      cnt_vdc_q <= cnt_vdc_c;
    elsif rising_edge(clk_s) then
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
  -- The Videopac console
  -----------------------------------------------------------------------------
  vp_console_b : vp_console
    generic map (
      is_pal_g => 1
    )
    port map (
      clk_i          => clk_s,
      clk_cpu_en_i   => clk_cpu_en_s,
      clk_vdc_en_i   => clk_vdc_en_s,
      res_n_i        => reset_n_s,
      cart_cs_o      => open,
      cart_cs_n_o    => open,
      cart_wr_n_o    => open,
      cart_a_o       => cart_a_s,
      cart_d_i       => cart_d_s,
      cart_bs0_o     => cart_bs0_s,
      cart_bs1_o     => cart_bs1_s,
      cart_psen_n_o  => cart_psen_n_s,
      cart_t0_i      => gnd_s,
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
      r_o            => r_s,
      g_o            => g_s,
      b_o            => b_s,
      l_o            => l_s,
      hsync_n_o      => hsync_n_s,
      vsync_n_o      => vsync_n_s,
      hbl_o          => open,
      vbl_o          => open,
      snd_o          => snd_s,
      snd_vec_o      => snd_vec_s
    );
  --
  rgb: process (clk_s, reset_n_s)
    variable col_v : natural range 0 to 15;
  begin
    if reset_n_s = '0' then
      rgb_r_o <= (others => '0');
      rgb_g_o <= (others => '0');
      rgb_b_o <= (others => '0');

    elsif rising_edge(clk_s) then
      col_v := to_integer(unsigned'(l_s & r_s & g_s & b_s));
      rgb_r_o <= std_logic_vector(to_unsigned(full_rgb_table_c(col_v)(r_c), 8))(7 downto 5);
      rgb_g_o <= std_logic_vector(to_unsigned(full_rgb_table_c(col_v)(g_c), 8))(7 downto 5);
      rgb_b_o <= std_logic_vector(to_unsigned(full_rgb_table_c(col_v)(b_c), 8))(7 downto 5);
    end if;
  end process rgb;
  --
  comp_sync_n_o <= hsync_n_s and vsync_n_s;


  -----------------------------------------------------------------------------
  -- The cartridge ROM
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
  --
  cart_rom_b : lpm_rom
    generic map (
      LPM_WIDTH           =>  8,
      LPM_WIDTHAD         => 13,
      LPM_ADDRESS_CONTROL => "REGISTERED",
      LPM_OUTDATA         => "UNREGISTERED",
      LPM_FILE            => "cart.hex",
      LPM_TYPE            => "LPM_ROM"
    )
    port map (
      ADDRESS  => rom_a_s(12 downto 0),
      INCLOCK  => clk_s,
      MEMENAB  => vdd_s,
      Q        => rom_d_s
    );
  --
  cart_d_s <=   rom_d_s
              when cart_psen_n_s = '0' else
                (others => '1');


--  -----------------------------------------------------------------------------
--  -- Process sram_ctrl
--  --
--  -- Purpose:
--  --   Maps the external SRAM to the cartridge interface.
--  --
--  sram_ctrl: process (cart_a_s,
--                      rama_d_b)
--  begin
--    rama_lb_n_o <= '1';
--    rama_ub_n_o <= '1';
--    rama_a_o(17 downto 11) <= (others => '0');
--    rama_a_o(10 downto  0) <= cart_a_s(11 downto 1);
--
--    if cart_a_s(0) = '0' then
--      rama_lb_n_o <= '0';
--      cart_d_s <= rama_d_b( 7 downto 0);
--    else
--      rama_ub_n_o <= '0';
--      cart_d_s <= rama_d_b(15 downto 8);
--    end if;
--  end process sram_ctrl;
--  --
--  -----------------------------------------------------------------------------


  -----------------------------------------------------------------------------
  -- SNES Gamepads
  -----------------------------------------------------------------------------
  snespads_b : snespad
    generic map (
      num_pads_g       => 2,
      reset_level_g    => 0,
      button_level_g   => 0,
      clocks_per_6us_g => 198
    )
    port map (
      clk_i            => clk_s,
      reset_i          => por_n_s,
      pad_clk_o        => pad_clk_o,
      pad_latch_o      => pad_latch_o,
      pad_data_i       => pad_data_i,
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
  --
  keyb_enc_s( 7) <= '1';
  keyb_enc_s( 8) <= keyb_dec_s(1) or but_tl_s(0) or but_a_s(0);
  keyb_enc_s( 9) <= '1';
  keyb_enc_s(10) <= '1';
  keyb_enc_s(11) <= '1';
  keyb_enc_s(12) <= '1';
  keyb_enc_s(13) <= '1';
  keyb_enc_s(14) <= '1';
                   

  -----------------------------------------------------------------------------
  -- Digital-analog audio converter
  -----------------------------------------------------------------------------
  dac_audio_s(7 downto 4) <= snd_vec_s;
  dac_audio_s(3 downto 0) <= (others => '0');
  --
  dac_b : dac
    generic map (
      msbi_g => 7
    )
    port map (
      clk_i   => clk_s,
      res_n_i => por_n_s,
      dac_i   => dac_audio_s,
      dac_o   => audio_s
    );
  --
  audio_r_o <= audio_s;
  audio_l_o <= audio_s;
  audio_o   <= dac_audio_s;


  -----------------------------------------------------------------------------
  -- JOP pin defaults
  -----------------------------------------------------------------------------
  -- UART
  txd_o       <= '1';
  rts_o       <= '1';
  -- RAMA
  rama_a_o    <= (others => '0');
  rama_cs_n_o <= '0';
  rama_oe_n_o <= '0';
  rama_we_n_o <= '1';
  rama_lb_n_o <= '1';
  rama_ub_n_o <= '1';
  -- RAMB
  ramb_a_o    <= (others => '0');
  ramb_cs_n_o <= '1';
  ramb_oe_n_o <= '1';
  ramb_we_n_o <= '1';
  ramb_lb_n_o <= '1';
  ramb_ub_n_o <= '1';
  -- Flash
  fl_a_o      <= (others => '0');
  fl_we_n_o   <= '1';
  fl_oe_n_o   <= '1';
  fl_cs_n_o   <= '1';
  fl_cs2_n_o  <= '1';

end struct;
