-------------------------------------------------------------------------------
--
-- FPGA Videopac
--
-- $Id: vp_console.vhd,v 1.15 2007/04/07 10:49:23 arnim Exp $
-- $Name: videopac_rel_1_0 $
--
-- Toplevel of Videopac console
--
-- References:
--
-- * Rene's VIDEOPAC tech info
--   http://www.geocities.com/rene_g7400/vp_info.html
--
-- * Soeren's VIDEOPAC G7000 site
--   http://soeren.informationstheater.de/g7000/Index.html
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

entity vp_console is

  generic (
    is_pal_g : integer := 1
  );
  port (
    -- System Interface -------------------------------------------------------
    clk_i          : in  std_logic;
    clk_cpu_en_i   : in  std_logic;
    clk_vdc_en_i   : in  std_logic;
    res_n_i        : in  std_logic;
    -- Cartridge Interface ----------------------------------------------------
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
    -- Joystick Interface -----------------------------------------------------
    -- idx = 0 : left joystick
    -- idx = 1 : right joystick
    joy_up_n_i     : in  std_logic_vector( 1 downto 0);
    joy_down_n_i   : in  std_logic_vector( 1 downto 0);
    joy_left_n_i   : in  std_logic_vector( 1 downto 0);
    joy_right_n_i  : in  std_logic_vector( 1 downto 0);
    joy_action_n_i : in  std_logic_vector( 1 downto 0);
    -- Keyboard Interface -----------------------------------------------------
    keyb_dec_o     : out std_logic_vector( 6 downto 1);
    keyb_enc_i     : in  std_logic_vector(14 downto 7);
    -- Video Interface --------------------------------------------------------
    r_o            : out std_logic;
    g_o            : out std_logic;
    b_o            : out std_logic;
    l_o            : out std_logic;
    hsync_n_o      : out std_logic;
    vsync_n_o      : out std_logic;
    hbl_o          : out std_logic;
    vbl_o          : out std_logic;
    -- Sound Interface --------------------------------------------------------
    snd_o          : out std_logic;
    snd_vec_o      : out std_logic_vector(3 downto 0)
  );

end vp_console;


library ieee;
use ieee.numeric_std.all;

use work.t48_system_comp_pack.t8048_notri;
use work.i8244_core_comp_pack.i8244_top_sync;

-- pragma translate_off
use work.vp_tb_pack.all;
-- pragma translate_on

architecture struct of vp_console is

  component generic_ram_ena
    generic (
      addr_width_g : integer := 10;
      data_width_g : integer := 8
    );
    port (
      clk_i : in  std_logic;
      a_i   : in  std_logic_vector(addr_width_g-1 downto 0);
      we_i  : in  std_logic;
      ena_i : in  std_logic;
      d_i   : in  std_logic_vector(data_width_g-1 downto 0);
      d_o   : out std_logic_vector(data_width_g-1 downto 0)
    );
  end component;

  component vp_glue
    port (
      clk_i       : in  std_logic;
      res_n_i     : in  std_logic;
      -- Address Interface ----------------------------------------------------
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
      -- Video Interface ------------------------------------------------------
      hbl_i       : in  std_logic;
      vbl_i       : in  std_logic;
      l_i         : in  std_logic;
      p17_i       : in  std_logic;
      blank_o     : out std_logic;
      l_o         : out std_logic
    );
  end component;

  -- CPU signals
  signal int_n_s,
         rd_n_s,
         wr_n_s,
         ale_s          : std_logic;
  signal db_to_cpu_s,
         db_from_cpu_s,
         db_s           : std_logic_vector(7 downto 0);
  signal db_dir_s       : std_logic;
  signal p2_to_cpu_s,
         p2_from_cpu_s  : std_logic_vector(7 downto 0);
  signal p1_to_cpu_s,
         p1_from_cpu_s  : std_logic_vector(7 downto 0);

  -- glue signals
  signal a_low_s     : std_logic_vector( 7 downto 0);
  signal a_s         : std_logic_vector(11 downto 0);
  signal blank_s     : std_logic;
  signal vdc_cs_n_s,
         vdc_rd_n_s,
         vdc_wr_n_s  : std_logic;

  -- VDC signals
  signal hbl_s,
         vbl_s,
         hsync_s,
         vsync_s      : std_logic;
  signal l_from_vdc_s : std_logic;
  signal d_from_vdc_s : std_logic_vector(7 downto 0);

  -- RAM signals
  signal d_from_ram_s : std_logic_vector(7 downto 0);
  signal ram_cs_s,
         ram_cs_n_s,
         ram_wr_n_s,
         ram_wr_s     : std_logic;

  signal keyb_dec_s : std_logic_vector(6 downto 1);

  subtype joy_t  is std_logic_vector(7 downto 0);
  type    joys_t is array (natural range 0 to 1) of joy_t;
  signal  joys_s : joys_t;

  signal vdd_s,
         gnd_s  : std_logic;

begin

  vdd_s <= '1';
  gnd_s <= '0';


  -----------------------------------------------------------------------------
  -- I8048 uController
  -----------------------------------------------------------------------------
  t8048_b : t8048_notri
    generic map (
      gate_port_input_g => 1
    )
    port map (
      xtal_i        => clk_i,
      xtal_en_i     => clk_cpu_en_i,
      reset_n_i     => res_n_i,
      t0_i          => cart_t0_i,
      t0_o          => cart_t0_o,
      t0_dir_o      => cart_t0_dir_o,
      int_n_i       => int_n_s,
      ea_i          => gnd_s,
      rd_n_o        => rd_n_s,
      psen_n_o      => cart_psen_n_o,
      wr_n_o        => wr_n_s,
      ale_o         => ale_s,
      db_i          => db_to_cpu_s,
      db_o          => db_from_cpu_s,
      db_dir_o      => db_dir_s,
      t1_i          => blank_s,
      p2_i          => p2_to_cpu_s,
      p2_o          => p2_from_cpu_s,
      p2l_low_imp_o => open,
      p2h_low_imp_o => open,
      p1_i          => p1_to_cpu_s,
      p1_o          => p1_from_cpu_s,
      p1_low_imp_o  => open,
      prog_n_o      => open
    );
  --
  p1_to_cpu_s      <= (others => '1');
  a_s(11 downto 8) <= p2_from_cpu_s(3 downto 0);
  a_s( 7 downto 0) <= a_low_s;
  cart_a_o         <= a_s;
  cart_bs0_o       <= p1_from_cpu_s(0);
  cart_bs1_o       <= p1_from_cpu_s(1);


  -----------------------------------------------------------------------------
  -- Some glue logic
  -----------------------------------------------------------------------------
  glue_b : vp_glue
    port map (
      clk_i       => clk_i,
      res_n_i     => res_n_i,
      cpu_db_i    => db_s,
      ale_i       => ale_s,
      rd_n_i      => rd_n_s,
      wr_n_i      => wr_n_s,
      p13_i       => p1_from_cpu_s(3),
      p14_i       => p1_from_cpu_s(4),
      p16_i       => p1_from_cpu_s(6),
      a_low_o     => a_low_s,
      cart_cs_o   => cart_cs_o,
      cart_cs_n_o => cart_cs_n_o,
      cart_wr_n_o => cart_wr_n_o,
      ram_cs_o    => ram_cs_s,
      ram_cs_n_o  => ram_cs_n_s,
      ram_wr_n_o  => ram_wr_n_s,
      vdc_cs_n_o  => vdc_cs_n_s,
      vdc_rd_n_o  => vdc_rd_n_s,
      vdc_wr_n_o  => vdc_wr_n_s,
      hbl_i       => hbl_s,
      vbl_i       => vbl_s,
      l_i         => l_from_vdc_s,
      p17_i       => p1_from_cpu_s(7),
      blank_o     => blank_s,
      l_o         => l_o
    );
  --
  cart_d_o <= db_from_cpu_s;


  -----------------------------------------------------------------------------
  -- I8244 VDC
  -----------------------------------------------------------------------------
  vdc_b : i8244_top_sync
    generic map (
      is_pal_g => is_pal_g
    )
    port map (
      clk_i      => clk_i,
      clk_en_i   => clk_vdc_en_i,
      res_n_i    => res_n_i,
      intr_n_o   => int_n_s,
      stb_i      => gnd_s,
      bg_o       => open,
      hsync_o    => hsync_s,
      vsync_o    => vsync_s,
      ms_i       => vdd_s,
      hbl_o      => hbl_s,
      vbl_i      => gnd_s,
      vbl_o      => vbl_s,
      cx_i       => gnd_s,
      l_o        => l_from_vdc_s,
      cs_n_i     => vdc_cs_n_s,
      wr_n_i     => vdc_wr_n_s,
      rd_n_i     => vdc_rd_n_s,
      din_i      => db_s,
      dout_o     => d_from_vdc_s,
      dout_en_o  => open,
      r_o        => r_o,
      g_o        => g_o,
      b_o        => b_o,
      ale_i      => ale_s,
      snd_o      => snd_o,
      snd_vec_o  => snd_vec_o
    );
  --
  hsync_n_o <= not hsync_s;
  vsync_n_o <= not vsync_s;
  hbl_o     <= hbl_s;
  vbl_o     <= vbl_s;


  -- pragma translate_off
  tb_clk_s         <= clk_i;
  tb_ale_s         <= ale_s;
  tb_vdc_cs_n_s    <= vdc_cs_n_s;
  tb_vdc_wr_n_s    <= vdc_wr_n_s;
  tb_vdc_rd_n_s    <= vdc_rd_n_s;
  tb_db_from_cpu_s <= db_s;
  tb_d_from_vdc_s  <= d_from_vdc_s;
  tb_vbl_s         <= vbl_s;
  -- pragma translate_on


  -----------------------------------------------------------------------------
  -- 128 bytes RAM
  -----------------------------------------------------------------------------
  ram_wr_s <= ram_cs_s and not ram_cs_n_s and not ram_wr_n_s;
  --
  ram_128_b : generic_ram_ena
    generic map (
      addr_width_g => 7,
      data_width_g => 8
    )
    port map (
      clk_i => clk_i,
      a_i   => a_s(6 downto 0),
      we_i  => ram_wr_s,
      ena_i => vdd_s,
      d_i   => db_s,
      d_o   => d_from_ram_s
    );


  -----------------------------------------------------------------------------
  -- Read MUX
  -----------------------------------------------------------------------------
  db_to_cpu_s <= d_from_vdc_s when vdc_cs_n_s = '0' and vdc_rd_n_s = '0' else
                 d_from_ram_s when ram_cs_s = '1' and ram_cs_n_s = '0'   else
                 joys_s(0)    when rd_n_s = '0' and keyb_dec_s(2) = '0'  else
                 joys_s(1)    when rd_n_s = '0' and keyb_dec_s(1) = '0'  else
                 cart_d_i;
  db_s <=   db_from_cpu_s
          when db_dir_s = '1' else
            db_to_cpu_s;


  -----------------------------------------------------------------------------
  -- Process keyb
  --
  -- Purpose:
  --   Decodes and encodes the keyboard lines.
  --
  keyb: process (p1_from_cpu_s,
                 p2_from_cpu_s,
                 keyb_enc_i,
                 keyb_dec_s)
    variable keyb_idx_v : natural range 0 to 7;
    variable keyb_dec_v : std_logic_vector(7 downto 0);
  begin
    -- the following logic implements the 74ls156 decoder chip
    keyb_dec_v := (others => '1');
    keyb_idx_v := to_integer(unsigned(p2_from_cpu_s(2 downto 0)));
    if p1_from_cpu_s(2) = '0' then
      keyb_dec_v(keyb_idx_v) := '0';
    end if;
    -- assign signal and output port
    keyb_dec_s <= keyb_dec_v(5 downto 0);
    keyb_dec_o <= keyb_dec_s;

    -- the following logic implements the 74ls148 encoder chip
    p2_to_cpu_s <= (others => '1');
    for idx in 0 to 7 loop
      if keyb_enc_i(14 - idx) = '0' then
        p2_to_cpu_s(7 downto 5) <= std_logic_vector(to_unsigned(idx, 3));
        p2_to_cpu_s(4) <= '0';
        exit;
      end if;
    end loop;
  end process keyb;
  --
  -----------------------------------------------------------------------------


  -----------------------------------------------------------------------------
  -- Joystick remapping
  -----------------------------------------------------------------------------
  joy: for num in 0 to 1 generate
    joys_s(num) <= (0 => joy_up_n_i(num),
                    1 => joy_right_n_i(num),
                    2 => joy_down_n_i(num),
                    3 => joy_left_n_i(num),
                    4 => joy_action_n_i(num),
                    others => '1');
  end generate;

end struct;
