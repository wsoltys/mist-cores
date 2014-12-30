--
-- Adapted for FPGA Videopac by A. Laeuger, 30-Dec-2006
--
-- Based on
--
-- A simulation model of Pacman hardware
-- VHDL conversion by MikeJ - October 2002
--
-- FPGA PACMAN video scan doubler
--
-- based on a design by Tatsuyuki Satoh
--
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
-- THIS CODE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
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
-- You are responsible for any legal issues arising from your use of this code.
--
-- The latest version of this file can be found at: www.fpgaarcade.com
--
-- Email pacman@fpgaarcade.com
--
-- Revision list
--
-- version 002 initial release
--
library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

entity DBLSCAN is
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
end;


use work.tech_comp_pack.dpram;

architecture RTL of DBLSCAN is

  -- VGA Mode3 timing
  constant vga3_pix_c     : real    := 40.0;  -- pixel duration (ns)
  constant vga3_hs_c      : real    := 96.0;  -- pixel per sync
  constant vga3_hbp_c     : real    := 48.0;  -- pixel per back porch
  constant vga3_hdisp_c   : real    := 640.0; -- pixel per display
  constant vga3_hfp_c     : real    := 16.0;  -- pixel per front porch
  constant vga3_num_pix_c : real    := vga3_hs_c + vga3_hbp_c + vga3_hdisp_c +
                                       vga3_hfp_c;

  -- specific timing
  constant dbl_pix_c      : real    := 70.0;  -- pixel duration (ns)
  constant ratio_c        : real    := vga3_pix_c / dbl_pix_c;
  constant dbl_hs_c       : natural := integer(vga3_hs_c * ratio_c) - 2;
  constant dbl_hbp_c      : natural := integer(vga3_hbp_c * ratio_c);
  constant dbl_hdisp_c    : natural := integer(vga3_hdisp_c * ratio_c);
  constant dbl_hfp_c      : natural := integer(vga3_hfp_c * ratio_c);
--  constant dbl_num_pix_c  : natural := integer(vga3_num_pix_c * ratio_c);
  constant dbl_num_pix_c  : natural := dbl_hs_c + dbl_hbp_c + dbl_hdisp_c +
                                       dbl_hfp_c;
  -- correction value to center visible picture portion
  constant dbl_adjust_c   : natural := 22;

  --
  -- input timing
  --
  signal hsync_in_t1 : std_logic;
  signal vsync_in_t1 : std_logic;
  signal hpos_i : std_logic_vector(8 downto 0) := (others => '0');    -- input capture postion
  signal ibank : std_logic;
  signal we_a : std_logic;
  signal we_b : std_logic;
  signal rgb_in,
         rgb_out_a,
         rgb_out_b  : std_logic_vector(3 downto 0);
  --
  -- output timing
  --
  signal hpos_o : unsigned(8 downto 0) := (others => '0');
  signal hpos_o_adj : std_logic_vector(8 downto 0) := (others => '0');
  signal ohs : std_logic;
  signal ohs_t1 : std_logic;
  signal ovs : std_logic;
  signal ovs_t1 : std_logic;
  signal obank : std_logic;
  signal obank_t1 : std_logic;
  --
  signal vs_cnt : std_logic_vector(2 downto 0);

  constant pos_rgb_r_c : natural := 0;
  constant pos_rgb_g_c : natural := 1;
  constant pos_rgb_b_c : natural := 2;
  constant pos_rgb_l_c : natural := 3;

begin

  p_input_timing : process(CLK_RGB, RESET_N_I)
    variable rising_h : boolean;
    variable rising_v : boolean;
  begin
    if RESET_N_I = '0' then
      hsync_in_t1 <= '0';
      vsync_in_t1 <= '0';
      ibank       <= '0';
      hpos_i      <= (others => '0');

    elsif rising_edge (CLK_RGB) then
      if CLK_EN_RGB = '1' then
        hsync_in_t1 <= HSYNC_IN;
        vsync_in_t1 <= VSYNC_IN;

        rising_h := (HSYNC_IN = '1') and (hsync_in_t1 = '0');
        rising_v := (VSYNC_IN = '1') and (vsync_in_t1 = '0');

        if rising_v then
          ibank <= '0';
        elsif rising_h then
          ibank <= not ibank;
        end if;

        if rising_h then
          hpos_i <= (others => '0');
        else
          hpos_i <= std_logic_vector(unsigned(hpos_i) + 1);
        end if;
      end if;

    end if;

  end process;

  we_a <=     ibank and CLK_EN_RGB;
  we_b <= not ibank and CLK_EN_RGB;
  rgb_in <= (pos_rgb_l_c => RGB_L_IN,
             pos_rgb_b_c => RGB_B_IN,
             pos_rgb_g_c => RGB_G_IN,
             pos_rgb_r_c => RGB_R_IN);

  u_ram_a : dpram
    generic map (
      addr_width_g => 9,
      data_width_g => 4
    )
    port map (
      clk_a_i  => CLK_RGB,
      we_i     => we_a,
      addr_a_i => hpos_i,
      data_a_i => rgb_in,
      data_a_o => open,
      clk_b_i  => CLK_VGA,
      addr_b_i => hpos_o_adj,
      data_b_o => rgb_out_a
    );
  u_ram_b : dpram
    generic map (
      addr_width_g => 9,
      data_width_g => 4
    )
    port map (
      clk_a_i  => CLK_RGB,
      we_i     => we_b,
      addr_a_i => hpos_i,
      data_a_i => rgb_in,
      data_a_o => open,
      clk_b_i  => CLK_VGA,
      addr_b_i => hpos_o_adj,
      data_b_o => rgb_out_b
    );


  p_output_timing : process(CLK_VGA, RESET_N_I)
    variable rising_h : boolean;
  begin
    if RESET_N_I = '0' then
      hpos_o <= (others => '0');
      obank  <= '0';
      vs_cnt <= "000";
      ohs    <= '0';
      ohs_t1 <= '0';
      ovs    <= '0';
      ovs_t1 <= '0';

    elsif rising_edge(CLK_VGA) then
      if CLK_EN_VGA = '1' then
        rising_h := ((ohs = '1') and (ohs_t1 = '0'));

        if rising_h or hpos_o = dbl_num_pix_c-1 then
          hpos_o <= (others => '0');
        else
          hpos_o <= hpos_o + 1;
        end if;

        if (ovs = '1') and (ovs_t1 = '0') then -- rising_v
          obank <= '0';
          vs_cnt <= "000";
        elsif rising_h then
          obank <= not obank;
          if (vs_cnt(2) = '0') then
            vs_cnt <= std_logic_vector(unsigned(vs_cnt) + 1);
          end if;
        end if;

        ohs <= HSYNC_IN; -- reg on CLK_VGA
        ohs_t1 <= ohs;

        ovs <= VSYNC_IN; -- reg on CLK_VGA
        ovs_t1 <= ovs;
      end if;

    end if;
  end process;

  -- adjust hpos_o in a way that the RGB picture is centered
  hpos_o_adj <= std_logic_vector(hpos_o - dbl_adjust_c);

  p_op : process(CLK_VGA, RESET_N_I)
  begin
    if RESET_N_I = '0' then
      HSYNC_OUT <= '0';
      BLANK_OUT <= '0';
      obank_t1  <= '0';
      VGA_R_OUT <= '0';
      VGA_G_OUT <= '0';
      VGA_B_OUT <= '0';
      VGA_L_OUT <= '0';
      VSYNC_OUT <= '0';

    elsif rising_edge(CLK_VGA) then
      if CLK_EN_VGA = '1' then

        HSYNC_OUT <= '0';
        if hpos_o < dbl_hs_c then
          HSYNC_OUT <= '1';
        end if;

        BLANK_OUT <= '0';
        if hpos_o < dbl_hs_c + dbl_hbp_c + dbl_adjust_c or
           hpos_o > dbl_hs_c + dbl_hbp_c + dbl_hdisp_c - dbl_adjust_c then
          BLANK_OUT <= '1';
        end if;

        obank_t1 <= obank;
        if (obank_t1 = '1') then
          VGA_R_OUT <= rgb_out_b(pos_rgb_r_c);
          VGA_G_OUT <= rgb_out_b(pos_rgb_g_c);
          VGA_B_OUT <= rgb_out_b(pos_rgb_b_c);
          VGA_L_OUT <= rgb_out_b(pos_rgb_l_c);
        else
          VGA_R_OUT <= rgb_out_a(pos_rgb_r_c);
          VGA_G_OUT <= rgb_out_a(pos_rgb_g_c);
          VGA_B_OUT <= rgb_out_a(pos_rgb_b_c);
          VGA_L_OUT <= rgb_out_a(pos_rgb_l_c);
        end if;

        VSYNC_OUT <= not vs_cnt(2);
      end if;

    end if;
  end process;

end RTL;
