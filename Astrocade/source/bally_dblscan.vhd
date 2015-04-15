--
-- A simulation model of Bally Astrocade hardware
-- Copyright (c) MikeJ - Nov 2004
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
-- Email support@fpgaarcade.com
--
-- Revision list
--
-- version 003 spartan3e release
-- version 001 initial release
--
--
-- I was in a bit of a hurry, so I have bolted the old simple scan doubler onto a vga
-- sync generator which runs at the same frame rate as the NTSC scan generator in the
-- Data chip. I_FPSYNC is high for the first active pixel in the frame, and I use this
-- to sync the two together. I then have offsets to get the actual picture centered.
--
-- This could be simplified, but for now I want the flexibilty.
--

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_unsigned.all;
  use ieee.numeric_std.all;

library UNISIM;
  use UNISIM.Vcomponents.all;

entity BALLY_DBLSCAN is
  port (
	I_R               : in    std_logic_vector( 3 downto 0);
	I_G               : in    std_logic_vector( 3 downto 0);
	I_B               : in    std_logic_vector( 3 downto 0);
	I_HSYNC           : in    std_logic;
	I_VSYNC           : in    std_logic;
	--
	I_FPSYNC          : in    std_logic; -- first pixel, field 1
	--
	O_R               : out   std_logic_vector( 3 downto 0);
	O_G               : out   std_logic_vector( 3 downto 0);
	O_B               : out   std_logic_vector( 3 downto 0);
	O_HSYNC           : out   std_logic;
	O_VSYNC           : out   std_logic;
	--
	I_RESET           : in    std_logic;
	ENA_X2            : in    std_logic;
	ENA               : in    std_logic;
	CLK               : in    std_logic;
	ENA_SCANLINES     : in    std_logic
	);
end;

architecture RTL of BALLY_DBLSCAN is
  --
  --types & constants
  --
  subtype  Bus12         is std_logic_vector (11 downto 0);
  -- 14.2857 MHz
  constant V_FRONT_PORCH_START  : Bus12 := x"1E2"; -- line 480 + 2
  constant V_SYNC_START         : Bus12 := x"1EB"; -- line 491
  constant V_BACK_PORCH_START   : Bus12 := x"1ED"; -- line 493
  constant LINE_PER_FRAME       : Bus12 := x"20D"; -- 525 lines

  constant H_FRONT_PORCH_START  : Bus12 := x"16c"; -- 364
  constant H_SYNC_START         : Bus12 := x"174"; -- 372
  constant H_BACK_PORCH_START   : Bus12 := x"1ac"; -- 428
  constant PIXEL_PER_LINE       : Bus12 := x"1c7"; -- 455 -- 454 would be better with the clock we have
  constant HOFFSET              : Bus12 := x"030"; -- input to output hphase ...
  constant HPOS_O_OFFSET        : Bus12 := x"042"; -- where does the first pixel land in the input buffer?
  --
  -- input timing
  --
  signal hsync_in_t1            : std_logic;
  signal vsync_in_t1            : std_logic;
  signal hpos_i                 : std_logic_vector(10 downto 0) := (others => '0');    -- input capture postion
  signal bank                   : std_logic_vector(1 downto 0);
  signal o_bank                 : std_logic_vector(1 downto 0);
  signal o_bank_t1              : std_logic_vector(1 downto 0);
  signal we_a                   : std_logic;
  signal re_a_x2                : std_logic;
  signal we_b                   : std_logic;
  signal re_b_x2                : std_logic;
  signal rgb_in                 : std_logic_vector(15 downto 0);
  --
  -- output timing
  --
  signal field2                 : std_logic;
  signal hpos_o                 : std_logic_vector(10 downto 0) := (others => '0');
  ----
  signal rgb_out_a              : std_logic_vector(15 downto 0);
  signal rgb_out_b              : std_logic_vector(15 downto 0);

  signal line_count             : std_logic_vector(9 downto 0);
  signal pixel_count            : std_logic_vector(10 downto 0);
  signal hterm                  : boolean;
  signal vterm                  : boolean;
  signal v_sync                 : std_logic;
  signal h_sync                 : std_logic;

  signal vertical_blanking      : std_logic;
  signal horizontal_blanking    : std_logic;
  signal active_video           : std_logic;
  signal active_video_t1        : std_logic;
  signal active_video_t2        : std_logic;

  signal h_sync_t1              : std_logic;
  signal h_sync_t2              : std_logic;
  signal v_sync_t1              : std_logic;
  signal v_sync_t2              : std_logic;

begin

  p_input_timing : process
	variable rising_h : boolean;
	variable rising_v : boolean;
  begin
	wait until rising_edge (CLK);
	if (ENA   = '1') then
	  hsync_in_t1 <= I_HSYNC;
	  vsync_in_t1 <= I_VSYNC;

	  rising_h := (I_HSYNC = '1') and (hsync_in_t1 = '0');
	  rising_v := (I_VSYNC = '1') and (vsync_in_t1 = '0');

	  if rising_v then
		bank <= (others => '0');
	  elsif rising_h then
		bank <= bank + "1";
	  end if;

	  if rising_h then
		hpos_i <= (others => '0');
	  else
		hpos_i <= hpos_i + "1";
	  end if;

	end if;
  end process;

  we_a    <= not bank(1);
  we_b    <=     bank(1);
  re_a_x2 <= not o_bank(1) and ENA_X2;
  re_b_x2 <=     o_bank(1) and ENA_X2;

  rgb_in <= "0000" & I_B & I_G & I_R;

  u_ram_a : RAMB16_S18_S18
	port map (
	  -- output
	  DOPB  => open,
	  DOB   => rgb_out_a,
	  DIPB  => "00",
	  DIB   => x"0000",
	  ADDRB(9)          => o_bank(0),
	  ADDRB(8 downto 0) => hpos_o(8 downto 0),
	  WEB   => '0',
	  ENB   => re_a_x2,
	  SSRB  => '0',
	  CLKB  => CLK,

	  -- input
	  DOPA  => open,
	  DOA   => open,
	  DIPA   => "00",
	  DIA   => rgb_in,
	  ADDRA(9)          => bank(0),
	  ADDRA(8 downto 0) => hpos_i(8 downto 0),
	  WEA   => we_a,
	  ENA   => ENA,
	  SSRA  => '0',
	  CLKA  => CLK
	  );

  u_ram_b : RAMB16_S18_S18
	port map (
	  -- output
	  DOPB  => open,
	  DOB   => rgb_out_b,
	  DIPB  => "00",
	  DIB   => x"0000",
	  ADDRB(9)          => o_bank(0),
	  ADDRB(8 downto 0) => hpos_o(8 downto 0),
	  WEB   => '0',
	  ENB   => re_b_x2,
	  SSRB  => '0',
	  CLKB  => CLK,

	  -- input
	  DOPA  => open,
	  DOA   => open,
	  DIPA   => "00",
	  DIA   => rgb_in,
	  ADDRA(9)          => bank(0),
	  ADDRA(8 downto 0) => hpos_i(8 downto 0),
	  WEA   => we_b,
	  ENA   => ENA,
	  SSRA  => '0',
	  CLKA  => CLK
	  );

  -- line count 0, pixel count 0 = first pixel of active video
  p_cnt_compare_comb   : process(pixel_count, line_count)
  begin
	hterm <= (pixel_count = (PIXEL_PER_LINE(10 downto 0) - "1"));
	vterm <= (line_count = (LINE_PER_FRAME(9 downto 0) - "1"));
  end process;

  p_hv_cnt             : process(CLK, I_RESET)
  begin
	if (I_RESET = '1') then
	  pixel_count <= (others => '0');
	  line_count  <= (others => '0');
	elsif rising_edge(CLK) then
	  if (ENA_X2 = '1') then
		if (I_FPSYNC = '1') and (ENA = '1') then -- sync up
		  pixel_count <= HOFFSET(10 downto 0);
		  line_count  <= (LINE_PER_FRAME(9 downto 0) - "100");
		  field2 <= '1'; -- about to be field 1
		else
		  if hterm then
			pixel_count <= (others => '0');
		  else
			pixel_count <= pixel_count + "1";
		  end if;

		  if hterm then
			if vterm then
			  field2 <= not field2;
			  line_count <= (others => '0');
			else
			  line_count <= line_count + "1";
			end if;
		  end if;
		end if;
	  end if;
	end if;
  end process;

  p_vertical_sync      : process(CLK, I_RESET)
	variable vcnt_eq_front_porch_start : boolean;
	variable vcnt_eq_sync_start        : boolean;
	variable vcnt_eq_back_porch_start  : boolean;
	variable hterm_m8 : boolean;
  begin
	if (I_RESET = '1') then
	  v_sync <= '1';
	  vertical_blanking <= '0';
	elsif rising_edge(CLK) then
	  if (ENA_X2 = '1') then
		vcnt_eq_front_porch_start := (line_count = (V_FRONT_PORCH_START(9 downto 0) - "1"));
		vcnt_eq_sync_start        := (line_count = (       V_SYNC_START(9 downto 0) - "1"));
		vcnt_eq_back_porch_start  := (line_count = ( V_BACK_PORCH_START(9 downto 0) - "1"));

		hterm_m8 := (pixel_count = (PIXEL_PER_LINE(10 downto 0) - "1001"));

		if vcnt_eq_sync_start and hterm then
		  v_sync <= '0';
		elsif vcnt_eq_back_porch_start and hterm then
		  v_sync <= '1';
		end if;

		if vcnt_eq_front_porch_start and hterm then
		  vertical_blanking <= '1';
		elsif vterm and hterm then
		  vertical_blanking <= '0';
		end if;

	  end if;
	end if;
  end process;

  p_horizontal_sync    : process(CLK, I_RESET)
	variable hcnt_eq_front_porch_start_m2  : boolean;
	variable hcnt_eq_front_porch_start     : boolean;
	variable hcnt_eq_sync_start            : boolean;
	variable hcnt_eq_back_porch_start      : boolean;
	variable hterm_m2 : boolean;
  begin
	if (I_RESET = '1') then
	  h_sync <= '1';
	  horizontal_blanking <= '0';
	elsif rising_edge(CLK) then
	  if (ENA_X2 = '1') then
		hcnt_eq_front_porch_start_m2  := (pixel_count = ( H_FRONT_PORCH_START(10 downto 0) - "0011"));
		hcnt_eq_front_porch_start     := (pixel_count = ( H_FRONT_PORCH_START(10 downto 0) - "1"));
		hcnt_eq_sync_start            := (pixel_count = (        H_SYNC_START(10 downto 0) - "1"));
		hcnt_eq_back_porch_start      := (pixel_count = (  H_BACK_PORCH_START(10 downto 0) - "1"));

		hterm_m2 := (pixel_count = (PIXEL_PER_LINE(10 downto 0) - "0011"));

		if hcnt_eq_sync_start then
		  h_sync <= '0';
		elsif hcnt_eq_back_porch_start then
		  h_sync <= '1';
		end if;

		if hcnt_eq_front_porch_start then
		  horizontal_blanking <= '1';
		elsif hterm then
		  horizontal_blanking <= '0';
		end if;

	  end if;
	end if;
  end process;

  p_active_video_comb  : process(horizontal_blanking, vertical_blanking)
  begin
	active_video <= not(horizontal_blanking or vertical_blanking);
  end process;

  p_shifter            : process
  begin
	wait until rising_edge(CLK);
	if (ENA_X2 = '1') then
	hpos_o <= pixel_count + HPOS_O_OFFSET(10 downto 0);
	o_bank_t1 <= o_bank;

	h_sync_t1 <= h_sync;
	h_sync_t2 <= h_sync_t1;
	v_sync_t1 <= v_sync;
	v_sync_t2 <= v_sync_t1;
	active_video_t1 <= active_video;
	active_video_t2 <= active_video_t1;
	end if;
  end process;

  p_obank : process(bank, field2, line_count)
  begin
	if field2 = '0' then
	  o_bank <= bank - "10";
	else
	  if (line_count(0) = '0') then
		o_bank <= bank - "01";
	  else
		o_bank <= bank - "10";
	  end if;
	end if;

  end process;

  p_video_mux          : process(CLK, I_RESET)
	variable video_r                : std_logic_vector(3 downto 0);
	variable video_g                : std_logic_vector(3 downto 0);
	variable video_b                : std_logic_vector(3 downto 0);
  begin
	if (I_RESET = '1') then
	  O_R <= x"0";
	  O_G <= x"0";
	  O_B <= x"0";

	  O_VSYNC <= '1';
	  O_HSYNC <= '1';

	elsif rising_edge(CLK) then
	  if (o_bank_t1(1) = '1') then
		video_b := rgb_out_b(11 downto 8);
		video_g := rgb_out_b( 7 downto 4);
		video_R := rgb_out_b( 3 downto 0);
	  else
		video_b := rgb_out_a(11 downto 8);
		video_g := rgb_out_a( 7 downto 4);
		video_r := rgb_out_a( 3 downto 0);
	  end if;


	  if (ENA_X2 = '1') then
		if (active_video_t2 = '1') then
      if ENA_SCANLINES = '0' or line_count(0) = '1' then
        O_R <= video_r;
        O_G <= video_g;
        O_B <= video_b;
      else
        O_R <= '0' & video_r(3 downto 1);
        O_G <= '0' & video_g(3 downto 1);
        O_B <= '0' & video_b(3 downto 1);
      end if;
		else
		  O_R <= x"0";
		  O_G <= x"0";
		  O_B <= x"0";
		end if;

		O_VSYNC <= v_sync_t2;
		O_HSYNC <= h_sync_t2;
	  end if;
	end if;
  end process;

end architecture RTL;

