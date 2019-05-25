--
-- mist_cv.vhd
--
-- colecovision toplevel for the MiST board
-- This file is a *derivative* work of the source cited below and from pacedev.net
-- The original source can be downloaded from <http://www.fpgaarcade.com>
-- https://github.com/wsoltys/mist-cores
--
-------------------------------------------------------------------------------
--
-- FPGA Colecovision
--
-- $Id: cv_console.vhd,v 1.13 2006/02/28 22:29:55 arnim Exp $
--
-- Toplevel of the Colecovision console
--
-- References:
--
--   * Dan Boris' schematics of the Colecovision board
--     http://www.atarihq.com/danb/files/colecovision.pdf
--
--   * Schematics of the Colecovision controller, same source
--     http://www.atarihq.com/danb/files/ColecoController.pdf
--
--   * Technical information, same source
--     http://www.atarihq.com/danb/files/CV-Tech.txt
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

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.cv_keys_pack.all;
use work.vdp18_col_pack.all;

entity mist_cv is

  port (
    -- Clocks
    
    CLOCK_27    : in std_logic_vector(1 downto 0); -- 27 MHz


    -- SDRAM
    SDRAM_nCS : out std_logic; -- Chip Select
    SDRAM_DQ : inout std_logic_vector(15 downto 0); -- SDRAM Data bus 16 Bits
    SDRAM_A : out std_logic_vector(12 downto 0); -- SDRAM Address bus 13 Bits
    SDRAM_DQMH : out std_logic; -- SDRAM High Data Mask
    SDRAM_DQML : out std_logic; -- SDRAM Low-byte Data Mask
    SDRAM_nWE : out std_logic; -- SDRAM Write Enable
    SDRAM_nCAS : out std_logic; -- SDRAM Column Address Strobe
    SDRAM_nRAS : out std_logic; -- SDRAM Row Address Strobe
    SDRAM_BA : out std_logic_vector(1 downto 0); -- SDRAM Bank Address
    SDRAM_CLK : out std_logic; -- SDRAM Clock
    SDRAM_CKE: out std_logic; -- SDRAM Clock Enable
    
    -- SPI
    SPI_SCK : in std_logic;
    SPI_DI : in std_logic;
    SPI_DO : out std_logic;
    SPI_SS2 : in std_logic;
    SPI_SS3 : in std_logic;
    CONF_DATA0 : in std_logic;

    -- VGA output
    

    VGA_HS,                                             -- H_SYNC
    VGA_VS : out std_logic;                             -- V_SYNC
    VGA_R,                                              -- Red[5:0]
    VGA_G,                                              -- Green[5:0]
    VGA_B : out std_logic_vector(5 downto 0);           -- Blue[5:0]
    
    -- Audio
    AUDIO_L,
    AUDIO_R : out std_logic;
    
    -- LEDG
    LED : out std_logic

    );
  
end mist_cv;

architecture rtl of mist_cv is

  constant CONF_STR : string := "COLECO;COLBINROM;"&
                                "F,SG ,Load;"&
                                "O45,RAM Size,1k,8k,SGM;"&
                                "O7,Exp mod. 2,Off,On;"&
                                "O6,Joystick swap,Off,On;"&
                                "O23,Scanlines,Off,25%,50%,75%;"&
                                "T0,Reset;";

  function to_slv(s: string) return std_logic_vector is 
    constant ss: string(1 to s'length) := s; 
    variable rval: std_logic_vector(1 to 8 * s'length); 
    variable p: integer; 
    variable c: integer; 
  
  begin 
    for i in ss'range loop
      p := 8 * i;
      c := character'pos(ss(i));
      rval(p - 7 to p) := std_logic_vector(to_unsigned(c,8)); 
    end loop; 
    return rval; 

  end function; 


component user_io
        generic ( STRLEN : integer := 0 );
   port (
        clk_sys : in std_logic;
        clk_sd  : in std_logic;
        SPI_CLK, SPI_SS_IO, SPI_MOSI :in std_logic;
        SPI_MISO : out std_logic;
        conf_str : in std_logic_vector(8*STRLEN-1 downto 0);
        joystick_0 : out std_logic_vector(31 downto 0);
        joystick_1 : out std_logic_vector(31 downto 0);
        joystick_analog_0 : out std_logic_vector(15 downto 0);
        joystick_analog_1 : out std_logic_vector(15 downto 0);
        status: out std_logic_vector(31 downto 0);
        switches : out std_logic_vector(1 downto 0);
        buttons : out std_logic_vector(1 downto 0);
        scandoubler_disable: out std_logic;
        ypbpr: out std_logic;
        ps2_kbd_clk : out std_logic;
        ps2_kbd_data : out std_logic;
        ps2_mouse_clk : out std_logic;
        ps2_mouse_data : out std_logic
      );
  end component user_io;
  
  component data_io is
    port (
        sck         : in std_logic;
        ss          : in std_logic;
        sdi         : in std_logic;
        downloading : out std_logic;
        index       : out std_logic_vector(7 downto 0);
        clk         : in std_logic;
        clkref      : in std_logic;
        wr          : out std_logic;
        a           : out std_logic_vector(24 downto 0);
        d           : out std_logic_vector(7 downto 0)
    );
  end component data_io;

component sdram
    port (
        SDRAM_DQ    : inout std_logic_vector(15 downto 0);
        SDRAM_A     : out std_logic_vector(12 downto 0);
        SDRAM_DQML  : out std_logic;
        SDRAM_DQMH  : out std_logic;
        SDRAM_BA    : out std_logic_vector(1 downto 0);
        SDRAM_nCS   : out std_logic;
        SDRAM_nWE   : out std_logic;
        SDRAM_nRAS  : out std_logic;
        SDRAM_nCAS  : out std_logic;
        SDRAM_CKE   : out std_logic;

        init        : in std_logic;
        clk         : in std_logic;
        wtbt        : in std_logic_vector(1 downto 0);

        addr        : in std_logic_vector(24 downto 0);
        rd          : in std_logic;
        dout        : out std_logic_vector(7 downto 0);
        din         : in std_logic_vector(7 downto 0);
        we          : in std_logic;
        ready       : out std_logic
    );
end component sdram;

component scandoubler
    port (
            clk_sys     : in std_logic;
            scanlines   : in std_logic_vector(1 downto 0);
    
            hs_in       : in std_logic;
            vs_in       : in std_logic;
            r_in        : in std_logic_vector(5 downto 0);
            g_in        : in std_logic_vector(5 downto 0);
            b_in        : in std_logic_vector(5 downto 0);
  
            hs_out      : out std_logic;
            vs_out      : out std_logic;
            r_out       : out std_logic_vector(5 downto 0);
            g_out       : out std_logic_vector(5 downto 0);
            b_out       : out std_logic_vector(5 downto 0)
        );
end component scandoubler;
  
component osd
         generic ( OSD_COLOR : integer := 1 );  -- blue
    port (  clk_sys     : in std_logic;

            R_in        : in std_logic_vector(5 downto 0);
            G_in        : in std_logic_vector(5 downto 0);
            B_in        : in std_logic_vector(5 downto 0);
            HSync       : in std_logic;
            VSync       : in std_logic;

            R_out       : out std_logic_vector(5 downto 0);
            G_out       : out std_logic_vector(5 downto 0);
            B_out       : out std_logic_vector(5 downto 0);

            SPI_SCK     : in std_logic;
            SPI_SS3     : in std_logic;
            SPI_DI      : in std_logic
        );
    end component osd;

COMPONENT rgb2ypbpr
        PORT
        (
        red     :        IN std_logic_vector(5 DOWNTO 0);
        green   :        IN std_logic_vector(5 DOWNTO 0);
        blue    :        IN std_logic_vector(5 DOWNTO 0);
        y       :        OUT std_logic_vector(5 DOWNTO 0);
        pb      :        OUT std_logic_vector(5 DOWNTO 0);
        pr      :        OUT std_logic_vector(5 DOWNTO 0)
        );
END COMPONENT;

  signal clk21m3 : std_logic;
  signal clkref  : std_logic;
  signal rom_en  : std_logic;
  signal force_reset : std_logic := '0';
  signal reset_n_s : std_logic;
  
  signal switches   : std_logic_vector(1 downto 0);
  signal buttons    : std_logic_vector(1 downto 0);
  signal joy        : std_logic_vector(5 downto 0);
  signal joy0       : std_logic_vector(31 downto 0);
  signal joy1       : std_logic_vector(31 downto 0);
  signal joy_an0    : std_logic_vector(15 downto 0);
  signal joy_an1    : std_logic_vector(15 downto 0);
  signal joy_an     : std_logic_vector(15 downto 0);
  signal status     : std_logic_vector(31 downto 0);
  signal scandoubler_disable : std_logic;
  signal ypbpr      : std_logic;
  signal ps2Clk     : std_logic;
  signal ps2Data    : std_logic;
  signal audio      : std_logic;
  signal pll_locked : std_logic;
  signal joya       : std_logic_vector(7 downto 0);
  signal joyb       : std_logic_vector(7 downto 0);

  signal coleco_red      : std_logic_vector(7 downto 0);
  signal coleco_green    : std_logic_vector(7 downto 0);
  signal coleco_blue     : std_logic_vector(7 downto 0);
  signal coleco_hs       : std_logic;
  signal coleco_vs       : std_logic;
    
  signal sd_r         : std_logic_vector(5 downto 0);
  signal sd_g         : std_logic_vector(5 downto 0);
  signal sd_b         : std_logic_vector(5 downto 0);
  signal sd_hs        : std_logic;
  signal sd_vs        : std_logic;

  signal osd_red_i    : std_logic_vector(5 downto 0);
  signal osd_green_i  : std_logic_vector(5 downto 0);
  signal osd_blue_i   : std_logic_vector(5 downto 0);
  signal osd_vs_i     : std_logic;
  signal osd_hs_i     : std_logic;
  signal osd_red_o : std_logic_vector(5 downto 0);
  signal osd_green_o : std_logic_vector(5 downto 0);
  signal osd_blue_o : std_logic_vector(5 downto 0);
  signal vga_y_o : std_logic_vector(5 downto 0);
  signal vga_pb_o : std_logic_vector(5 downto 0);
  signal vga_pr_o : std_logic_vector(5 downto 0);  
  
  signal index          : std_logic_vector(7 downto 0);
  signal downl          : std_logic := '0';
  signal old_downl      : std_logic;
  signal cart_a         : std_logic_vector(24 downto 0);
  signal cart_d         : std_logic_vector(7 downto 0);
  signal chksum         : std_logic_vector(7 downto 0);
  
  signal clk_cnt_q            : unsigned(1 downto 0);
	signal clk_en_5m37_q			  : std_logic;
	signal clk_21m3_s					  : std_logic;
  signal clk_mem_s      : std_logic;
  signal clk_mem_cnt    : unsigned(2 downto 0);
  signal clk_en_10m7_q			  : std_logic;
  signal clk_en_spinner_counter_s : unsigned(15 downto 0);
  signal clk_en_spinner_s   : std_logic;
  signal por_n_s              : std_logic;
  
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

  signal ram_a_s             : std_logic_vector(14 downto 0);
  signal cpu_ram_a_s         : std_logic_vector(14 downto 0);
  signal cpu_ram_ce_n_s      : std_logic;
  signal cpu_ram_we_n_s      : std_logic;
  signal cpu_ram_d_to_cv_s,
         cpu_ram_d_from_cv_s : std_logic_vector( 7 downto 0);
  signal cpu_ram_we_s        : std_logic;

  signal vram_a_s            : std_logic_vector(13 downto 0);
  signal vram_we_s           : std_logic;
  signal vram_d_to_cv_s,
         vram_d_from_cv_s    : std_logic_vector( 7 downto 0);

  signal cart_a_s            : std_logic_vector(24 downto 0);
  signal cart_d_s            : std_logic_vector( 7 downto 0);
  signal cart_en_80_n_s,
         cart_en_a0_n_s,
         cart_en_c0_n_s,
         cart_en_e0_n_s,
         cart_en_sg1000_n_s: std_logic;
         
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

  signal unsigned_audio_s    : unsigned(10 downto 0);
  signal audio_s             : std_logic;
  
  signal ps2_keys_s				    : std_logic_vector(15 downto 0);
	signal ps2_joy_s				    : std_logic_vector(15 downto 0);

  signal romwr_a            : std_logic_vector(24 downto 0);
  signal ioctl_dout         : std_logic_vector(7 downto 0);
  signal rom_wr             : std_logic;
  signal sd_wrack           : std_logic;
  signal ram_ready          : std_logic;
  signal sg1000             : std_logic;
  signal dahjeeA            : std_logic;
  signal sg1000_row         : std_logic_vector(2 downto 0);
  signal sg1000_col         : std_logic_vector(11 downto 0);

begin

  LED <= not downl;
  reset_n_s <= not(status(0) or buttons(1) or force_reset or not pll_locked);

  pll : entity work.mist_pll
    port map (
      inclk0 => CLOCK_27(0),
      c0     => clk_21m3_s,
      c1     => clk_mem_s,
      locked => pll_locked
      );
      
  SDRAM_CLK <= not clk_mem_s;
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
      clk_en_spinner_s <=  '0';
      clk_en_spinner_counter_s <= (others => '0');

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

      -- clk enable for spinner
      clk_en_spinner_counter_s <= clk_en_spinner_counter_s + 1;
      if clk_en_spinner_counter_s = 0 then
        clk_en_spinner_s <= '1';
      else
        clk_en_spinner_s <= '0';
      end if;
    end if;
  end process clk_cnt;

  -----------------------------------------------------------------------------
  -- The Colecovision console
  -----------------------------------------------------------------------------
  sg1000 <= index(1);

  cv_console_b : entity work.cv_console
    generic map (
      is_pal_g        => 0,
      compat_rgb_g    => 0
    )
    port map (
      clk_i           => clk_21m3_s,
      clk_en_10m7_i   => clk_en_10m7_q,
      reset_n_i       => reset_n_s,
      sg1000          => sg1000,
      sg1000_row_o    => sg1000_row,
      sg1000_col_i    => sg1000_col,
      dahjeeA_i       => dahjeeA,
      por_n_o         => por_n_s,
      ctrl_p1_i       => ctrl_p1_s,
      ctrl_p2_i       => ctrl_p2_s,
      ctrl_p3_i       => ctrl_p3_s,
      ctrl_p4_i       => ctrl_p4_s,
      ctrl_p5_o       => ctrl_p5_s,
      ctrl_p6_i       => ctrl_p6_s,
      ctrl_p7_i       => ctrl_p7_s,
      ctrl_p8_o       => ctrl_p8_s,
      ctrl_p9_i       => ctrl_p9_s,
      joy0_i          => not joya,
      joy1_i          => not joyb,
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
      cart_a_o        => cart_a_s(19 downto 0),
      cart_pages_i    => romwr_a(19 downto 14),
      cart_en_80_n_o  => cart_en_80_n_s,
      cart_en_a0_n_o  => cart_en_a0_n_s,
      cart_en_c0_n_o  => cart_en_c0_n_s,
      cart_en_e0_n_o  => cart_en_e0_n_s,
      cart_en_sg1000_n_o => cart_en_sg1000_n_s,
      cart_d_i        => cart_d_s,
      col_o           => open,
      rgb_r_o         => coleco_red,
      rgb_g_o         => coleco_green,
      rgb_b_o         => coleco_blue,
      hsync_n_o       => coleco_hs,
      vsync_n_o       => coleco_vs,
      comp_sync_n_o   => open,
      audio_o         => unsigned_audio_s
    );
    
  -----------------------------------------------------------------------------
  -- BIOS ROM
  -----------------------------------------------------------------------------
  bios_b : entity work.sprom
    generic map
		(
      widthad_a     => 13,
			init_file			=> "../../../roms/hex/bios.hex"
    )
    port map 
		(
      clock    			=> clk_21m3_s,
      address 			=> bios_rom_a_s,
      q       			=> bios_rom_d_s
    );
    
  -----------------------------------------------------------------------------
  -- CPU RAM
  -----------------------------------------------------------------------------
  cpu_ram_we_s <= clk_en_10m7_q and
                  not (cpu_ram_we_n_s or cpu_ram_ce_n_s);
  ram_a_s <=           cpu_ram_a_s(14 downto 0) when (sg1000 = '1' and dahjeeA = '1')
        else     "1" & cpu_ram_a_s(13 downto 0) when (sg1000 = '1' and cpu_ram_a_s(14) = '0') -- 16k at $8000 for Basic/The Castle/Othello
        else    "00" & cpu_ram_a_s(12 downto 0) when status(5 downto 4) = "01" -- 8k
        else "00000" & cpu_ram_a_s( 9 downto 0) when status(5 downto 4) = "00" -- 1k
        else     "0" & cpu_ram_a_s(13 downto 0) when sg1000 = '1' -- SGM means 16k on SG1000
        else cpu_ram_a_s; -- SGM/32k

  cpu_ram_b : entity work.spram
    generic map 
		(
      widthad_a     => 15
    )
    port map
		(
      clock    			=> clk_21m3_s,
      address 			=> ram_a_s,
      wren    			=> cpu_ram_we_s,
      data    			=> cpu_ram_d_from_cv_s,
      q       			=> cpu_ram_d_to_cv_s
    );
 
  -----------------------------------------------------------------------------
  -- VRAM
  -----------------------------------------------------------------------------
  
  vram_b : entity work.spram
    generic map (
      widthad_a      => 14
    )
    port map (
      wren      => vram_we_s,
      address   => vram_a_s,
      clock     => clk_21m3_s,
      data      => vram_d_from_cv_s,
      q         => vram_d_to_cv_s
    );

	-- PS/2 keyboard interface
	ps2if_inst : entity work.colecoKeyboard
    port map
    (
      clk       	=> clk_21m3_s,
      reset     	=> not reset_n_s,
  
      -- inputs from PS/2 port
      ps2_clk  		=> ps2Clk,
      ps2_data 		=> ps2Data,
  
      -- user outputs
      keys          => ps2_keys_s,
      joy           => ps2_joy_s,

      sg1000_row    => sg1000_row,
      sg1000_col    => sg1000_col
    );

  joya <= joy0(7 downto 0) when status(6) = '0' else joy1(7 downto 0);
  joyb <= joy1(7 downto 0) when status(6) = '0' else joy0(7 downto 0);
  -----------------------------------------------------------------------------
  -- Process pad_ctrl
  --
  -- Purpose:
  --   Maps the gamepad signals to the controller buses of the console.
  --
  pad_ctrl: process (clk_21m3_s, ctrl_p5_s, ctrl_p8_s, ps2_keys_s, ps2_joy_s, joya, joyb, status)
    variable key_v : natural range cv_keys_t'range;
    variable quadr_in : std_logic_vector(1 downto 0);
    variable joy: std_logic_vector(7 downto 0);
  begin

    for idx in 1 to 2 loop
      if idx = 1 then
          joy := joya;
      else
          joy := joyb;
      end if;

      if rising_edge(clk_21m3_s) then
        if status(7) = '0' then
            ctrl_p7_s(idx) <= '1';
            ctrl_p9_s(idx) <= '1';
        elsif clk_en_spinner_s = '1' then
            quadr_in := ctrl_p7_s(idx) & ctrl_p9_s(idx);
            if joy(1) = '1' then
                case quadr_in is
                when "00" => ctrl_p9_s(idx) <= '1';
                when "01" => ctrl_p7_s(idx) <= '1';
                when "11" => ctrl_p9_s(idx) <= '0';
                when "10" => ctrl_p7_s(idx) <= '0';
                when others => null;
                end case;
            elsif joy(0) = '1' then
                case quadr_in is
                when "00" => ctrl_p7_s(idx) <= '1';
                when "01" => ctrl_p9_s(idx) <= '0';
                when "11" => ctrl_p7_s(idx) <= '0';
                when "10" => ctrl_p9_s(idx) <= '1';
                end case;
            end if;
        end if;
      end if;

      if    ctrl_p5_s(idx) = '0' and ctrl_p8_s(idx) = '1' then
        -- keys and right button enabled --------------------------------------

        key_v := cv_key_none_c;

        --if but_tl_s(idx-1) = '0' then
          if ps2_keys_s(13) = '1' then
            -- KEY 1
            key_v := cv_key_1_c;
          elsif ps2_keys_s(7) = '1' then
            -- KEY 2
            key_v := cv_key_2_c;
          elsif ps2_keys_s(12) = '1' then
            -- KEY 3
            key_v := cv_key_3_c;
          elsif ps2_keys_s(2) = '1' then
            -- KEY 4
            key_v := cv_key_4_c;
          elsif ps2_keys_s(3) = '1' then
            -- KEY 5
            key_v := cv_key_5_c;  
          elsif ps2_keys_s(14) = '1' then
            -- KEY 6
            key_v := cv_key_6_c;  
          elsif ps2_keys_s(5) = '1' then
            -- KEY 7
            key_v := cv_key_7_c;  
          elsif ps2_keys_s(1) = '1' then
            -- KEY 8
            key_v := cv_key_8_c;
          elsif ps2_keys_s(11) = '1' then
            -- KEY 9
            key_v := cv_key_9_c;
          elsif ps2_keys_s(10) = '1' then
            -- KEY 0
            key_v := cv_key_0_c;         
          elsif ps2_keys_s(9) = '1' then
            -- KEY *
            key_v := cv_key_asterisk_c;
          elsif ps2_keys_s(6) = '1' then
            -- KEY #
            key_v := cv_key_number_c;
          end if;
        --end if;

        ctrl_p1_s(idx) <= cv_keys_c(key_v)(1);
        ctrl_p2_s(idx) <= cv_keys_c(key_v)(2);
        ctrl_p3_s(idx) <= cv_keys_c(key_v)(3);
        ctrl_p4_s(idx) <= cv_keys_c(key_v)(4);

        -- KEY X
        ctrl_p6_s(idx) <= not ps2_keys_s(0) and not joy(5); -- button 2

      elsif ctrl_p5_s(idx) = '1' and ctrl_p8_s(idx) = '0' then
        -- joystick and left button enabled -----------------------------------
        ctrl_p1_s(idx) <= not ps2_joy_s(0) and not joy(3);	-- up
        ctrl_p2_s(idx) <= not ps2_joy_s(1) and not joy(2); -- down
        ctrl_p3_s(idx) <= not ps2_joy_s(2) and not joy(1); -- left
        ctrl_p4_s(idx) <= not ps2_joy_s(3) and not joy(0); -- right
        ctrl_p6_s(idx) <= not ps2_joy_s(4) and not joy(4); -- button 1

      else
        -- nothing active -----------------------------------------------------
        ctrl_p1_s(idx) <= '1';
        ctrl_p2_s(idx) <= '1';
        ctrl_p3_s(idx) <= '1';
        ctrl_p4_s(idx) <= '1';
        ctrl_p6_s(idx) <= '1';
      end if;
    end loop;
  end process pad_ctrl;
  --
  
  -----------------------------------------------------------------------------

  -----------------------------------------------------------------------------
  -- VGA Scan Doubler
  -----------------------------------------------------------------------------

scandoubler_inst: scandoubler
    port map (
        clk_sys     => clk_21m3_s,
        scanlines   => status(3 downto 2),

        hs_in       => coleco_hs,
        vs_in       => coleco_vs,
        r_in        => coleco_red(7 downto 2),
        g_in        => coleco_green(7 downto 2),
        b_in        => coleco_blue(7 downto 2),
        
        hs_out      => sd_hs,
        vs_out      => sd_vs,
        r_out       => sd_r,
        g_out       => sd_g,
        b_out       => sd_b
    );

osd_inst: osd
    port map (
        clk_sys     => clk_21m3_s,

        SPI_SCK     => SPI_SCK,
        SPI_SS3     => SPI_SS3,
        SPI_DI      => SPI_DI,

        R_in        => osd_red_i,
        G_in        => osd_green_i,
        B_in        => osd_blue_i,
        HSync       => osd_hs_i,
        VSync       => osd_vs_i,

        R_out       => osd_red_o,
        G_out       => osd_green_o,
        B_out       => osd_blue_o
    );

--
rgb2component: component rgb2ypbpr
        port map
        (
           red => osd_red_o,
           green => osd_green_o,
           blue => osd_blue_o,
           y => vga_y_o,
           pb => vga_pb_o,
           pr => vga_pr_o
        );

osd_red_i   <= coleco_red(7 downto 2) when scandoubler_disable = '1' else sd_r;
osd_green_i <= coleco_green(7 downto 2) when scandoubler_disable = '1' else sd_g;
osd_blue_i  <= coleco_blue(7 downto 2) when scandoubler_disable = '1' else sd_b;
osd_hs_i    <= coleco_hs when scandoubler_disable = '1' else sd_hs;
osd_vs_i    <= coleco_vs when scandoubler_disable = '1' else sd_vs;

 -- If 15kHz Video - composite sync to VGA_HS and VGA_VS high for MiST RGB cable
VGA_HS <= not (coleco_hs xor coleco_vs) when scandoubler_disable='1' else not (sd_hs xor sd_vs) when ypbpr='1' else sd_hs;
VGA_VS <= '1' when scandoubler_disable='1' or ypbpr='1' else sd_vs;
VGA_R <= vga_pr_o when ypbpr='1' else osd_red_o;
VGA_G <= vga_y_o  when ypbpr='1' else osd_green_o;
VGA_B <= vga_pb_o when ypbpr='1' else osd_blue_o;  
  -----------------------------------------------------------------------------

  dac : entity work.dac
    generic map (10)
    port map (
      clk_i     => clk_21m3_s,
      res_n_i   => reset_n_s,
      dac_i     => std_logic_vector(unsigned_audio_s),
      dac_o     => audio_s
    ); 
    
-- MiST interfaces
  
  user_io_d : user_io
    generic map (STRLEN => CONF_STR'length)
    
    port map ( 
      clk_sys => clk_21m3_s,
      clk_sd => '0',
      SPI_CLK => SPI_SCK,
      SPI_SS_IO => CONF_DATA0,    
      SPI_MISO => SPI_DO,    
      SPI_MOSI => SPI_DI,       
      conf_str => to_slv(CONF_STR),
      status => status,
      joystick_0 => joy0,
      joystick_1 => joy1,
      joystick_analog_0 => joy_an0,
      joystick_analog_1 => joy_an1,
      scandoubler_disable => scandoubler_disable,
      ypbpr =>ypbpr,
      SWITCHES => switches,   
      BUTTONS => buttons,
      ps2_kbd_clk => ps2Clk,
      ps2_kbd_data => ps2Data
    );

  data_io_inst: data_io
    port map(SPI_SCK, SPI_SS2, SPI_DI, downl, index, clk_mem_s, clkref, rom_wr, romwr_a, ioctl_dout);

  cart_rom: sdram
  port map (
        SDRAM_DQ    => SDRAM_DQ,
        SDRAM_A     => SDRAM_A,
        SDRAM_DQML  => SDRAM_DQML,
        SDRAM_DQMH  => SDRAM_DQMH,
        SDRAM_BA    => SDRAM_BA,
        SDRAM_nCS   => SDRAM_nCS,
        SDRAM_nWE   => SDRAM_nWE,
        SDRAM_nRAS  => SDRAM_nRAS,
        SDRAM_nCAS  => SDRAM_nCAS,
        SDRAM_CKE   => SDRAM_CKE,

        init        => not pll_locked,
        clk         => clk_mem_s,
        wtbt        => "00",

        addr        => cart_a,
        rd          => rom_en,
        dout        => cart_d_s,
        din         => ioctl_dout,
        we          => rom_wr,
        ready       => ram_ready
  );

  cart_a_s(24 downto 20) <= "00000";
  rom_en <= not (cart_en_80_n_s and cart_en_a0_n_s and cart_en_c0_n_s and cart_en_e0_n_s and cart_en_sg1000_n_s);
  cart_a <= cart_a_s when downl = '0' else romwr_a;

  clkref <= '1' when clk_mem_cnt = "000" else '0';
  force_reset <= downl;

  process(clk_mem_s)
  begin
    if rising_edge (clk_mem_s) then
        clk_mem_cnt <= clk_mem_cnt + 1;
        old_downl <= downl;

        if sg1000 = '1' and downl = '1' then
            if old_downl = '0' then
                chksum <= (others => '0');
                dahjeeA <= '0';
            end if;
            if romwr_a(15 downto 0) = x"2000" then
                chksum <= ioctl_dout;
            elsif romwr_a(15 downto 13) = "001" then -- 2xxx - 3xxx
                chksum <= chksum and ioctl_dout;
            end if;
        end if;
        if sg1000 = '1' and downl = '0' and chksum = x"FF" then
            dahjeeA <= '1';
        end if;
    end if;
  end process;

  AUDIO_L     <= audio_s;
  AUDIO_R     <= audio_s;

end rtl;