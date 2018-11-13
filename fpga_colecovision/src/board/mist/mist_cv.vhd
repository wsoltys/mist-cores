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

  constant CONF_STR : string := "COLECO;COL;F1,BIN;O2,Enable Scanlines,no,yes;";

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
    port(sck: in std_logic;
         ss: in std_logic;
         sdi: in std_logic;
         downloading: out std_logic;
         size: out std_logic_vector(15 downto 0);
         clk: in std_logic;
         we: in std_logic;
         a: in std_logic_vector(14 downto 0);
         din: in std_logic_vector(7 downto 0);
         dout: out std_logic_vector(7 downto 0));
  end component;
  
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

  signal clk21m3 : std_logic;
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
  signal ps2Clk     : std_logic;
  signal ps2Data    : std_logic;
  signal audio      : std_logic;
  
  signal pll_locked : std_logic;
  
  signal VGA_R_O  : std_logic_vector(5 downto 0);
  signal VGA_G_O  : std_logic_vector(5 downto 0);
  signal VGA_B_O  : std_logic_vector(5 downto 0);
  signal VGA_HS_O : std_logic;
  signal VGA_VS_O : std_logic;
  
  signal hsync_out : std_logic;
  signal vsync_out : std_logic;
  signal csync_out : std_logic;
  
  signal downl          : std_logic := '0';
  signal size           : std_logic_vector(15 downto 0) := (others=>'0');
  signal cart_a         : std_logic_vector(14 downto 0);
  signal cart_d         : std_logic_vector(7 downto 0);
  
  signal clk_cnt_q            : unsigned(1 downto 0);
	signal clk_en_5m37_q			  : std_logic;
	signal clk_21m3_s					  : std_logic;
  signal clk_en_10m7_q			  : std_logic;
  signal por_n_s              : std_logic;
  
  signal rgb_col_s            : std_logic_vector(3 downto 0);
  signal rgb_hsync_n_s,
         rgb_vsync_n_s        : std_logic;
  signal rgb_hsync_s,
         rgb_vsync_s          : std_logic;

  signal vga_col_s            : std_logic_vector(3 downto 0);
  signal vga_hsync_s,
         vga_vsync_s          : std_logic;
         
  signal vid_hsync						  : std_logic;
	signal vid_vsync						  : std_logic;
         
  signal vid_r,
				 vid_g,
				 vid_b							  : std_logic_vector(7 downto 0);
  
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

  signal signed_audio_s      : signed(7 downto 0);
  signal dac_audio_s         : std_logic_vector( 7 downto 0);
  signal audio_s             : std_logic;
  
  signal ps2_keys_s				    : std_logic_vector(15 downto 0);
	signal ps2_joy_s				    : std_logic_vector(15 downto 0);

begin

  LED <= '1';
  reset_n_s <= not(status(0) or buttons(1) or force_reset or not pll_locked);

  pll : entity work.mist_pll
    port map (
      inclk0 => CLOCK_27(0),
      c0     => clk_21m3_s,
      locked => pll_locked
      );
      
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
  -- The Colecovision console
  -----------------------------------------------------------------------------
  cv_console_b : entity work.cv_console
    generic map (
      is_pal_g        => 0,
      compat_rgb_g    => 0
    )
    port map (
      clk_i           => clk_21m3_s,
      clk_en_10m7_i   => clk_en_10m7_q,
      reset_n_i       => reset_n_s,
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
      audio_o         => signed_audio_s
    );
    
  rgb_hsync_s <= not rgb_hsync_n_s;
  rgb_vsync_s <= not rgb_vsync_n_s;
  
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
  cpu_ram_b : entity work.spram
    generic map 
		(
      widthad_a     => 10
    )
    port map
		(
      clock    			=> clk_21m3_s,
      address 			=> cpu_ram_a_s,
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
      keys				=> ps2_keys_s,
      joy					=> ps2_joy_s
    );
    
  -----------------------------------------------------------------------------
  -- Process pad_ctrl
  --
  -- Purpose:
  --   Maps the gamepad signals to the controller buses of the console.
  --
  pad_ctrl: process (ctrl_p5_s, ctrl_p8_s)
    variable key_v : natural range cv_keys_t'range;
  begin
    -- quadrature device not implemented
    ctrl_p7_s          <= "11";
    ctrl_p9_s          <= "11";

    for idx in 1 to 2 loop
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
        ctrl_p6_s(idx) <= not ps2_keys_s(0) and not joy1(5); -- button 2

      elsif ctrl_p5_s(idx) = '1' and ctrl_p8_s(idx) = '0' then
        -- joystick and left button enabled -----------------------------------
        ctrl_p1_s(idx) <= not ps2_joy_s(0) and not joy1(3);	-- up
        ctrl_p2_s(idx) <= not ps2_joy_s(1) and not joy1(2); -- down
        ctrl_p3_s(idx) <= not ps2_joy_s(2) and not joy1(1); -- left
        ctrl_p4_s(idx) <= not ps2_joy_s(3) and not joy1(0); -- right
        ctrl_p6_s(idx) <= not ps2_joy_s(4) and not joy1(4); -- button 1

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
  dblscan_b : entity work.dblscan
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
        if scandoubler_disable = '1' then
          vga_col_v := to_integer(unsigned(rgb_col_s));
          vga_r_v   := full_rgb_table_c(vga_col_v)(r_c);
          vga_g_v   := full_rgb_table_c(vga_col_v)(g_c);
          vga_b_v   := full_rgb_table_c(vga_col_v)(b_c);
          --
          vid_r     <= std_logic_vector(to_unsigned(vga_r_v, 8));
          vid_g     <= std_logic_vector(to_unsigned(vga_g_v, 8));
          vid_b     <= std_logic_vector(to_unsigned(vga_b_v, 8));
          
          vid_hsync <= not rgb_hsync_s;
          vid_vsync <= not rgb_vsync_s;
        else
          vga_col_v := to_integer(unsigned(vga_col_s));
          vga_r_v   := full_rgb_table_c(vga_col_v)(r_c);
          vga_g_v   := full_rgb_table_c(vga_col_v)(g_c);
          vga_b_v   := full_rgb_table_c(vga_col_v)(b_c);
          --
          vid_r     <= std_logic_vector(to_unsigned(vga_r_v, 8));
          vid_g     <= std_logic_vector(to_unsigned(vga_g_v, 8));
          vid_b     <= std_logic_vector(to_unsigned(vga_b_v, 8));
          
          vid_hsync <= not vga_hsync_s;
          vid_vsync <= not vga_vsync_s;
        end if;
      end if;

    end if;
  end process vga_col;
  
  -- a minimig vga->scart cable expects a composite sync signal on the VGA_HS output 
  -- and VCC on VGA_VS (to switch into rgb mode)
  csync_out <= not (vid_hsync xor vid_vsync);
  VGA_HS <= vid_hsync when scandoubler_disable='0' else csync_out;
  VGA_VS <= vid_vsync when scandoubler_disable='0' else '1';
  
  -----------------------------------------------------------------------------

  -----------------------------------------------------------------------------
  -- Convert signed audio data of the console (range 127 to -128) to
  -- simple unsigned value.
  -----------------------------------------------------------------------------
  dac_audio_s <= std_logic_vector(unsigned(signed_audio_s + 128));
  
  dac : entity work.dac
    port map (
      clk_i     => clk_21m3_s,
      res_n_i   => reset_n_s,
      dac_i     => dac_audio_s,
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
      SWITCHES => switches,   
      BUTTONS => buttons,
      ps2_kbd_clk => ps2Clk,
      ps2_kbd_data => ps2Data
    );
    
  osd_inst : osd
    port map (
      clk_sys => clk_21m3_s,
      SPI_DI => SPI_DI,
      SPI_SCK => SPI_SCK,
      SPI_SS3 => SPI_SS3,
      R_in => vid_r(7 downto 2),
      G_in => vid_g(7 downto 2),
      B_in => vid_b(7 downto 2),
      HSync => vid_hsync,
      VSync => vid_vsync,
      R_out => VGA_R,
      G_out => VGA_G,
      B_out => VGA_B
    );
    
  data_io_inst: data_io
    port map(SPI_SCK, SPI_SS2, SPI_DI, downl, size, clk_21m3_s, '0', cart_a, (others=>'0'), cart_d);
    
  process(downl)
  begin
    if(downl = '0') then
      cart_a <= cart_a_s;
      cart_d_s <= cart_d;
      force_reset <= '0';
    else
      cart_a <= cart_a_s;
      cart_d_s <= x"FF";
      force_reset <= '1';
    end if;
  end process;
    
  SDRAM_nCAS  <= '1'; -- disable sdram
  AUDIO_L     <= audio_s;
  AUDIO_R     <= audio_s;

end rtl;