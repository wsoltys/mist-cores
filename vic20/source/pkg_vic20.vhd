--
-- A simulation model of VIC20 hardware
-- Copyright (c) MikeJ - March 2003
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
-- Email vic20@fpgaarcade.com
--
--
-- Revision list
--
-- version 001 initial release

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_arith.all;
  use ieee.std_logic_unsigned.all;

package pkg_vic20 is

--  component T65
--      port(
--          Mode    : in  std_logic_vector(1 downto 0);      -- "00" => 6502, "01" => 65C02, "10" => 65C816
--          Res_n   : in  std_logic;
--          Clk     : in  std_logic;
--          Rdy     : in  std_logic;
--          Abort_n : in  std_logic;
--          IRQ_n   : in  std_logic;
--          NMI_n   : in  std_logic;
--          SO_n    : in  std_logic;
--          R_W_n   : out std_logic;
--          Sync    : out std_logic;
--          EF      : out std_logic;
--          MF      : out std_logic;
--          XF      : out std_logic;
--          ML_n    : out std_logic;
--          VP_n    : out std_logic;
--          VDA     : out std_logic;
--          VPA     : out std_logic;
--          A       : out std_logic_vector(23 downto 0);
--          DI      : in  std_logic_vector(7 downto 0);
--          DO      : out std_logic_vector(7 downto 0)
--      );
--  end component;


  component T65
	port(
		Mode    : in std_logic_vector(1 downto 0);      -- "00" => 6502, "01" => 65C02, "10" => 65C816
		Res_n   : in std_logic;
		Clk             : in std_logic;
		Rdy             : in std_logic;
		Abort_n : in std_logic;
		IRQ_n   : in std_logic;
		NMI_n   : in std_logic;
		SO_n    : in std_logic;
		R_W_n   : out std_logic;
		Sync    : out std_logic;
		EF              : out std_logic;
		MF              : out std_logic;
		XF              : out std_logic;
		ML_n    : out std_logic;
		VP_n    : out std_logic;
		VDA             : out std_logic;
		VPA             : out std_logic;
		A               : out std_logic_vector(23 downto 0);
		DI              : in std_logic_vector(7 downto 0);
		DO              : out std_logic_vector(7 downto 0)
	);
  end component;

  
--  component R6502_TC
--    port (
--      clk_clk_i   : in  std_logic;
--      d_i         : in  std_logic_vector (7 downto 0);
--      irq_n_i     : in  std_logic;
--      nmi_n_i     : in  std_logic;
--      rdy_i       : in  std_logic;
--      rst_rst_n_i : in  std_logic;
--      so_n_i      : in  std_logic;
--      a_o         : out std_logic_vector (15 downto 0);
--      d_o         : out std_logic_vector (7 downto 0);
--      rd_o        : out std_logic;
--      sync_o      : out std_logic;
--      wr_n_o      : out std_logic;
--      wr_o        : out std_logic);
--  end component;

  component VIC20_VIC
    port (
      RW_L              : in    std_logic;

      ADDR_IN           : in    std_logic_vector(13 downto 0);
      ADDR_OUT          : out   std_logic_vector(13 downto 0);

      DATA_OE_OUT_L     : out   std_logic;
      DATA_IN           : in    std_logic_vector(11 downto 0);
      DATA_OUT          : out   std_logic_vector( 7 downto 0);
      --
      AUDIO_OUT         : out   std_logic_vector(3 downto 0);

      VIDEO_R_OUT       : out   std_logic_vector(3 downto 0);
      VIDEO_G_OUT       : out   std_logic_vector(3 downto 0);
      VIDEO_B_OUT       : out   std_logic_vector(3 downto 0);

      HSYNC_OUT         : out   std_logic;
      VSYNC_OUT         : out   std_logic;
      COMP_SYNC_L_OUT   : out   std_logic;
      --
      --
      LIGHT_PEN_IN      : in    std_logic;
      POTX_IN           : in    std_logic;
      POTY_IN           : in    std_logic;

      ENA_1MHZ          : out   std_logic; -- 1.1 MHz strobe
      P2_H              : out   std_logic; -- 2.2 MHz cpu access
      CLK_4             : in    std_logic  -- 4.436 MHz
      );
  end component;

  component M6522 is
    port (
      RS              : in    std_logic_vector(3 downto 0);
      DATA_IN         : in    std_logic_vector(7 downto 0);
      DATA_OUT        : out   std_logic_vector(7 downto 0);
      DATA_OUT_OE_L   : out   std_logic;

      RW_L            : in    std_logic;
      CS1             : in    std_logic;
      CS2_L           : in    std_logic;

      IRQ_L           : out   std_logic; -- note, not open drain

      CA1_IN          : in    std_logic;
      CA2_IN          : in    std_logic;
      CA2_OUT         : out   std_logic;
      CA2_OUT_OE_L    : out   std_logic;

      PA_IN           : in    std_logic_vector(7 downto 0);
      PA_OUT          : out   std_logic_vector(7 downto 0);
      PA_OUT_OE_L     : out   std_logic_vector(7 downto 0);

      -- port b
      CB1_IN          : in    std_logic;
      CB1_OUT         : out   std_logic;
      CB1_OUT_OE_L    : out   std_logic;

      CB2_IN          : in    std_logic;
      CB2_OUT         : out   std_logic;
      CB2_OUT_OE_L    : out   std_logic;

      PB_IN           : in    std_logic_vector(7 downto 0);
      PB_OUT          : out   std_logic_vector(7 downto 0);
      PB_OUT_OE_L     : out   std_logic_vector(7 downto 0);

      RESET_L         : in    std_logic;
      P2_H            : in    std_logic; -- high for phase 2 clock  ____----__
      CLK_4           : in    std_logic  -- 4x system clock (4HZ)   _-_-_-_-_-
      );
  end component;

  component VIC20_PS2_IF
    port (

      PS2_CLK         : in    std_logic;
      PS2_DATA        : in    std_logic;

      COL_IN          : in    std_logic_vector(7 downto 0);
      ROW_OUT         : out   std_logic_vector(7 downto 0);
      RESTORE         : out   std_logic;

      RESET_L         : in    std_logic;
      ENA_1MHZ        : in    std_logic;
      P2_H            : in    std_logic; -- high for phase 2 clock  ____----__
      CLK_4           : in    std_logic  -- 4x system clock (4HZ)   _-_-_-_-_-
      );
  end component;

  component VIC20_CHAR_ROM
    port (
      CLK         : in    std_logic;
      ADDR        : in    std_logic_vector(11 downto 0);
      DATA        : out   std_logic_vector(7 downto 0)
      );
  end component;

  component VIC20_BASIC_ROM
    port (
      CLK         : in    std_logic;
      ADDR        : in    std_logic_vector(12 downto 0);
      DATA        : out   std_logic_vector(7 downto 0)
      );
  end component;

  component VIC20_KERNAL_ROM
    port (
      CLK         : in    std_logic;
      ADDR        : in    std_logic_vector(12 downto 0);
      DATA        : out   std_logic_vector(7 downto 0)
      );
  end component;

  component VIC20_GAME_ROM
    port (
      CLK         : in    std_logic;
      ADDR        : in    std_logic_vector(12 downto 0);
      DATA        : out   std_logic_vector(7 downto 0)
      );
  end component;

  component VIC20_RAM
    port (
      V_ADDR      : in  std_logic_vector(9 downto 0);
      DIN         : in  std_logic_vector(7 downto 0);
      DOUT        : out std_logic_vector(7 downto 0);
      V_RW_L      : in  std_logic;
      ENA         : in  std_logic;
      CS_L        : in  std_logic; -- used for write enable gate only
      CLK         : in  std_logic
    );
  end component;

  component VIC20_RAMS
    port (
      V_ADDR      : in  std_logic_vector(9 downto 0);
      DIN         : in  std_logic_vector(7 downto 0);
      DOUT        : out std_logic_vector(7 downto 0);
      V_RW_L      : in  std_logic;
      --ENA         : in  std_logic;
      CS1_L        : in  std_logic; -- used for write enable gate only
      CS2_L        : in  std_logic; -- used for write enable gate only
      CLK         : in  std_logic
    );
  end component;


end;

package body pkg_vic20 is

end;
