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
--

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_arith.all;
  use ieee.std_logic_unsigned.all;

use work.pkg_vic20.all;

--
-- This is a crude SIMULATION model of an external flash rom
-- If you have a big enough device you could add a real clock to it
-- and include it into the top level design.
--
entity VIC20_ROMS is
  port (
    DATA              : inout std_logic_vector( 7 downto 0);
    ADDR              : in    std_logic_vector(18 downto 0);
    WE_L              : in    std_logic;
    OE_L              : in    std_logic;
    CE_L              : in    std_logic;
	clk               : in    std_logic
    );
end;

architecture RTL of VIC20_ROMS is
  --signal clk             : std_logic := '0';
  signal basic_rom_dout  : std_logic_vector(7 downto 0);
  signal kernal_rom_dout : std_logic_vector(7 downto 0);
  signal rom_data        : std_logic_vector(7 downto 0);

begin
  --clk <= not clk after 5 ns;
  -- assumes basic  0x0000 - 0x1FFF
  -- assumes kernal 0x2000 - 0x3FFF
  basic_rom : VIC20_BASIC_ROM
    port map (
      CLK         => clk,
      ADDR        => ADDR(12 downto 0),
      DATA        => basic_rom_dout
      );

  kernal_rom : VIC20_KERNAL_ROM
    port map (
      CLK         => clk,
      ADDR        => ADDR(12 downto 0),
      DATA        => kernal_rom_dout
      );

  p_rom_data : process(ADDR, basic_rom_dout, kernal_rom_dout)
  begin
    if ADDR(13) = '0' then
      rom_data <= basic_rom_dout;
    else
      rom_data <= kernal_rom_dout;
    end if;
  end process;

  DATA(7 downto 0) <= rom_data;-- after 80 ns;
end RTL;
