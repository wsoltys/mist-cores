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

--use work.pkg_vic20_xilinx_prims.all;
use work.pkg_vic20.all;

entity VIC20_RAMS is
  port (
  V_ADDR : in  std_logic_vector(9 downto 0);
  DIN    : in  std_logic_vector(7 downto 0);
  DOUT   : out std_logic_vector(7 downto 0);
  V_RW_L : in  std_logic;
  CS1_L  : in  std_logic;
  CS2_L  : in  std_logic;
  --ENA    : in  std_logic;
  CLK    : in  std_logic  
  );
end;

architecture RTL of VIC20_RAMS is

  signal we : std_logic;
  signal RST_N : std_logic;

  type arr is array(0 to 512) of std_logic_vector(7 downto 0);

  signal datastore : arr;

begin
--  r1 : component RAMB4_S4
--    port map (
--      do   => DOUT(7 downto 4),
--      di   => DIN(7 downto 4),
--      addr => V_ADDR(9 downto 0),
--      we   => we,
--      en   => '1',
--      rst  => '0',
--      clk  => CLK
--      );
--
--  r0 : component RAMB4_S4
--    port map (
--      do   => DOUT(3 downto 0),
--      di   => DIN(3 downto 0),
--      addr => V_ADDR(9 downto 0),
--      we   => we,
--      en   => '1',
--      rst  => '0',
--      clk  => CLK
--      );
--

  main: process (CLK)
  begin  -- process main
    if CLK'event and CLK = '1' then  -- rising clock edge
      if we = '1' then
          datastore(conv_integer(CS1_L & V_ADDR)) <= DIN;
        else
          DOUT <= datastore(conv_integer(CS1_L & V_ADDR));
        end if;
      end if;
  end process main;
  
  p_we : process(V_RW_L, CS1_L,CS2_L)
  begin
    we <= ((not CS1_L) or (not CS2_L)) and (not V_RW_L);
  end process;

end architecture RTL;
