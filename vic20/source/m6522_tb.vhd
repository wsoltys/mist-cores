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

entity M6522_TB is
end;

architecture SIM of M6522_TB is

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
      -- port a
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

  constant CLKPERIOD_4    : time := 250 ns;

  signal clk_4            : std_logic;
  signal reset_l          : std_logic;

  signal clk_gen_cnt      : std_logic_vector(1 downto 0) := "00";
  signal p2_h             : std_logic;

  signal rs               : std_logic_vector(3 downto 0) := "XXXX";
  signal data_in          : std_logic_vector(7 downto 0) := "XXXXXXXX";
  signal data_out         : std_logic_vector(7 downto 0);
  signal data_out_oe_l    : std_logic;

  signal rw_l             : std_logic := '1';
  signal cs1              : std_logic := '0';
  signal cs2_l            : std_logic := '1';

  signal irq_l            : std_logic;

  signal ca1_in           : std_logic;
  signal ca2_in           : std_logic;
  signal ca2_out          : std_logic;
  signal ca2_out_oe_l     : std_logic;

  signal pa_in            : std_logic_vector(7 downto 0);
  signal pa_out           : std_logic_vector(7 downto 0);
  signal pa_out_oe_l      : std_logic_vector(7 downto 0);

  signal cb1_in           : std_logic;
  signal cb1_out          : std_logic;
  signal cb1_out_oe_l     : std_logic;

  signal cb2_in           : std_logic;
  signal cb2_out          : std_logic;
  signal cb2_out_oe_l     : std_logic;

  signal pb_in            : std_logic_vector(7 downto 0);
  signal pb_out           : std_logic_vector(7 downto 0);
  signal pb_out_oe_l      : std_logic_vector(7 downto 0);

  signal sr               : std_logic_vector(8 downto 0);
  signal cb1_clk          : std_logic;
begin
  p_clk_4  : process
  begin
    CLK_4 <= '0';
    wait for CLKPERIOD_4 / 2;
    CLK_4 <= '1';
    wait for CLKPERIOD_4 - (CLKPERIOD_4 / 2);
  end process;

  p_rst : process
  begin
    reset_l <= '0';
    wait for 2000 ns;
    reset_l <= '1';
    wait;
  end process;

  p_p6_in : process
  begin
    pb_in(6) <= '0';
    wait for 10000 ns;
    pb_in(6) <= '1';
    wait for 10000 ns;
  end process;

  p_ca1_in : process
  begin
    CA1_IN  <= '1';
    wait for 40000 ns;
    CA1_IN  <= '0';
    wait for 1000 ns;
    CA1_IN  <= '1';
    wait;
  end process;

  p_cb1_clk : process
  begin
    cb1_clk <= '1';
    wait for 10000 ns;
    cb1_clk  <= '0';
    wait for 10000 ns;
    cb1_clk  <= '1';
  end process;

  p_strobe_gen : process
  begin
    wait until rising_edge(clk_4);
    clk_gen_cnt <= clk_gen_cnt + "1";

    p2_h <= not clk_gen_cnt(1);
  end process;

  p_serial_in : process(reset_l, CB1_OUT, cb1_clk)
  begin
    if (reset_l = '0') then
      sr <= 'X' & x"34";
    --elsif falling_edge(CB1_OUT) then
    elsif falling_edge(cb1_clk) then
      sr(8 downto 1) <= sr(7 downto 0);
      --sr(0) <= '0';
      sr(0) <=  sr(8);
    end if;
  end process;
  CB2_IN <= sr(8);
  CB1_IN <= cb1_clk;

  via :  M6522
    port map (

      RS              => rs,
      DATA_IN         => data_in,
      DATA_OUT        => data_out,
      DATA_OUT_OE_L   => data_out_oe_l,

      RW_L            => rw_l,
      CS1             => cs1,
      CS2_L           => cs2_l,

      IRQ_L           => irq_l,
      -- port a
      CA1_IN          => ca1_in,
      CA2_IN          => ca2_in,
      CA2_OUT         => ca2_out,
      CA2_OUT_OE_L    => ca2_out_oe_l,

      PA_IN           => pa_in,
      PA_OUT          => pa_out,
      PA_OUT_OE_L     => pa_out_oe_l,

      -- port b
      CB1_IN          => cb1_in,
      CB1_OUT         => cb1_out,
      CB1_OUT_OE_L    => cb1_out_oe_l,

      CB2_IN          => cb2_in,
      CB2_OUT         => cb2_out,
      CB2_OUT_OE_L    => cb2_out_oe_l,

      PB_IN           => pb_in,
      PB_OUT          => pb_out,
      PB_OUT_OE_L     => pb_out_oe_l,

      RESET_L         => reset_l,
      P2_H            => p2_h,
      CLK_4           => clk_4
      );


      CA2_IN  <= '0';
      PA_IN   <= "00000000";
      PB_IN(5 downto 0)   <= "000000";
      PB_IN(7) <= '0';

 p_sim : process

  procedure wait_for_reset is
  begin
    wait until rising_edge(clk_4);
    while (reset_l = '0') loop
      wait until rising_edge(clk_4);
    end loop;
  end wait_for_reset;

  procedure write(
    addr : in bit_vector(3 downto 0);
    data : in bit_vector(7 downto 0)) is
  begin
    rw_l <= '1';
    cs1  <= '0';
    cs2_l <= '1';
    data_in <= "XXXXXXXX";
    while (clk_gen_cnt /= "10") loop
      wait until rising_edge(clk_4);
    end loop;
    wait until rising_edge(clk_4);

    rs      <= to_stdlogicvector(addr);
    data_in <= to_stdlogicvector(data);
    rw_l <= '0';
    cs1  <= '1';
    cs2_l <= '0';
    wait until rising_edge(clk_4);
    wait until rising_edge(clk_4);
    wait until rising_edge(clk_4);
    wait until rising_edge(clk_4);
    rw_l <= '1';
    cs1  <= '0';
    cs2_l <= '1';
    data_in <= "XXXXXXXX";
  end write;

  procedure read(
    addr : in bit_vector(3 downto 0)) is
  begin
    rw_l <= '1';
    cs1  <= '0';
    cs2_l <= '1';
    data_in <= "XXXXXXXX";
    while (clk_gen_cnt /= "10") loop
      wait until rising_edge(clk_4);
    end loop;
    wait until rising_edge(clk_4);

    rs      <= to_stdlogicvector(addr);
    data_in <= "XXXXXXXX";
    rw_l <= '1';
    cs1  <= '1';
    cs2_l <= '0';
    wait until rising_edge(clk_4);
    wait until rising_edge(clk_4);
    wait until rising_edge(clk_4);
    wait until rising_edge(clk_4);
    rw_l <= '1';
    cs1  <= '0';
    cs2_l <= '1';
  end read;

 begin
   wait_for_reset;
   write(x"E",x"7F"); -- ier reg, clear ints
   read(x"E");
   write(x"B",x"60"); -- acr reg, tim1 free running, used for irq
   read(x"B");
   write(x"E",x"c0"); -- ier t1 ena
   write(x"4",x"0F"); -- t1 lsb
   write(x"5",x"00"); -- t1 msb
   write(x"8",x"0F"); -- t2 lsb
   write(x"9",x"00"); -- t2 msb
   -- test handshake
   write(x"C",x"08"); -- pcr reg,
   write(x"1",x"00"); -- ora reg,

   wait for 27 us;
   read(x"D"); -- ifr
   read(x"4"); -- t1 lsb, clear int
   wait for 27 us;
   write(x"E",x"7f"); -- ier clear
   write(x"D",x"FF"); -- clear all ints
   write(x"E",x"84"); -- ier sr
   wait for 10 us;
   --write(x"B",x"0C"); -- acr reg, shift reg,
   wait for 50 us;
   write(x"D",x"FF"); -- clear all ints
   write(x"A",x"80"); -- sr
   write(x"B",x"14"); -- acr reg, shift reg,
   write(x"8",x"30"); -- timer2
   write(x"9",x"00"); -- timer2 go
   wait for 1000 us;
   write(x"D",x"FF"); -- clear all ints
   read(x"A"); -- sr

   wait;
  end process;

end architecture SIM;

