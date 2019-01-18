-------------------------------------------------------------------------------
--
-- FPGA Colecovision
--
-- $Id: cv_ctrl.vhd,v 1.3 2006/01/08 23:58:04 arnim Exp $
--
-- Controller Interface Module
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

entity cv_ctrl is

  port (
    clk_i           : in  std_logic;
    clk_en_3m58_i   : in  std_logic;
    reset_n_i       : in  std_logic;
    ctrl_en_key_n_i : in  std_logic;
    ctrl_en_joy_n_i : in  std_logic;
    a1_i            : in  std_logic;
    ctrl_p1_i       : in  std_logic_vector(2 downto 1);
    ctrl_p2_i       : in  std_logic_vector(2 downto 1);
    ctrl_p3_i       : in  std_logic_vector(2 downto 1);
    ctrl_p4_i       : in  std_logic_vector(2 downto 1);
    ctrl_p5_o       : out std_logic_vector(2 downto 1);
    ctrl_p6_i       : in  std_logic_vector(2 downto 1);
    ctrl_p7_i       : in  std_logic_vector(2 downto 1);
    ctrl_p8_o       : out std_logic_vector(2 downto 1);
    ctrl_p9_i       : in  std_logic_vector(2 downto 1);
    d_o             : out std_logic_vector(7 downto 0);
    int_n_o         : out std_logic
  );

end cv_ctrl;


architecture rtl of cv_ctrl is

  signal sel_q : std_logic;

  signal nand_i1: std_logic_vector(2 downto 1);
  signal nand_i2: std_logic_vector(2 downto 1);
  signal nand_o: std_logic_vector(2 downto 1);
  signal nand_o_d: std_logic_vector(2 downto 1);
  signal int: std_logic_vector(2 downto 1);
  type timer_type is array (2 downto 1) of unsigned (11 downto 0);
  signal rctimer: timer_type;

begin

  -----------------------------------------------------------------------------
  -- Process seq
  --
  -- Purpose:
  --   Implements the R/S flip-flop which selects the controller function.
  --
  seq: process (clk_i, reset_n_i)
    variable ctrl_en_v : std_logic_vector(1 downto 0);
  begin
    if reset_n_i = '0' then
      sel_q <= '0';

    elsif clk_i'event and clk_i = '1' then
      if clk_en_3m58_i = '1' then
        ctrl_en_v := ctrl_en_key_n_i & ctrl_en_joy_n_i;
        case ctrl_en_v is
          when "01" =>
            sel_q <= '0';
          when "10" =>
            sel_q <= '1';
          when others =>
            null;
        end case;
      end if;
    end if;
  end process seq;
  --
  -----------------------------------------------------------------------------


  -----------------------------------------------------------------------------
  -- Controller select
  -----------------------------------------------------------------------------
  ctrl_p5_o(1) <= sel_q;
  ctrl_p5_o(2) <= sel_q;
  ctrl_p8_o(1) <= not sel_q;
  ctrl_p8_o(2) <= not sel_q;


  -----------------------------------------------------------------------------
  -- Process ctrl_read
  --
  -- Purpose:
  --   Read multiplexer for the controller lines.
  --
  ctrl_read: process (clk_i, a1_i, nand_o, nand_i1, nand_i2,
                      ctrl_p1_i, ctrl_p2_i, ctrl_p3_i, ctrl_p4_i,
                      ctrl_p6_i, ctrl_p7_i, ctrl_p9_i, int)
    variable idx_v : natural range 1 to 2;
  begin
    if a1_i = '0' then
      -- read controller #1
      idx_v := 1;
    else
      -- read controller #2
      idx_v := 2;
    end if;

    -- quadrature decoders: movement and direction signals
    for idx in 1 to 2 loop
        nand_i1(idx) <= ctrl_p9_i(idx);
        nand_o(idx) <= not (nand_i1(idx) and nand_i2(idx));
        if rising_edge(clk_i) then
            int(idx) <= '0';
            nand_o_d(idx) <= nand_o(idx);
            if rctimer(idx) = 0 then
                nand_i2(idx) <= '1';
            else
                -- signal movement until the timer is active
                nand_i2(idx) <= '0';
                -- and also fire an interrupt for a while
                if rctimer(idx) > x"4f0" then
                    int(idx) <= '1';
                end if;
            end if;
            if nand_o_d(idx) = '0' and nand_o(idx) = '1' then
                -- movement detected, start timer
                rctimer(idx) <= x"5f0";
            end if;
            if rctimer(idx) /= 0 then
                rctimer(idx) <= rctimer(idx) - 1;
            end if;
        end if;
    end loop;
    int_n_o <= not (int(1) or int(2));

    d_o <= nand_o(idx_v) &           -- quadrature information
           ctrl_p6_i(idx_v) &
           ctrl_p7_i(idx_v) &
           nand_i2(idx_v) &          -- quadrature information
           ctrl_p3_i(idx_v) &
           ctrl_p2_i(idx_v) &
           ctrl_p4_i(idx_v) &
           ctrl_p1_i(idx_v);
  end process ctrl_read;
  --
  -----------------------------------------------------------------------------


end rtl;
