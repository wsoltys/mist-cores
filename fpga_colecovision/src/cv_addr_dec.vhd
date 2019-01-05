-------------------------------------------------------------------------------
--
-- FPGA Colecovision
--
-- $Id: cv_addr_dec.vhd,v 1.3 2006/01/05 22:22:29 arnim Exp $
--
-- Address Decoder
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

entity cv_addr_dec is

  port (
    clk_i           : in  std_logic;
    reset_n_i       : in  std_logic;
    sg1000          : in  std_logic;
    dahjeeA_i       : in  std_logic;
    a_i             : in  std_logic_vector(15 downto 0);
    d_i             : in  std_logic_vector(7 downto 0);
    cart_pages_i    : in  std_logic_vector(5 downto 0);
    cart_page_o     : out std_logic_vector(5 downto 0);
    iorq_n_i        : in  std_logic;
    rd_n_i          : in  std_logic;
    wr_n_i          : in  std_logic;
    mreq_n_i        : in  std_logic;
    rfsh_n_i        : in  std_logic;
    bios_rom_ce_n_o : out std_logic;
    ram_ce_n_o      : out std_logic;
    vdp_r_n_o       : out std_logic;
    vdp_w_n_o       : out std_logic;
    psg_we_n_o      : out std_logic;
    ay_addr_we_n_o  : out std_logic;
    ay_data_we_n_o  : out std_logic;
    ay_data_rd_n_o  : out std_logic;
    ctrl_r_n_o      : out std_logic;
    ctrl_en_key_n_o : out std_logic;
    ctrl_en_joy_n_o : out std_logic;
    cart_en_80_n_o  : out std_logic;
    cart_en_a0_n_o  : out std_logic;
    cart_en_c0_n_o  : out std_logic;
    cart_en_e0_n_o  : out std_logic;
    cart_en_sg1000_n_o: out std_logic
  );

end cv_addr_dec;


architecture rtl of cv_addr_dec is

  signal megacart_en   : std_logic;
  signal megacart_page : std_logic_vector(5 downto 0);
  signal bios_en       : std_logic;

begin

  -----------------------------------------------------------------------------
  -- Process dec
  --
  -- Purpose:
  --   Implements the address decoding logic.
  --
  dec: process (a_i,
                iorq_n_i,
                rd_n_i, wr_n_i,
                mreq_n_i,
                rfsh_n_i,
                cart_pages_i,
                bios_en,
                sg1000,
                megacart_en,
                megacart_page)
    variable mux_v : std_logic_vector(2 downto 0);
  begin
    -- default assignments
    bios_rom_ce_n_o <= '1';
    ram_ce_n_o      <= '1';
    vdp_r_n_o       <= '1';
    vdp_w_n_o       <= '1';
    psg_we_n_o      <= '1';
    ay_addr_we_n_o  <= '1';
    ay_data_we_n_o  <= '1';
    ay_data_rd_n_o  <= '1';
    ctrl_r_n_o      <= '1';
    ctrl_en_key_n_o <= '1';
    ctrl_en_joy_n_o <= '1';
    cart_en_80_n_o  <= '1';
    cart_en_a0_n_o  <= '1';
    cart_en_c0_n_o  <= '1';
    cart_en_e0_n_o  <= '1';
    cart_en_sg1000_n_o <='1';

    if sg1000 = '0' and (
       cart_pages_i = "000011" or --  64k
       cart_pages_i = "000111" or -- 128k
       cart_pages_i = "001111" or -- 256k
       cart_pages_i = "011111" or -- 512k
       cart_pages_i = "111111") then -- 1M
        megacart_en <= '1';
    else
        megacart_en <= '0';
    end if;

    -- Paging
    case a_i(15 downto 14) is
        when "10" =>
            if megacart_en = '1' then
                cart_page_o <= cart_pages_i;
            else
                cart_page_o <= "000000";
            end if;
        when "11" =>
            if megacart_en = '1' then
                cart_page_o <= megacart_page;
            else
                cart_page_o <= "000001";
            end if;
        when others =>
            cart_page_o <= "000000";
    end case;

    -- Memory access ----------------------------------------------------------
    if mreq_n_i = '0' and rfsh_n_i = '1' then
        if sg1000 = '1' then
            if a_i(15 downto 14) = "11" then -- c000 - ffff
                ram_ce_n_o <= '0';
            elsif a_i(15 downto 13) = "001" and dahjeeA_i = '1' then -- 2000 - 3fff
                ram_ce_n_o <= '0';
            else
                cart_en_sg1000_n_o <= '0';
            end if;
        else
            case a_i(15 downto 13) is
            when "000" =>
                if bios_en = '1' then
                    bios_rom_ce_n_o   <= '0';
                else
                    ram_ce_n_o <= '0';
                end if;
            when "001" | "010" | "011" =>
                ram_ce_n_o        <= '0'; -- 2000 - 7fff = 24k
            when "100" =>
                cart_en_80_n_o    <= '0';
            when "101" =>
                cart_en_a0_n_o    <= '0';
            when "110" =>
                cart_en_c0_n_o    <= '0';
            when "111" =>
                cart_en_e0_n_o    <= '0';
            when others =>
                null;
            end case;
        end if;
    end if;

    -- I/O access -------------------------------------------------------------
    if iorq_n_i = '0' then
      if sg1000 = '0' and a_i(7) = '1' then
        mux_v := a_i(6) & a_i(5) & wr_n_i;
        case mux_v is
          when "000" =>
            ctrl_en_key_n_o <= '0';
          when "010" =>
            vdp_w_n_o       <= '0';
          when "011" =>
            if rd_n_i = '0' then
              vdp_r_n_o     <= '0';
            end if;
          when "100" =>
            ctrl_en_joy_n_o <= '0';
          when "110" =>
            psg_we_n_o      <= '0';
          when "111" =>
            if rd_n_i = '0' then
              ctrl_r_n_o    <= '0';
            end if;
          when others =>
            null;
        end case;
      end if;

      if sg1000 = '1' then
        mux_v := a_i(7) & a_i(6) & wr_n_i;
        case mux_v is
          when "010" =>
            psg_we_n_o <= '0';
          when "100" =>
            vdp_w_n_o <= '0';
          when "101" =>
            if rd_n_i = '0' then
              vdp_r_n_o <= '0';
            end if;
          when "111" =>
            if rd_n_i = '0' then
              ctrl_r_n_o <= '0';
            end if;
          when others =>
            null;
        end case;
      end if;

      if a_i(7 downto 0) = x"50" and wr_n_i = '0' then
        ay_addr_we_n_o  <= '0';
      elsif a_i(7 downto 0) = x"51" and wr_n_i = '0' then
        ay_data_we_n_o  <= '0';
      elsif a_i(7 downto 0) = x"52" and rd_n_i = '0' then
        ay_data_rd_n_o  <= '0';
      end if;

    end if;

  end process dec;


  --
  -----------------------------------------------------------------------------
  megacart: process (reset_n_i, clk_i)
  begin
        if reset_n_i = '0' then
            megacart_page <= "000000";
            bios_en <= '1';
        elsif rising_edge( clk_i ) then
            -- MegaCart paging
            if megacart_en = '1' and rfsh_n_i = '1' and mreq_n_i = '0' and
               rd_n_i = '0' and a_i(15 downto 6) = x"FF"&"11"
            then
               megacart_page <= a_i(5 downto 0) and cart_pages_i;
            end if;

            -- SGM BIOS enable/disable
            if sg1000 = '1' then
                bios_en <= '0';
            elsif iorq_n_i = '0' and mreq_n_i = '1' and rfsh_n_i = '1' and wr_n_i = '0' and a_i(7 downto 0) = x"7f"
            then
                bios_en <= d_i(1);
            end if;
        end if;
  end process megacart;

end rtl;
