-------------------------------------------------------------------------------
--
-- FPGA Videopac
--
-- $Id: mc_ctrl.vhd,v 1.6 2007/03/31 20:25:39 arnim Exp $
--
-- Multicart controller
--
--
-- Theory of operation
-- ===================
--
-- The multicard controller (MC) is inserted between the VP cartridge port and
-- the game ROM/EPROM/FLASH storage chip. It generates the address for the
-- storage chip based on the VP address and the current mode settings.
-- In general, the MC operates in either of two modes:
--   * menu mode
--   * application mode
--
-- After reset, it starts in menu mode and traverses to application mode once
-- under software control. Switching back to menu mode is only possible with
-- reset or power cycle.
--
-- Menu mode
-- ---------
-- The first 2k of the external storage memory are available as plain program
-- memory in menu mode. I.e. this range is mapped as a standard 2k game
-- cartridge with exclusion of A10, BS0 and BS1.
-- To allow for 256 menu entries, there is a bank switching mechanism active
-- that banks the last page of the first 1k
--   --> programm address range from 0700h to 07ffh
-- Whenever this range is accessed, a programmable bank offset is added to
-- the external memory address:
--
-- External memory       8048 program memory
-- +-------+             +-------+
-- | 0700h | ebank 0     | 0700h |
-- | 07ffh | --------+-> | 07ffh |
-- +-------+         |   +-------+
-- | 0800h | ebank 1 |
-- | 08ffh | --------+
-- +-------+         |
-- | 0900h | ebank 2 |
-- | 09ffh | --------+
-- +-------+
--
--   [...]
--
-- In menu mode, there are several registers defined and active that serve
-- the purpose of configuring the application mode by means of software.
--
-- CART_2K_OFFSET  (@ 080h)
-- Specifies the offset for external storage memory access in application mode
-- in units or 2k blocks. All 8 bits are used, thus the max offset is
--   255 * 2k = 510k
--
-- CART_SIZE_MASK  (@ 081h)
-- Bit 7: 0 = skip A10, 1 = use A10
--        Selects the usage of A10 in application mode.
--        Setting this to 0 skips A10, resulting in a memory layout compatible
--        with standard 2k, 4k and 8k cartridge.
--        Setting this to 1 uses A10 for cartridges with 12k or 16k size.
-- Bit 1..0: Size mask
--           Selects the size mask in application mode.
--           These bits mask the BS0 (bit 0) and BS1 (bit 1) to generate
--           the proper addresses for 2k, 4k and >= 8k cartridges.
--             Cart  Bits
--             Size   1 0
--            ------------
--               2k   0 0
--               4k   0 1
--             >=8k   1 1
--
-- CART_EBANK (@ 082h)
-- Specifies the offset for program memory range 0700h to 07ffh in menu mode in
-- units of a page. Only the lower 4 bits are used, thus the max offset is
--   15 * 256 = 3840
--
-- CART_MODE (@0ffh)
-- When written with 0a5h, the mode is switched from menu to application. A
-- reset on mc_res_n_o output is generated and the settings in CART_2K_OFFSET
-- and CART_SIZE_MASK become effective.
--
-- Application mode
-- ----------------
-- Access to all registers of menu mode are disabled, thus there's no means
-- for the game software to change the settings done during menu mode.
--
-------------------------------------------------------------------------------
--
-- Copyright (c) 2007, Arnim Laeuger (arnim.laeuger@gmx.net)
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

entity mc_ctrl is

  port (
    clk_i       : in  std_logic;
    reset_n_i   : in  std_logic;
    cart_a_i    : in  std_logic_vector(11 downto 0);
    cart_d_i    : in  std_logic_vector( 7 downto 0);
    cart_cs_i   : in  std_logic;
    cart_cs_n_i : in  std_logic;
    cart_wr_n_i : in  std_logic;
    cart_bs0_i  : in  std_logic;
    cart_bs1_i  : in  std_logic;
    extmem_a_o  : out std_logic_vector(18 downto 0)
  );

end mc_ctrl;

library ieee;
use ieee.numeric_std.all;

architecture rtl of mc_ctrl is

  -----------------------------------------------------------------------------
  -- Register address constants
  --
  -- The following constants are offsets to the global address range
  -- from 080h to 0ffh of the external data memory storage.
  --
  subtype  reg_addr_t      is natural range 0 to 127;
  constant reg_2k_offset_c : reg_addr_t := 16#00#;
  constant reg_size_mask_c : reg_addr_t := 16#01#;
  constant reg_ebank_c     : reg_addr_t := 16#02#;
  constant reg_mode_c      : reg_addr_t := 16#7f#;
  --
  constant mode_switch_c   : std_logic_vector(7 downto 0) := "10100101";
  --
  -----------------------------------------------------------------------------

  signal cart_size_mask_q : std_logic_vector(12 downto 11);
  signal cart_use_a10_q   : std_logic;
  signal cart_2k_offset_q : unsigned(7 downto 0);
  signal cart_ebank_q     : unsigned(3 downto 0);
  type   cart_mode_t      is (CART_MODE_MENU, CART_MODE_APPL);
  signal cart_mode_q      : cart_mode_t;

begin

  -----------------------------------------------------------------------------
  -- Assemble the cartridge ROM address bus
  -----------------------------------------------------------------------------
  rom_addr: process (cart_a_i,
                     cart_bs0_i, cart_bs1_i,
                     cart_mode_q,
                     cart_size_mask_q,
                     cart_use_a10_q,
                     cart_ebank_q)
    variable extmem_a_v : std_logic_vector(extmem_a_o'range);
  begin
    extmem_a_v := (others => '0');

    extmem_a_v(9 downto 0) := cart_a_i(9 downto 0);

    if cart_mode_q = CART_MODE_MENU then
      extmem_a_v(10) := cart_a_i(11);
      extmem_a_v(11) := '0';
      extmem_a_v(12) := '0';

      -- implements cart entry banking
      if cart_a_i(10 downto 8) = "111" then
        extmem_a_v(12 downto 8) := std_logic_vector(
          unsigned(extmem_a_v(12 downto 8)) + cart_ebank_q);
      end if;
    else
      if cart_use_a10_q = '1' then
        extmem_a_v(10) := cart_a_i(10);
        extmem_a_v(11) := cart_a_i(11);
        extmem_a_v(12) := cart_bs0_i and cart_size_mask_q(11);
        extmem_a_v(13) := cart_bs1_i and cart_size_mask_q(12);
      else
        extmem_a_v(10) := cart_a_i(11);
        extmem_a_v(11) := cart_bs0_i and cart_size_mask_q(11);
        extmem_a_v(12) := cart_bs1_i and cart_size_mask_q(12);
      end if;

      extmem_a_v(18 downto 11) := std_logic_vector(
        unsigned(extmem_a_v(18 downto 11)) + cart_2k_offset_q);
    end if;

    extmem_a_o <= extmem_a_v;
  end process rom_addr;
  --
  -----------------------------------------------------------------------------


  -----------------------------------------------------------------------------
  -- Process mc_regs
  --
  -- Purpose:
  --   Implements the registers for the multicard implementation.
  --
  mc_regs: process (clk_i, reset_n_i)
  begin
    if reset_n_i = '0' then
      cart_size_mask_q <= (others => '0');
      cart_use_a10_q   <= '0';
      cart_2k_offset_q <= (others => '0');
      cart_ebank_q     <= (others => '0');
      cart_mode_q      <= CART_MODE_MENU;

    elsif rising_edge(clk_i) then
      if cart_cs_i = '0' and cart_cs_n_i = '1' and
         cart_a_i(7) = '1' and
         cart_mode_q = CART_MODE_MENU and
         cart_wr_n_i = '0' then
        -- write access
        case to_integer(unsigned(cart_a_i(6 downto 0))) is
          -- cartidge 2k offset
          when reg_2k_offset_c =>
            cart_2k_offset_q <= unsigned(cart_d_i);

          -- cartridge size mask bits
          when reg_size_mask_c =>
            cart_size_mask_q <= cart_d_i(1 downto 0);
            cart_use_a10_q   <= cart_d_i(7);

          -- entry banking
          when reg_ebank_c =>
            cart_ebank_q <= unsigned(cart_d_i(3 downto 0));

          -- mode switch
          when reg_mode_c =>
            if cart_d_i = mode_switch_c then
              cart_mode_q   <= CART_MODE_APPL;
            end if;

          when others =>
            null;
        end case;
      end if;

    end if;
  end process mc_regs;
  --
  -----------------------------------------------------------------------------

end rtl;
