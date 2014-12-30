-------------------------------------------------------------------------------
-- $Id: ps2_keyboard_comp_pack-p.vhd,v 1.2 2007/02/12 21:00:28 arnim Exp $
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

package ps2_keyboard_comp_pack is

  component ps2_keyboard_interface
    generic (
      TIMER_60USEC_VALUE_PP : integer := 750; -- Number of sys_clks for 60usec
      TIMER_60USEC_BITS_PP  : integer :=  10; -- Number of bits needed for timer
      TIMER_5USEC_VALUE_PP  : integer :=  62; -- Number of sys_clks for debounce
      TIMER_5USEC_BITS_PP   : integer :=   6  -- Number of bits needed for timer
    );
    port(
      clk             : in    std_logic;
      reset           : in    std_logic;
      ps2_clk         : inout std_logic;
      ps2_data        : inout std_logic;
      rx_extended     : out   std_logic;
      rx_released     : out   std_logic;
      rx_shift_key_on : out   std_logic;
      rx_ascii        : out   std_logic_vector(7 downto 0);
      rx_data_ready   : out   std_logic;       -- rx_read_o
      rx_read         : in    std_logic;       -- rx_read_ack_i
      tx_data         : in    std_logic_vector(7 downto 0);
      tx_write        : in    std_logic;
      tx_write_ack    : out   std_logic;
      tx_error_no_keyboard_ack : out  std_logic
    );
  end component;

end;
