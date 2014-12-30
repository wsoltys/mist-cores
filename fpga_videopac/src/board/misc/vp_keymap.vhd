-------------------------------------------------------------------------------
-- $Id: vp_keymap.vhd,v 1.3 2007/02/12 22:17:17 arnim Exp $
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity vp_keymap is

  port (
    -- System Interface -------------------------------------------------------
    clk_i           : in  std_logic;
    res_n_i         : in  std_logic;
    -- Videopac Interface -----------------------------------------------------
    keyb_dec_i      : in  std_logic_vector( 6 downto 1);
    keyb_enc_o      : out std_logic_vector(14 downto 7);
    -- Keyboard Interface -----------------------------------------------------
    rx_data_ready_i : in  std_logic;    -- new RX ASCII data availble
    rx_ascii_i      : in  std_logic_vector( 7 downto 0); -- RX ASCII data
    rx_released_i   : in  std_logic;    -- last key was released
    rx_read_o       : out std_logic     -- RX ASCII data acknowledge
  );

end vp_keymap;


library ieee;
use ieee.numeric_std.all;

architecture rtl of vp_keymap is

  constant row_low_c  : natural := 7;
  constant num_rows_c : natural := 8;
  constant col_low_c  : natural := 1;
  constant num_cols_c : natural := 6;

  subtype col_range_t is natural range col_low_c to num_cols_c+col_low_c-1;
  subtype row_t       is std_logic_vector(col_range_t);
  subtype row_range_t is natural range row_low_c to num_rows_c+row_low_c-1;
  type    matrix_t    is array (row_range_t) of row_t;
  signal  matrix_q    : matrix_t;

  type     row_c_t    is array (col_range_t) of character;
  type     matrix_c_t is array (row_range_t) of row_c_t;
  constant matrix_c_c : matrix_c_t := (
    ('0', '8', '+', 'q', 'a', '-'),
    ('1', '9', 'w', 's', 'z', '*'),
    ('2', '\', 'e', 'd', 'x', '/'),
    ('3', '\', 'r', 'f', 'c', '='),
    ('4', ' ', 't', 'g', 'v', 'y'),
    ('5', '?', 'u', 'h', 'b', 'n'),
    ('6', 'l', 'i', 'j', 'm', '\'),
    ('7', 'p', 'o', 'k', '.', '\'));

begin

  -----------------------------------------------------------------------------
  -- Process matrix
  --
  -- Purpose:
  --   Implements the keyboard matrix flip-flops.
  --
  matrix: process (clk_i, res_n_i)
    variable row_v       : row_range_t;
    variable col_v       : col_range_t;
    variable rx_ascii_v  : natural;
    variable valid_key_v : boolean;
  begin
    if res_n_i = '0' then
      matrix_q  <= (others => (others => '0'));
      rx_read_o <= '0';

    elsif rising_edge(clk_i) then
      rx_read_o <= '0';

      if rx_data_ready_i = '1' then
        rx_read_o <= '1';

        rx_ascii_v  := to_integer(unsigned(rx_ascii_i));
        valid_key_v := false;
        row_v       := row_range_t'low;
        col_v       := col_range_t'low;
        case rx_ascii_v is
          when 8 =>                     -- backspace
            row_v := 13;
            col_v :=  6;
            valid_key_v := true;

          when 10 =>                    -- return
            row_v := 14;
            col_v :=  6;
            valid_key_v := true;

          when others =>
            for row in row_range_t loop
              for col in col_range_t loop
                if rx_ascii_v = character'pos(matrix_c_c(row)(col)) then
                  row_v := row;
                  col_v := col;
                  valid_key_v := true;
                end if;
              end loop;
            end loop;

        end case;

        if valid_key_v then
          matrix_q(row_v)(col_v) <= not rx_released_i;
        end if;

      end if;
    end if;
  end process matrix;
  --
  -----------------------------------------------------------------------------


  -----------------------------------------------------------------------------
  -- Process key_map
  --
  -- Purpose:
  --   Implements the key map connections between keyb_dec_i and
  --   keyb_enc_o.
  --
  key_map: process (keyb_dec_i,
                    matrix_q)
  begin
    -- default assignment
    keyb_enc_o <= (others => '1');

    for row in row_range_t loop
      for col in col_range_t loop
        if keyb_dec_i(col) = '0' and
           matrix_q(row)(col) = '1' then
          keyb_enc_o(row) <= '0';
        end if;
      end loop;
    end loop;

  end process key_map;
  --
  -----------------------------------------------------------------------------

end rtl;
