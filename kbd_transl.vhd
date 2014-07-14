-------------------------------------------------------------------------------
--
-- PS/2 keyboard scancode-to-ASCII conversion
--
-- Stephen A. Edwards, sedwards@cs.columbia.edu
-- From an original by Alex Freed
--
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity kbd_transl is
  port (
    shift                : in std_logic;
    incode		 : in unsigned(7 downto 0);
    outcode              : out unsigned(6 downto 0)
   );
end kbd_transl;

architecture kbd_transl_rtl of kbd_transl is

  signal ascii                : unsigned(7 downto 0);
  signal scancode_plus_shift : unsigned(11 downto 0);

begin

  outcode <= ascii(6 downto 0);

  scancode_plus_shift <= "000" & shift & incode;
  
--  This part translates the scan code into an ASCII value...
--  Only the ASCII codes which I considered important have been included.
--  if you want more, just add the appropriate case statement lines...
--  (You will need to know the keyboard scan codes you wish to assign.)
--  The entries are listed in ascending order of ASCII value.

  with scancode_plus_shift select
    ascii <=
     X"08" when X"066", -- Backspace ("backspace" key)
     X"08" when X"166", -- Backspace ("backspace" key)
     X"09" when X"00d", -- Horizontal Tab
     X"09" when X"10d", -- Horizontal Tab
     X"0d" when X"05a", -- Carriage return ("enter" key)
     X"0d" when X"15a", -- Carriage return ("enter" key)
     X"1b" when X"076", -- Escape ("esc" key)
     X"1b" when X"176", -- Escape ("esc" key)
     X"20" when X"029", -- Space
     X"20" when X"129", -- Space
     X"21" when X"116", -- !
     X"22" when X"152", -- "
     X"23" when X"126", -- #
     X"24" when X"125", -- $
     X"25" when X"12e", --
     X"26" when X"13d", --
     X"27" when X"052", --
     X"28" when X"146", --
     X"29" when X"145", --
     X"2a" when X"13e", -- *
     X"2b" when X"155", -- +
     X"2c" when X"041", -- ,
     X"2d" when X"04e", -- -
     X"2e" when X"049", -- .
     X"2f" when X"04a", -- /
     X"30" when X"045", -- 0
     X"31" when X"016", -- 1
     X"32" when X"01e", -- 2
     X"33" when X"026", -- 3
     X"34" when X"025", -- 4
     X"35" when X"02e", -- 5
     X"36" when X"036", -- 6
     X"37" when X"03d", -- 7
     X"38" when X"03e", -- 8
     X"39" when X"046", -- 9
     X"3a" when X"14c", -- :
     X"3b" when X"04c", -- ;
     X"3c" when X"141", -- <
     X"3d" when X"055", -- =
     X"3e" when X"149", -- >
     X"3f" when X"14a", -- ?
     X"40" when X"11e", -- @
     X"41" when X"11c", -- A
     X"42" when X"132", -- B
     X"43" when X"121", -- C
     X"44" when X"123", -- D
     X"45" when X"124", -- E
     X"46" when X"12b", -- F
     X"47" when X"134", -- G
     X"48" when X"133", -- H
     X"49" when X"143", -- I
     X"4a" when X"13b", -- J
     X"4b" when X"142", -- K
     X"4c" when X"14b", -- L
     X"4d" when X"13a", -- M
     X"4e" when X"131", -- N
     X"4f" when X"144", -- O
     X"50" when X"14d", -- P
     X"51" when X"115", -- Q
     X"52" when X"12d", -- R
     X"53" when X"11b", -- S
     X"54" when X"12c", -- T
     X"55" when X"13c", -- U
     X"56" when X"12a", -- V
     X"57" when X"11d", -- W
     X"58" when X"122", -- X
     X"59" when X"135", -- Y
     X"5a" when X"11a", -- Z
     X"5b" when X"054", -- [
     X"5c" when X"05d", -- \
     X"5d" when X"05b", -- ]
     X"5e" when X"136", -- ^
     X"5f" when X"14e", -- _
     X"60" when X"00e", -- `
     X"41" when X"01c", -- A
     X"42" when X"032", -- B
     X"43" when X"021", -- C
     X"44" when X"023", -- D
     X"45" when X"024", -- E
     X"46" when X"02b", -- F
     X"47" when X"034", -- G
     X"48" when X"033", -- H
     X"49" when X"043", -- I
     X"4a" when X"03b", -- J
     X"4b" when X"042", -- K
     X"4c" when X"04b", -- L
     X"4d" when X"03a", -- M
     X"4e" when X"031", -- N
     X"4f" when X"044", -- O
     X"50" when X"04d", -- P
     X"51" when X"015", -- Q
     X"52" when X"02d", -- R
     X"53" when X"01b", -- S
     X"54" when X"02c", -- T
     X"55" when X"03c", -- U
     X"56" when X"02a", -- V
     X"57" when X"01d", -- W
     X"58" when X"022", -- X
     X"59" when X"035", -- Y
     X"5a" when X"01a", -- Z
     X"7b" when X"154", -- {
     X"7c" when X"15d", -- |
     X"7d" when X"15b", -- }
     X"7e" when X"10e", -- ~
     X"7f" when X"071", -- (Delete OR DEL on numeric keypad)
     X"15" when X"074", -- right arrow (cntrl U)
     X"08" when X"06b", -- left arrow (BS)
     X"0B" when X"075", -- (up arrow)
     X"0A" when X"072", -- (down arrow, ^J, LF)
     X"7f" when X"171", -- (Delete OR DEL on numeric keypad)
     X"00" when others;

end kbd_transl_rtl;
