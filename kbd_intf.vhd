-------------------------------------------------------------------------------
--
-- PS/2 Keyboard Interface for the Apple ][
--
-- Stephen A. Edwards, sedwards@cs.columbia.edu
-- From an original by Alex Freed
--
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity kbd_intf is
  port (
    PS2_Clk              : in std_logic;
    PS2_Data             : in std_logic;
    CLK_14M              : in std_logic;
    read_kb              : in std_logic;
    reset                : in std_logic;
    K                    : out unsigned(7 downto 0)
    );
end kbd_intf;

architecture kbd_intf_rtl of kbd_intf is
  
  signal Scan_Code            : unsigned(7 downto 0);
  signal code_latched         : unsigned(7 downto 0);
  signal ctrl, shift          : std_logic;
  signal decoded              : unsigned(6 downto 0);
  signal kbd_state            : unsigned(2 downto 0);
  signal kbd_state_next       : unsigned(2 downto 0);

  signal DoRead, Scan_DAV : std_logic;
  signal kbd_available : std_logic;

begin

  controller : entity work.PS2_Ctrl port map (
      Clk        => CLK_14M,
      Reset      => reset,
      PS2_Clk    => PS2_Clk,
      PS2_Data   => PS2_Data,
      DoRead     => DoRead,
      Scan_DAV   => Scan_DAV,
      Scan_Code  => Scan_Code);

  DoRead <= Scan_DAV;

  translator : entity work.kbd_transl port map (
      shift      => shift,
      incode     => code_latched,
      outcode    => decoded);

  K <= kbd_available & "00" & decoded(4 downto 0) when ctrl = '1' else
       kbd_available & decoded;

  process (CLK_14M, reset)
  begin
    if reset = '1' then
      shift <= '0';
      ctrl <= '0';
    elsif rising_edge(CLK_14M) then
      if kbd_state = 1 then
        if Scan_Code = "00010010" or Scan_Code = "01011001" then
          shift <= '1';
        elsif  Scan_Code = "00010100" then
          ctrl <= '1';
        end if;
      elsif  kbd_state = 6 then
        if Scan_Code = "00010010" or Scan_Code = "01011001" then
          shift <= '0';
        elsif Scan_Code = "00010100" then
          ctrl <= '0';
        end if;
      end if;
    end if;
  end process;

  process (CLK_14M, reset)
  begin
    if reset = '1' then
      kbd_state <= (others => '0');
      code_latched <= (others => '0');
      kbd_available <= '0';
    elsif rising_edge(CLK_14M) then
      kbd_state <= kbd_state_next ;
      if read_kb = '1' then
        kbd_available <= '0';
      end if;
      if kbd_state = 7 then
        code_latched <= Scan_Code ;
        kbd_available <= '1';
      end if;
    end if;
  end process;

  process (Scan_Code, Scan_DAV, kbd_state)
  begin
    case TO_INTEGER(kbd_state) is
      when 0 =>
        if Scan_DAV = '1' then
          kbd_state_next <= "001";
        else
          kbd_state_next <= "000";
        end if;
      when 1 =>
        -- have something, get it
        kbd_state_next <= "010";
      when 2 =>
        if  Scan_Code = "11110000" then
          kbd_state_next <= "011";
        elsif Scan_Code = "11100000" then
          kbd_state_next <= "000";
        elsif Scan_Code = "00010010" or Scan_Code = "00010100" or
          Scan_Code = "01011001" then
          kbd_state_next <= "000";
        else
          kbd_state_next <= "111";
        end if;
      when 3 =>
          --  was F0 wait a couple of states for Scan_DAV to go down
        kbd_state_next <= "100";
      when 4 =>
        kbd_state_next <= "101";
      when 5 =>
        if  ( Scan_DAV = '1' ) then
          kbd_state_next <= "110";
        else
          --  wait for more
          kbd_state_next <= "101";
        end if;
      when 6 | 7 =>
        kbd_state_next <= "000";
      when others =>
        kbd_state_next <= "000";
    end case;
  end process;

end kbd_intf_rtl;
