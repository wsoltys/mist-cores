-------------------------------------------------------------------------------
--
-- Testbench for Apple ][
--
-- Stephen A. Edwards, sedwards@cs.columbia.edu
--
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity testbench is
  
end testbench;

architecture tb of testbench is

  signal CLK_14M : std_logic := '0';
  signal reset : std_logic;
  signal speaker : std_logic;
  signal FLASH_CLK : std_logic := '0';

  signal ram_addr : unsigned(15 downto 0);
  signal D, ram_do : unsigned(7 downto 0);
  signal ram_we : std_logic;
  signal VIDEO, HBL, VBL, LD194, COLOR_LINE : std_logic;

begin

  process
  begin
    reset <= '1';
    wait for 1000 ns;
    reset <= '0';
    wait;
  end process;

  uut : entity work.apple2 port map (
    CLK_14M  => CLK_14M,
    FLASH_CLK => FLASH_CLK,
    reset    => reset,
    ram_addr => ram_addr,
    D => D,
    ram_do => ram_do,
    HBL => HBL,
    VBL => VBL,
    LD194 => LD194,
    ram_we   => ram_we,
    speaker  => speaker,
    K => X"00",
    PD => (others => '0'),
    GAMEPORT => X"00",
    VIDEO => VIDEO,
    COLOR_LINE => COLOR_LINE
    );

  vga : entity work.vga_controller port map (
    CLK_14M => CLK_14M,
    VIDEO   => VIDEO,
    COLOR_LINE => COLOR_LINE,
    HBL     => HBL,
    VBL     => VBL,
    LD194   => LD194);

  CLK_14M <= not CLK_14M after 34.920639355 ns;

  FLASH_CLK <= not FLASH_CLK after 250 ms;

  process
    type ram_type is array(0 to 65535) of unsigned(7 downto 0);
    variable ram : ram_type;
    variable address : integer;
    variable value : unsigned(7 downto 0);
  begin
    value := x"11";
    for address in 0 to 65535 loop
      ram(address) := value;
      -- value := value + 1;
    end loop;
    loop
      ram_do <= ram(to_integer(ram_addr));
      if ram_we = '1' then
        ram(to_integer(ram_addr)) := D;
      end if;
      wait on ram_addr, ram_we, D;
    end loop;
  end process;

end tb;
