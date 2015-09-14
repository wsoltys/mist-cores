-------------------------------------------------------------
--
-- Apple II RAM card emulation
--
-- Based on http://www.applelogic.org/files/LANGCARDMAN.pdf
-- and the work of Alex Freed http://alexfreed.com/FPGApple/
--
-------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ramcard is
  port (
    clk     : in std_logic;
    reset   : in std_logic;
    addr_in : in std_logic_vector(15 downto 0);
    cpu_di  : in std_logic_vector(7 downto 0);
    cpu_do  : out std_logic_vector(7 downto 0);
    we_in   : std_logic;
    
    -- to main ram
    addr_out: out std_logic_vector(15 downto 0);
    ram_di  : in std_logic_vector(7 downto 0);
    ram_do  : out std_logic_vector(7 downto 0);
    we_out  : std_logic;
  );
end ramcard;

architecture rtl of ramcard is

  -- 16kb extra ram
  type extra_ram is array(0 to 16383) of unsigned(7 downto 0);
  -- Double-ported RAM for holding the data
  signal memory : extra_ram;
  
  signal pre_we : std_logic;
  signal bank1  : std_logic;
  signal iwe    : std_logic := '0';
  signal ioe    : std_logic := '0';
  signal iaddr  : unsigned(13 downto 0) := (others => '0');
  signal Dxxx   : std_logic;

begin

  process(clk)
  begin
    if rising_edge(clk) then
      if reset = '1' then
        pre_we <= '0';
        we_out <= '0';
        iwe    <= '1';
        ioe  <= '0';
        bank1  <= '0';
      else if addr_in(15 downto 4) = x"C08" then
        bank1 <= addr_in(3);
        pre_we <= addr_in(0) and not we_in;
        iwe <= addr_in(0) and pre_we and not we_in;
        ioe <= not (addr_in(0) xor addr_in(1));
      end if;
    end if;
  end process;

  process(clk)
  begin
    if rising_edge(clk) then
      if iwe = '0' and ioe = '0' then
        ram_do <= cpu_di;
        cpu_do <= ram_di;
        we_out <= we_in;
      else
        ram_do <= (others => '0');
        we_out <= '0';
        iaddr <= addr_in(13) & addr_in(12) and not (bank1 and Dxxx) & addr_in(11 downto 0);
        if iwe = '1' then
          memory(to_integer(iaddr)) <= unsigned(cpu_di);
        else
          cpu_do <= std_logic_vector(memory(to_integer(iaddr)));
        end if;
    end if;
  end process;
  
  addr_out <= addr_in;
  Dxxx <= '1' when addr_in(15 downto 12) = "1101" else '0';

end rtl;