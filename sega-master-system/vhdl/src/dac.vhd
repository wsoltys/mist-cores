
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity dac is
	Port (
		clk	: in  STD_LOGIC;
		input	: in  STD_LOGIC_VECTOR (10 downto 0);
		output: out STD_LOGIC);
end dac;

architecture rtl of dac is

	signal delta_adder: unsigned(12 downto 0);
	signal sigma_adder: unsigned(12 downto 0);
	signal sigma_latch: unsigned(12 downto 0) := "0000001000000";
	signal delta_b		: unsigned(12 downto 0);
	
begin

	delta_b <= sigma_latch(12)&sigma_latch(12)&"00000000000";
	delta_adder <= unsigned(input) + delta_b;
	sigma_adder <= delta_adder + sigma_latch;
	
	process (clk, delta_adder)
	begin
		if rising_edge(clk) then
			sigma_latch <= sigma_adder;
			output <= sigma_adder(12);
		end if;
	end process;

end rtl;

