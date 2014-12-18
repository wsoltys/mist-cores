

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.Numeric_Std.all;

entity VIC20_RAMS is
  port (
    CLK     : in  std_logic;
    V_RW_L    : in  std_logic;
	 CS_L    : in  std_logic;
    V_ADDR    : in  std_logic_vector;
    DIN     : in  std_logic_vector;
    DOUT    : out std_logic_vector
  );
end entity VIC20_RAMS;

architecture RTL of VIC20_RAMS is

   type ram_type is array (0 to (2**V_ADDR'length)-1) of std_logic_vector(DIN'range);
   signal ram : ram_type;
   signal read_address : std_logic_vector(V_ADDR'range);
	signal we : std_logic;

begin

  p_we : process(V_RW_L, CS_L)
  begin
    we <= not (CS_L or V_RW_L);
  end process p_we;

  p_ram : process(CLK) is
  begin
    if rising_edge(CLK) then
      if we = '1' then
        ram(to_integer(unsigned(V_ADDR))) <= DIN;
      end if;
		DOUT <= ram(to_integer(unsigned(V_ADDR)));
  --    read_address <= V_ADDR;
    end if;
  end process p_ram;

  -- DOUT <= ram(to_integer(unsigned(read_address)));

end architecture RTL;
