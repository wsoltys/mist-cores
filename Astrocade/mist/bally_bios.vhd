library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_unsigned.all;
  use ieee.numeric_std.all;

entity BALLY_BIOS_0 is
  port 
	(
    CLK         : in    std_logic;
    ENA         : in    std_logic;
    ADDR        : in    std_logic_vector(11 downto 0);
    DATA        : out   std_logic_vector(7 downto 0)
    );
end BALLY_BIOS_0;

architecture SYN of BALLY_BIOS_0 is
begin

	rom_inst : entity work.sprom
		generic map
		(
			INIT_FILE 	=> "../roms/bally_bios_0.hex",
			WIDTHAD_A 	=> 12
		)
		port map
		(
			clock				=> CLK,
			address			=> ADDR,
			q						=> DATA
		);
		
end SYN;

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_unsigned.all;
  use ieee.numeric_std.all;

entity BALLY_BIOS_1 is
  port 
	(
    CLK         : in    std_logic;
    ENA         : in    std_logic;
    ADDR        : in    std_logic_vector(11 downto 0);
    DATA        : out   std_logic_vector(7 downto 0)
    );
end BALLY_BIOS_1;

architecture SYN of BALLY_BIOS_1 is
begin

	rom_inst : entity work.sprom
		generic map
		(
			INIT_FILE 	=> "../roms/bally_bios_1.hex",
			WIDTHAD_A 	=> 12
		)
		port map
		(
			clock				=> CLK,
			address			=> ADDR,
			q						=> DATA
		);
		
end SYN;
