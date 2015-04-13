library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity RAMB16_S9  is
  port 
	(
      do   : out std_logic_vector(7 downto 0);
      dop  : out std_logic;
      addr : in std_logic_vector(10 downto 0);
      clk  : in std_logic;
      di   : in std_logic_vector(7 downto 0);
      dip  : in std_logic_vector(0 downto 0);
      en   : in std_logic;
      ssr  : in std_logic;
      we   : in std_logic
  );
end RAMB16_S9;

architecture SYN of RAMB16_S9 is
begin

  spram_inst : entity work.spram
		generic map
		(
			widthad_a		=> 11
		)
    port map 
		(
      q   			=> do,
      address 	=> addr,
      clock			=> clk,
      data   		=> di,
      wren   		=> we
    );

	dop <= '0';
	
end architecture SYN;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity RAMB16_S4  is
  port 
	(
      do   : out std_logic_vector(3 downto 0);
      dop  : out std_logic;
      addr : in std_logic_vector(11 downto 0);
      clk  : in std_logic;
      di   : in std_logic_vector(3 downto 0);
      dip  : in std_logic_vector(0 downto 0);
      en   : in std_logic;
      ssr  : in std_logic;
      we   : in std_logic
  );
end RAMB16_S4;

architecture SYN of RAMB16_S4 is
begin

  spram_inst : entity work.spram
		generic map
		(
			widthad_a		=> 12,
			width_a			=> 4
		)
    port map 
		(
      q   			=> do,
      address 	=> addr,
      clock			=> clk,
      data   		=> di,
      wren   		=> we
    );

	dop <= '0';
	
end architecture SYN;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity RAMB16_S9_S9 is
	generic
	(
		SIM_COLLISION_CHECK : string
	);
	port 
	(
	  -- output
	  DOPB  : out std_logic;
	  DOB   : out std_logic_vector(7 downto 0);
	  DIPB  : in std_logic_vector(0 downto 0);
	  DIB   : in std_logic_vector(7 downto 0);
	  ADDRB : in std_logic_vector(10 downto 0);	-- 10..0
	  WEB   : in std_logic;
	  ENB   : in std_logic;
	  SSRB  : in std_logic;
	  CLKB  : in std_logic;

	  -- input
	  DOPA  : out std_logic;
	  DOA   : out std_logic_vector(7 downto 0);
	  DIPA  : in std_logic_vector(0 downto 0);
	  DIA   : in std_logic_vector(7 downto 0);
	  ADDRA : in std_logic_vector(10 downto 0);
	  WEA   : in std_logic;
	  ENA   : in std_logic;
	  SSRA  : in std_logic;
	  CLKA  : in std_logic
  );
end RAMB16_S9_S9;

architecture SYN of RAMB16_S9_S9 is
begin

	-- port A must be read-only for Cyclone-II SAFE WRITE
	-- in fpgaarcade invaders
	-- - port B is the read-only port
	-- - ENA inputs hard-wired ON
	
	dpram_inst : entity work.dpram
		generic map
		(
			widthad_a		=> 11
		)
		port map
		(
			clock_a			=> CLKB,
			address_a		=> ADDRB,
			data_a			=> DIB,
			wren_a			=> WEB,
			q_a					=> DOB,

			clock_b			=> CLKA,
			address_b		=> ADDRA,
			data_b			=> DIA,
			wren_b			=> WEA,
			q_b					=> DOA
		);

	DOPB <= '0';
	DOPA <= '0';
	
end SYN;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity RAMB16_S18_S18 is
	generic
	(
		SIM_COLLISION_CHECK : string
	);
	port 
	(
	  -- output
	  DOPB  : out std_logic;
	  DOB   : out std_logic_vector(15 downto 0);
	  DIPB  : in std_logic_vector(1 downto 0);
	  DIB   : in std_logic_vector(15 downto 0);
	  ADDRB : in std_logic_vector(9 downto 0);	-- 10..0
	  WEB   : in std_logic;
	  ENB   : in std_logic;
	  SSRB  : in std_logic;
	  CLKB  : in std_logic;

	  -- input
	  DOPA  : out std_logic;
	  DOA   : out std_logic_vector(15 downto 0);
	  DIPA  : in std_logic_vector(1 downto 0);
	  DIA   : in std_logic_vector(15 downto 0);
	  ADDRA : in std_logic_vector(9 downto 0);
	  WEA   : in std_logic;
	  ENA   : in std_logic;
	  SSRA  : in std_logic;
	  CLKA  : in std_logic
  );
end RAMB16_S18_S18;

architecture SYN of RAMB16_S18_S18 is
begin

	-- port A must be read-only for Cyclone-II SAFE WRITE
	-- in fpgaarcade invaders
	-- - port B is the read-only port
	-- - ENA inputs hard-wired ON
	
	dpram_inst : entity work.dpram
		generic map
		(
			widthad_a		=> 10,
			width_a			=> 16
		)
		port map
		(
			clock_a			=> CLKB,
			address_a		=> ADDRB,
			data_a			=> DIB,
			wren_a			=> WEB,
			q_a					=> DOB,

			clock_b			=> CLKA,
			address_b		=> ADDRA,
			data_b			=> DIA,
			wren_b			=> WEA,
			q_b					=> DOA
		);

	DOPA <= '0';
	DOPB <= '0';
	
end SYN;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
LIBRARY lpm;
USE lpm.all;

entity MULT18X18 is
	port
	(
  	P 	: out std_logic_vector(35 downto 0);
    A		: in std_logic_vector(17 downto 0);
    B		: in std_logic_vector(17 downto 0)
  );
end MULT18X18;

architecture SYN of MULT18X18 is

	COMPONENT lpm_mult
	GENERIC
	(
		lpm_hint		: STRING;
		lpm_representation		: STRING;
		lpm_type		: STRING;
		lpm_widtha		: NATURAL;
		lpm_widthb		: NATURAL;
		lpm_widthp		: NATURAL;
		lpm_widths		: NATURAL
	);
	PORT 
	(
		dataa	: IN STD_LOGIC_VECTOR (17 DOWNTO 0);
		datab	: IN STD_LOGIC_VECTOR (17 DOWNTO 0);
		result	: OUT STD_LOGIC_VECTOR (35 DOWNTO 0)
	);
	END COMPONENT;

begin

	lpm_mult_component : lpm_mult
		GENERIC MAP
		(
			lpm_hint => "MAXIMIZE_SPEED=5",
			lpm_representation => "UNSIGNED",
			lpm_type => "LPM_MULT",
			lpm_widtha => 18,
			lpm_widthb => 18,
			lpm_widthp => 36,
			lpm_widths => 1
		)
		PORT MAP
		(
			dataa => A,
			datab => B,
			result => P
		);

end SYN;	
