library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

package Vcomponents is

	component RAMB16_S9 is
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
	end component;

	component RAMB16_S9_S9 is
		generic
		(
			SIM_COLLISION_CHECK : string := "generate_x_only"
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
	end component;

	component RAMB16_S18_S18 is
		generic
		(
			SIM_COLLISION_CHECK : string := "generate_x_only"
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
	end component;

  component MULT18X18 is
      port
      (
        P 	: out std_logic_vector(35 downto 0);
        A		: in std_logic_vector(17 downto 0);
        B		: in std_logic_vector(17 downto 0)
      );
	end component;
	
end;
