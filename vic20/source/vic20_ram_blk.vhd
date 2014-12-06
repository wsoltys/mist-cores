

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_arith.all;
  use ieee.std_logic_unsigned.all;

entity VIC20_RAM_BLK is
  
	GENERIC
	(
		numblks_a		: natural := 8
	);
  port (
    V_ADDR : in  std_logic_vector(9 downto 0);
    DIN    : in  std_logic_vector(7 downto 0);
    DOUT0  : out std_logic_vector(7 downto 0) := (others => '0');
    DOUT1  : out std_logic_vector(7 downto 0) := (others => '0');
    DOUT2  : out std_logic_vector(7 downto 0) := (others => '0');
    DOUT3  : out std_logic_vector(7 downto 0) := (others => '0');
    DOUT4  : out std_logic_vector(7 downto 0) := (others => '0');
    DOUT5  : out std_logic_vector(7 downto 0) := (others => '0');
    DOUT6  : out std_logic_vector(7 downto 0) := (others => '0');
    DOUT7  : out std_logic_vector(7 downto 0) := (others => '0');
    V_RW_L : in  std_logic;
    CS_L   : in  std_logic_vector(7 downto 0) := (others => '1'); -- used for write enable gate only
    CLK    : in  std_logic
  );
end;

architecture RTL of VIC20_RAM_BLK is

  type rom_data_t is array (natural range <>) of std_logic_vector(7 downto 0);
  signal DOUT : rom_data_t(1 to 8); 

begin

  GEN_RAM_BLKS : for i in 1 to numblks_a generate
    signal we : std_logic;
  begin
  
    ram_inst : entity work.spram
      generic map
      (
        widthad_a	=> V_ADDR'length
      )
      port map
      (
        clock	  => CLK,
        address	=> V_ADDR,
        wren	  => we,
        data	  => DIN,
        q	=> DOUT(i)
      );
  
    p_we : process(V_RW_L, CS_L)
    begin
      we <= not CS_L(i) and not V_RW_L;
    end process;
  
  end generate GEN_RAM_BLKS;
  
  DOUT0 <= DOUT(1);
  DOUT1 <= DOUT(2);
  DOUT2 <= DOUT(3);
  DOUT3 <= DOUT(4);
  DOUT4 <= DOUT(5);
  DOUT5 <= DOUT(6);
  DOUT6 <= DOUT(7);
  DOUT7 <= DOUT(8);
  
end architecture RTL;
