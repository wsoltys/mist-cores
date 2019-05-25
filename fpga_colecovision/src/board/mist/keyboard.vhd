library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;

use work.kbd_pkg.all;

entity colecoKeyboard is
port
(
    clk       	: in     std_logic;
    reset     	: in     std_logic;

		-- inputs from PS/2 port
    ps2_clk     : inout  std_logic;
    ps2_data    : inout  std_logic;

    -- user outputs
    keys        : out std_logic_vector(15 downto 0);
    joy         : out std_logic_vector(15 downto 0);

    -- sg1000/sc3000 matrix
    sg1000_row  : in  std_logic_vector( 2 downto 0);
    sg1000_col  : out std_logic_vector(11 downto 0)
);
end colecoKeyboard;

architecture SYN of colecoKeyboard is

  component ps2kbd                                          
    port
    (
      clk       : in  std_logic;                            
      rst_n     : in  std_logic;                            
      tick1us   : in  std_logic;
      ps2_clk   : in  std_logic;                            
      ps2_data  : in  std_logic;                            

      reset     : out std_logic;                            
      keydown   : out std_logic;                            
      keyup     : out std_logic;                            
      scancode  : out std_logic_vector(7 downto 0)
    );
  end component;

  signal rst_n			: std_logic;

  -- 1us tick for PS/2 interface
  signal tick1us		: std_logic;

  signal ps2_reset      : std_logic;
  signal ps2_press      : std_logic;
  signal ps2_release    : std_logic;
  signal ps2_scancode   : std_logic_vector(7 downto 0);

  type key_matrix is array (0 to 7) of std_logic_vector(11 downto 0);
  signal sg1000_matrix  : key_matrix;

begin

	rst_n <= not reset;
	
	-- produce a 1us tick from the 20MHz ref clock
  process (clk, reset)
		variable count : integer range 0 to 19;
	begin
	  if reset = '1' then
			tick1us <= '0';
			count := 0;
	  elsif rising_edge (clk) then
			if count = 19 then
		  	tick1us <= '1';
		  	count := 0;
			else
		  	tick1us <= '0';
		  	count := count + 1;
			end if;
	  end if;
	end process;
	
    sg1000_col <= not sg1000_matrix(CONV_INTEGER(sg1000_row));

    latchInputs: process (clk, rst_n)

    begin

         -- note: all inputs are active HIGH

        if rst_n = '0' then
           keys <= (others => '0');
           joy <= (others => '0');
        elsif rising_edge (clk) then
            if (ps2_press or ps2_release) = '1' then
               case ps2_scancode is

                    -- this is not a valid scancode
                    -- but stuff the right button in here
                    when SCANCODE_X =>
                         keys(0) <= ps2_press;
                    when SCANCODE_8 =>
                         keys(1) <= ps2_press;
                    when SCANCODE_4 =>
                         keys(2) <= ps2_press;
                    when SCANCODE_5 =>
                         keys(3) <= ps2_press;
                    when SCANCODE_7 =>
                         keys(5) <= ps2_press;
                    when SCANCODE_Q =>						-- '#'
                         keys(6) <= ps2_press;
                    when SCANCODE_2 =>
                         keys(7) <= ps2_press;
                    when SCANCODE_W =>						-- '*'
                         keys(9) <= ps2_press;
                    when SCANCODE_0 =>
                         keys(10) <= ps2_press;
                    when SCANCODE_9 =>
                         keys(11) <= ps2_press;
                    when SCANCODE_3 =>
                         keys(12) <= ps2_press;
                    when SCANCODE_1 =>
                         keys(13) <= ps2_press;
                    when SCANCODE_6 =>
                         keys(14) <= ps2_press;

                    when SCANCODE_UP =>
                         joy(0) <= ps2_press;
                    when SCANCODE_DOWN =>
                         joy(1) <= ps2_press;
                    when SCANCODE_LEFT =>
                         joy(2) <= ps2_press;
                    when SCANCODE_RIGHT =>
                         joy(3) <= ps2_press;
                    when SCANCODE_Z =>
                         joy(4) <= ps2_press;

                    when others =>
                end case;
            end if; -- ps2_press or release

            -- sg1000 key matrix
            if (ps2_press or ps2_release) = '1' then
                case ps2_scancode is
                    when SCANCODE_8         => sg1000_matrix(0)(8) <= ps2_press;
                    when SCANCODE_I         => sg1000_matrix(0)(7) <= ps2_press;
                    when SCANCODE_K         => sg1000_matrix(0)(6) <= ps2_press;
                    when SCANCODE_COMMA     => sg1000_matrix(0)(5) <= ps2_press;
                    when SCANCODE_EQUALS    => sg1000_matrix(0)(4) <= ps2_press;
                    when SCANCODE_Z         => sg1000_matrix(0)(3) <= ps2_press;
                    when SCANCODE_A         => sg1000_matrix(0)(2) <= ps2_press;
                    when SCANCODE_Q         => sg1000_matrix(0)(1) <= ps2_press;
                    when SCANCODE_1         => sg1000_matrix(0)(0) <= ps2_press;

                    when SCANCODE_9         => sg1000_matrix(1)(8) <= ps2_press;
                    when SCANCODE_O         => sg1000_matrix(1)(7) <= ps2_press;
                    when SCANCODE_L         => sg1000_matrix(1)(6) <= ps2_press;
                    when SCANCODE_PERIOD    => sg1000_matrix(1)(5) <= ps2_press;
                    when SCANCODE_SPACE     => sg1000_matrix(1)(4) <= ps2_press;
                    when SCANCODE_X         => sg1000_matrix(1)(3) <= ps2_press;
                    when SCANCODE_S         => sg1000_matrix(1)(2) <= ps2_press;
                    when SCANCODE_W         => sg1000_matrix(1)(1) <= ps2_press;
                    when SCANCODE_2         => sg1000_matrix(1)(0) <= ps2_press;

                    when SCANCODE_0         => sg1000_matrix(2)(8) <= ps2_press;
                    when SCANCODE_P         => sg1000_matrix(2)(7) <= ps2_press;
                    when SCANCODE_SEMICOLON => sg1000_matrix(2)(6) <= ps2_press;
                    when SCANCODE_SLASH     => sg1000_matrix(2)(5) <= ps2_press;
                    when SCANCODE_HOME      => sg1000_matrix(2)(4) <= ps2_press;
                    when SCANCODE_C         => sg1000_matrix(2)(3) <= ps2_press;
                    when SCANCODE_D         => sg1000_matrix(2)(2) <= ps2_press;
                    when SCANCODE_E         => sg1000_matrix(2)(1) <= ps2_press;
                    when SCANCODE_3         => sg1000_matrix(2)(0) <= ps2_press;

                    when SCANCODE_MINUS     => sg1000_matrix(3)(8) <= ps2_press;
                    when SCANCODE_BACKQUOTE => sg1000_matrix(3)(7) <= ps2_press;
                    when SCANCODE_QUOTE     => sg1000_matrix(3)(6) <= ps2_press;
                    when SCANCODE_BACKSPACE => sg1000_matrix(3)(4) <= ps2_press;
                    when SCANCODE_DELETE    => sg1000_matrix(3)(4) <= ps2_press;
                    when SCANCODE_INS       => sg1000_matrix(3)(4) <= ps2_press;
                    when SCANCODE_V         => sg1000_matrix(3)(3) <= ps2_press;
                    when SCANCODE_F         => sg1000_matrix(3)(2) <= ps2_press;
                    when SCANCODE_R         => sg1000_matrix(3)(1) <= ps2_press;
                    when SCANCODE_4         => sg1000_matrix(3)(0) <= ps2_press;

                    when SCANCODE_F1        => sg1000_matrix(4)(8) <= ps2_press; -- ^
                    when SCANCODE_OPENBRKT  => sg1000_matrix(4)(7) <= ps2_press;
                    when SCANCODE_CLOSEBRKT => sg1000_matrix(4)(6) <= ps2_press;
                    when SCANCODE_DOWN      => sg1000_matrix(4)(5) <= ps2_press;
                    when SCANCODE_B         => sg1000_matrix(4)(3) <= ps2_press;
                    when SCANCODE_G         => sg1000_matrix(4)(2) <= ps2_press;
                    when SCANCODE_T         => sg1000_matrix(4)(1) <= ps2_press;
                    when SCANCODE_5         => sg1000_matrix(4)(0) <= ps2_press;

                    when SCANCODE_LALT      => sg1000_matrix(5)(11) <= ps2_press; -- FUNC
                    when SCANCODE_BACKSLASH => sg1000_matrix(5)(8) <= ps2_press;
                    when SCANCODE_ENTER     => sg1000_matrix(5)(6) <= ps2_press;
                    when SCANCODE_LEFT      => sg1000_matrix(5)(5) <= ps2_press;
                    when SCANCODE_N         => sg1000_matrix(5)(3) <= ps2_press;
                    when SCANCODE_H         => sg1000_matrix(5)(2) <= ps2_press;
                    when SCANCODE_Y         => sg1000_matrix(5)(1) <= ps2_press;
                    when SCANCODE_6         => sg1000_matrix(5)(0) <= ps2_press;

                    when SCANCODE_RSHIFT    => sg1000_matrix(6)(11) <= ps2_press;
                    when SCANCODE_LSHIFT    => sg1000_matrix(6)(11) <= ps2_press;
                    when SCANCODE_LCTRL     => sg1000_matrix(6)(10) <= ps2_press;
                    when SCANCODE_TAB       => sg1000_matrix(6)(9) <= ps2_press; -- GRAPH
                    when SCANCODE_ESC       => sg1000_matrix(6)(8) <= ps2_press; -- BREAK
                    when SCANCODE_UP        => sg1000_matrix(6)(6) <= ps2_press;
                    when SCANCODE_RIGHT     => sg1000_matrix(6)(5) <= ps2_press;
                    when SCANCODE_M         => sg1000_matrix(6)(3) <= ps2_press;
                    when SCANCODE_J         => sg1000_matrix(6)(2) <= ps2_press;
                    when SCANCODE_U         => sg1000_matrix(6)(1) <= ps2_press;
                    when SCANCODE_7         => sg1000_matrix(6)(0) <= ps2_press;

                    when others          => null;
                end case;
            end if;

            if (ps2_reset = '1') then
               keys <= (others => '0');
               joy <= (others => '0');
               sg1000_matrix(0) <= (others => '0');
               sg1000_matrix(1) <= (others => '0');
               sg1000_matrix(2) <= (others => '0');
               sg1000_matrix(3) <= (others => '0');
               sg1000_matrix(4) <= (others => '0');
               sg1000_matrix(5) <= (others => '0');
               sg1000_matrix(6) <= (others => '0');
               sg1000_matrix(7) <= (others => '0');
            end if;
        end if; -- rising_edge (clk)
    end process latchInputs;

  ps2kbd_inst : ps2kbd                                        
    port map
    (
      clk      	=> clk,                                     
      rst_n    	=> rst_n,
      tick1us  	=> tick1us,
      ps2_clk  	=> ps2_clk,
      ps2_data 	=> ps2_data,

      reset    	=> ps2_reset,
      keydown 	=> ps2_press,
      keyup 	  => ps2_release,
      scancode 	=> ps2_scancode
    );

end SYN;
