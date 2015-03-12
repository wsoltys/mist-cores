library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- http://tinyvga.com/vga-timing/640x480@60Hz

ENTITY VGASYNC IS
  PORT(
    CLK: IN STD_LOGIC; -- pixel clock 25.175 MHz
    HSYNC,VSYNC: OUT STD_LOGIC;
    R,G,B : OUT STD_LOGIC_VECTOR(5 downto 0)
  );
END VGASYNC;

ARCHITECTURE MAIN of VGASYNC IS

-- Horizontal timing (line)
constant hva : integer := 640; -- Visible area
constant hfp : integer :=  16; -- Front porch
constant hsp : integer :=  96; -- Sync pulse
constant hbp : integer :=  48; -- Back porch

-- Vertical timing (frame)
constant vva : integer := 480; -- Visible area
constant vfp : integer :=  10; -- Front porch
constant vsp : integer :=   2; -- Sync pulse
constant vbp : integer :=  32; -- Back porch


signal HPOS: integer range 0 to 800:=0;
signal VPOS: integer range 0 to 525:=0;

-- screen memory for 80x30=2400 characters
-- font rom from:
-- http://ece320web.groups.et.byu.net/labs/VGATextGeneration/VGA_Terminal.html
constant max_char : integer :=  2399;
type char_ram is array(0 to max_char) of unsigned(6 downto 0);
signal char_memory : char_ram := (others=>"0000000");

signal char_pos : integer range 0 to max_char:=0;
signal char_addr: std_logic_vector(10 downto 0);
signal char_data: std_logic_vector(7 downto 0);
signal char     : std_logic_vector(7 downto 0);


BEGIN

char_rom: entity work.font_rom 
  port map (
    clk  => CLK,
    addr => char_addr,
    data => char_data);
   
char_addr <= std_logic_vector(to_unsigned(to_integer(char_memory(char_pos)*16) + (VPOS mod 16), char_addr'length));

char_memory(500) <= "1001000";
char_memory(501) <= "1000101";
char_memory(502) <= "1001100";
char_memory(503) <= "1001100";
char_memory(504) <= "1001111";

char_memory(506) <= "1010111";
char_memory(507) <= "1101111";
char_memory(508) <= "1110010";
char_memory(509) <= "1101100";
char_memory(510) <= "1100100";

process(CLK)
begin
if rising_edge(CLK) then
  
  if HPOS < (hva+hfp+hsp+hbp) then
    HPOS <= HPOS + 1;
  else
    HPOS <= 0;
    if VPOS < (vva+vfp+vsp+vbp) then
      VPOS <= VPOS + 1;
    else
      VPOS <= 0;
    end if;
  end if;
  
  if HPOS > (hva+hfp) and HPOS < (hva+hfp+hsp) then
    HSYNC <= '0';
  else
    HSYNC <= '1';
  end if;
  
  if VPOS > (vva+vfp) and VPOS < (vva+vfp+vsp) then
    VSYNC <= '0';
  else
    VSYNC <= '1';
  end if;
  
  if (HPOS > hva) or (VPOS > vva) then
    R<=(OTHERS=>'0');
    G<=(OTHERS=>'0');
    B<=(OTHERS=>'0');
  else
  
    -- I dunno why I need to add 3 to get the characters right
    -- maybe its there to give the ram more cycles to prepare for the right char.
    char_pos  <= ((HPOS+3)/8) + ((VPOS/16)*80);
    char <= char_data;
    
  
    -- black background
    R<=(OTHERS=>'0');
    G<=(OTHERS=>'0');
    B<=(OTHERS=>'0');
    
    if char(7 - (HPOS mod 8)) = '1' then
      R<=(OTHERS=>'0');
      G<=(OTHERS=>'1');
      B<=(OTHERS=>'0');
    end if; 
     
  end if;


end if;
end process;


END MAIN;