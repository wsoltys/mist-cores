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


BEGIN


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
  
  if (HPOS > hva and HPOS < (hva+hfp+hsp+hbp)) or (VPOS > vva and VPOS < (vva+vfp+vsp+vbp)) then
    R<=(OTHERS=>'0');
    G<=(OTHERS=>'0');
    B<=(OTHERS=>'0');
  else
  
    -- Blue background
    R<=(OTHERS=>'0');
    G<=(OTHERS=>'0');
    B<=(OTHERS=>'1');
    
    -- White cross-hair
    if(HPOS > 315 and HPOS < 325) or (VPOS > 235 and VPOS < 245) then
      R<=(OTHERS=>'1');
      G<=(OTHERS=>'1');
      B<=(OTHERS=>'1');
    end if;   
     
  end if;


end if;
end process;


END MAIN;