library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- http://tinyvga.com/vga-timing/640x480@60Hz

ENTITY VGASYNC IS
  PORT(
    CLK: IN STD_LOGIC; -- pixel clock 25.175 MHz
    HSYNC,VSYNC: OUT STD_LOGIC;
    R,G,B : OUT STD_LOGIC_VECTOR(3 downto 0);
    KEYS: IN STD_LOGIC_VECTOR(3 downto 0);
    S: IN STD_LOGIC_VECTOR(1 downto 0)
  );
END VGASYNC;