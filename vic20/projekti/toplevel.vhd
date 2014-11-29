-- Copyright (C) 1991-2014 Altera Corporation
-- Your use of Altera Corporation's design tools, logic functions 
-- and other software and tools, and its AMPP partner logic 
-- functions, and any output files from any of the foregoing 
-- (including device programming or simulation files), and any 
-- associated documentation or information are expressly subject 
-- to the terms and conditions of the Altera Program License 
-- Subscription Agreement, Altera MegaCore Function License 
-- Agreement, or other applicable license agreement, including, 
-- without limitation, that your use is for the sole purpose of 
-- programming logic devices manufactured by Altera and sold by 
-- Altera or its authorized distributors.  Please refer to the 
-- applicable agreement for further details.

-- PROGRAM		"Quartus II 64-Bit"
-- VERSION		"Version 13.1.4 Build 182 03/12/2014 SJ Web Edition"
-- CREATED		"Sat Nov 29 12:34:36 2014"

LIBRARY ieee;
USE ieee.std_logic_1164.all; 

LIBRARY work;

ENTITY toplevel IS 
	PORT
	(
		PS2_CLK :  IN  STD_LOGIC;
		PS2_DAT :  IN  STD_LOGIC;
		CLOCK_27 :  IN  STD_LOGIC;
		FL_DQ :  INOUT  STD_LOGIC_VECTOR(7 DOWNTO 0);
		SRAM_DQ :  INOUT  STD_LOGIC_VECTOR(7 DOWNTO 0);
		SW :  IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
		VGA_HS :  OUT  STD_LOGIC;
		VGA_VS :  OUT  STD_LOGIC;
		VGA_CLK :  OUT  STD_LOGIC;
		FL_OE_N :  OUT  STD_LOGIC;
		FL_CE_N :  OUT  STD_LOGIC;
		FL_WE_N :  OUT  STD_LOGIC;
		FL_RST_N :  OUT  STD_LOGIC;
		TD_RESET :  OUT  STD_LOGIC;
		SRAM_WE_N :  OUT  STD_LOGIC;
		SRAM_CE_N :  OUT  STD_LOGIC;
		FL_ADDR :  OUT  STD_LOGIC_VECTOR(7 DOWNTO 0);
		LEDG :  OUT  STD_LOGIC_VECTOR(7 DOWNTO 0);
		SRAM_ADDR :  OUT  STD_LOGIC_VECTOR(17 DOWNTO 0);
		VGA_B :  OUT  STD_LOGIC_VECTOR(9 DOWNTO 6);
		VGA_G :  OUT  STD_LOGIC_VECTOR(9 DOWNTO 6);
		VGA_R :  OUT  STD_LOGIC_VECTOR(9 DOWNTO 6)
	);
END toplevel;

ARCHITECTURE bdf_type OF toplevel IS 

COMPONENT vic20
	PORT(I_PS2_CLK : IN STD_LOGIC;
		 I_PS2_DATA : IN STD_LOGIC;
		 RESET_L : IN STD_LOGIC;
		 CLK_40 : IN STD_LOGIC;
		 CART_SWITCH : IN STD_LOGIC;
		 B_FLASH_DATA : INOUT STD_LOGIC_VECTOR(7 DOWNTO 0);
		 SRAM_DATA : INOUT STD_LOGIC_VECTOR(7 DOWNTO 0);
		 SWITCH : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
		 HSYNC_OUT : OUT STD_LOGIC;
		 VSYNC_OUT : OUT STD_LOGIC;
		 CLK_8_o : OUT STD_LOGIC;
		 O_FLASH_CE_L : OUT STD_LOGIC;
		 O_FLASH_OE_L : OUT STD_LOGIC;
		 O_FLASH_WE_L : OUT STD_LOGIC;
		 O_FLASH_BYTE : OUT STD_LOGIC;
		 SRAM_WE_OUT : OUT STD_LOGIC;
		 AUDIO_OUT : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
		 O_FLASH_ADDR : OUT STD_LOGIC_VECTOR(21 DOWNTO 0);
		 SRAM_ADDR_OUT : OUT STD_LOGIC_VECTOR(17 DOWNTO 0);
		 VIDEO_B_OUT : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
		 VIDEO_G_OUT : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
		 VIDEO_R_OUT : OUT STD_LOGIC_VECTOR(3 DOWNTO 0)
	);
END COMPONENT;

COMPONENT altpll0
	PORT(inclk0 : IN STD_LOGIC;
		 c0 : OUT STD_LOGIC
	);
END COMPONENT;

SIGNAL	SYNTHESIZED_WIRE_0 :  STD_LOGIC;
SIGNAL	SYNTHESIZED_WIRE_1 :  STD_LOGIC;


BEGIN 
FL_RST_N <= '1';
TD_RESET <= '1';
SRAM_CE_N <= '0';
LEDG(4) <= SW(0);



b2v_inst : vic20
PORT MAP(I_PS2_CLK => PS2_CLK,
		 I_PS2_DATA => PS2_DAT,
		 RESET_L => SW(0),
		 CLK_40 => SYNTHESIZED_WIRE_0,
		 CART_SWITCH => SW(1),
		 B_FLASH_DATA => FL_ADDR,
		 SRAM_DATA => SRAM_DQ,
		 SWITCH => SW(3 DOWNTO 2),
		 HSYNC_OUT => VGA_HS,
		 VSYNC_OUT => VGA_VS,
		 CLK_8_o => VGA_CLK,
		 SRAM_WE_OUT => SRAM_WE_N,
		 AUDIO_OUT => LEDG(3 DOWNTO 0),
		 SRAM_ADDR_OUT => SRAM_ADDR,
		 VIDEO_B_OUT => VGA_B,
		 VIDEO_G_OUT => VGA_G,
		 VIDEO_R_OUT => VGA_R);


b2v_inst1 : altpll0
PORT MAP(inclk0 => CLOCK_27,
		 c0 => SYNTHESIZED_WIRE_0);




END bdf_type;