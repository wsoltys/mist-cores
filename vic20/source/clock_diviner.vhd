-------------------------------------------------------------------------------
-- Title      : 
-- Project    : 
-------------------------------------------------------------------------------
-- File       : clock_diviner.vhd
-- Author     :   <Fador@HINATA>
-- Company    : 
-- Created    : 2008-10-16
-- Last update: 2008-10-16
-- Platform   : 
-- Standard   : VHDL'87
-------------------------------------------------------------------------------
-- Description:
-------------------------------------------------------------------------------
-- Copyright (c) 2008 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2008-10-16  1.0      Fador	Created
-------------------------------------------------------------------------------

library ieee;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;

entity clock_div is
  
  port (
    CLK_IN  : in  std_logic;
    CLK_OUT : out std_logic);

end clock_div;


architecture RTL of clock_div is

signal clk_count : integer range 0 to 4;

signal clk_state : std_logic;

begin  -- RTL

  div: process (CLK_IN)    
  begin  -- process div
    if CLK_IN'event and CLK_IN = '1' then
      --clk_count <= clk_count+1;
      --if clk_count = 1 then
		--clk_count <= 0;
        clk_state <= not clk_state;
      --end if;
    end if;
	CLK_OUT <= clk_state;
  end process div;

end RTL;
