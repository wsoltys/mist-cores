-- MDSynth Sound Chip
--
-- Copyright (c) 2012, Meldora Inc.
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the
-- following conditions are met:
--
--    * Redistributions of source code must retain the above copyright notice, this list of conditions and the
--      following disclaimer.
--    * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the
--      following disclaimer in the documentation and/or other materials provided with the distribution.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
-- INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
-- WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
-- USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL; 


entity dac is
	port (  clk:        in std_logic;
            dac_in:     in std_logic_vector(3 downto 0);
            reset:      in std_logic;
            dac_out:    out std_logic);
end entity dac;

architecture dac_arch of dac is
signal delta_adder : std_logic_vector(9 downto 0);
signal sigma_adder : std_logic_vector(9 downto 0);
signal sigma_latch : std_logic_vector(9 downto 0);
signal delta_b : std_logic_vector(9 downto 0);
begin
	delta_b(9 downto 8) <= sigma_latch(9) & sigma_latch(9);
	delta_b(7 downto 0) <= (others => '0');
	delta_adder <= std_logic_vector(unsigned(dac_in) + unsigned(delta_b));
	sigma_adder <= std_logic_vector(unsigned(delta_adder) + unsigned(sigma_latch));
	process (clk, reset)
	begin
		if (reset = '0') then
			sigma_latch <= ('1', others => '0');
			dac_out <= '0';
		elsif (rising_edge(clk)) then
			sigma_latch <= sigma_adder;
			dac_out <= sigma_latch(9);
		end if;
	end process;
end architecture dac_arch;