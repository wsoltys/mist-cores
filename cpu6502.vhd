-------------------------------------------------------------------------------
--
-- 6502 Processor Core
--
-- Adapted by Stephen A. Edwards, sedwards@cs.columbia.edu
--
-- From an original by Peter Wendrich (pwsoft@syntiac.com)
-- http://www.syntiac.com/fpga64.html
--
--
-- 
--     __    __    __    __    __    __    __    __    __
--    /  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__ clk
--    |     |     |     |     |     |     |     |     |    
--   _____       _____       _____       _____       _____
-- _/     \_____/     \_____/     \_____/     \_____/      pre_phase_zero
--
--         ___         ___         ___         ___         _
--    XXXXX___XXXXXXXXX___XXXXXXXXX___XXXXXXXXX___XXXXXXXXX_ di
--
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.ALL;
use ieee.numeric_std.ALL;

entity cpu6502 is
  port (
    clk            : in std_logic;      -- 2X the main clock
    pre_phase_zero : in std_logic;      -- must alternate
    reset          : in std_logic;
    nmi_n          : in std_logic;
    irq_n          : in std_logic;

    D_IN           : in unsigned(7 downto 0);
    D_OUT          : out unsigned(7 downto 0);
    A           : out unsigned(15 downto 0);
    R_W_N          : out std_logic;     -- Low is write

    pcDebugOut     : out unsigned(15 downto 0);
    opcodeDebugOut : out unsigned(7 downto 0)
    );
end cpu6502;

architecture Behavioral of cpu6502 is
  type cpuCycles is
    (getOpcode, secondFetch, pushPcl, pushPch, readPch_Pc, readPch_Hl,
     addXtoL, addYtoL, readH_Pc, fixH, useHL, prepare_S, stackRead,
     stackWrite, stackLow, stackHigh, incrementPc, branch, branchFixPch,
     indX_dummy, indX_L, indX_H, indY_L, indY_H);

  signal cpuCycle: cpuCycles := getOpcode;

  signal opcode      : unsigned(7 downto 0) := (others => '0');
  signal register_A  : unsigned(7 downto 0) := (others => '0');
  signal register_X  : unsigned(7 downto 0) := (others => '0');
  signal register_Y  : unsigned(7 downto 0) := (others => '0');
  -- Stack pointer 
  signal register_S  : unsigned(7 downto 0) := (others => '0');
  -- Status register [ N V - B D I Z C ]
  signal register_P  : unsigned(7 downto 0) := "00100000";
  signal register_PC : unsigned(15 downto 0) := (others => '1');
  -- Temporary register
  signal register_T  : unsigned(7 downto 0) := (others => '0'); 
  -- Absolute and indirect addressing low and high bytes
  signal register_L  : unsigned(7 downto 0) := (others => '0');
  signal register_H  : unsigned(7 downto 0) := (others => '0');
  
  signal writeEnable : std_logic;
  -- Read, Modify, Write cycle
  signal rmw         : unsigned(2 downto 0) := "000";

  signal nmi_ff      : std_logic := '1';
  signal irq_ff      : std_logic := '1';

  --
  -- ALU signals
  --
  signal aluPIn      : unsigned(7 downto 0) := "00100000";

  signal aluLeft     : unsigned(7 downto 0) := (others => '0');
  signal aluRight    : unsigned(7 downto 0) := (others => '0');

  signal aluOut      : unsigned(7 downto 0) := (others => '0');
  signal aluP        : unsigned(7 downto 0) := "00100000";  
  
begin
  
  aluInputs: process(opcode, aluPIn, register_T, register_A,
                     register_X, register_Y, register_S)
  begin

    case opcode is
      when X"E0" | X"E4" | X"EC" =>
        aluLeft <= register_X;
      when X"C0" | X"C4" | X"CC" =>
        aluLeft <= register_Y;
      when others =>
        aluLeft <= register_A;
    end case;

    case opcode is
      when X"BA" => -- TSX
        aluRight <= register_S;
      when	X"C0" | X"C1" | X"C4" | X"C5" | X"C9" | X"CC" | X"CD" |
        X"D1" | X"D5" | X"D9" | X"DD" | 
        X"E0" | X"E1" | X"E4" | X"E5" | X"E9" | X"EC" | X"ED" |
        X"F1" | X"F5" | X"F9" | X"FD" =>
        aluRight <= not(register_T);
      when others =>
        aluRight <= register_T;
    end case;

  end process;
  
  aluCalculation: process(opcode, aluLeft, aluRight, aluPIn, register_A,
                          register_T, register_X, register_Y, irq_ff)
    variable low: unsigned(4 downto 0);
    variable high: unsigned(4 downto 0);
    variable nineBits: unsigned(8 downto 0);
  begin
    aluP <= aluPIn;
    aluOut <= (others => '-');
    case opcode is
      when X"18" => -- CLC
        aluP(0) <= '0';
      when X"38" => -- SEC
        aluP(0) <= '1';
      when X"58" => -- CLI
        aluP(2) <= '0';
      when X"78" => -- SEI
        aluP(2) <= '1';
      when X"B8" => -- CLV
        aluP(6) <= '0';
      when X"D8" => -- CLD
        aluP(3) <= '0';
      when X"F8" => -- SED
        aluP(3) <= '1';
      when X"61" | X"65" | X"69" | X"6D" | X"71" |
        X"75" | X"79" | X"7D" => -- ADC		
        -- binary mode
        ninebits := ('0' & aluLeft) + ('0' & aluRight) +
                    ("00000000" & aluPIn(0));
        aluP(6) <= (aluLeft(7) xor ninebits(7)) and
                   (aluRight(7) xor ninebits(7));
        aluP(7) <= ninebits(7);
        if ninebits(7 downto 0) = "00000000" then
          aluP(1) <= '1';
        else
          aluP(1) <= '0';
        end if;			
        aluOut <= ninebits(7 downto 0);
        aluP(0) <= nineBits(8);

        -- decimal mode
        if aluPIn(3) = '1' then
          low := ('0' & aluLeft(3 downto 0)) + ('0' & aluRight(3 downto 0)) +
                 ("0000" & aluPIn(0));
          high:= ('0' & aluLeft(7 downto 4)) + ('0' & aluRight(7 downto 4));
          if low > 9 then
            low :=  low + 6;
            high := high + 1;
          end if;
          aluP(6) <= (aluLeft(7) xor high(3)) and (aluRight(7) xor high(3));
          aluP(7) <= high(3);
          if high > 9 then
            high := high + 6;
            aluP(0) <= '1';
          else
            aluP(0) <= high(4);
          end if;

          aluOut <= high(3 downto 0) & low(3 downto 0);
        end if;
      when X"E1" | X"E5" | X"E9" | X"ED" | X"F1" |
        X"F5" | X"F9" | X"FD" => -- SBC 
        -- binary mode
        ninebits := ('0' & aluLeft) + ('0' & aluRight) +
                    ("00000000" & aluPIn(0));
        aluP(6) <= (aluLeft(7) xor ninebits(7)) and
                   (aluRight(7) xor ninebits(7));
        aluP(7) <= ninebits(7);
        if ninebits(7 downto 0) = "00000000" then
          aluP(1) <= '1';
        else
          aluP(1) <= '0';
        end if;			
        aluOut <= ninebits(7 downto 0);
        aluP(0) <= nineBits(8);
        
        -- decimal mode
        if aluPIn(3) = '1' then
          low := ('0' & aluLeft(3 downto 0)) +
                 ('0' & aluRight(3 downto 0)) + ("0000" & aluPIn(0));
          high:= ('0' & aluLeft(7 downto 4)) + ('0' & aluRight(7 downto 4));
          if low(4) = '0' then
            low :=  low - 6;
          else
            high := high + 1;
          end if;
          if high(4) = '0' then
            high := high - 6;
          end if;

          aluOut <= high(3 downto 0) & low(3 downto 0);
          aluP(0) <= high(4);
        end if;				
      when X"21" | X"25" | X"29" | X"2D" | X"31" |
        X"35" | X"39" | X"3D" => -- AND
        nineBits := ("0" & register_A) and ("0" & register_T);
        aluP(7) <= nineBits(7);
        if nineBits(7 downto 0) = "00000000" then
          aluP(1) <= '1';
        else
          aluP(1) <= '0';
        end if;			
        aluOut <= nineBits(7 downto 0);
      when X"06" | X"0E" | X"16" | X"1E" => -- ASL
        nineBits := "0" & register_T(6 downto 0) & "0";
        aluP(0) <= register_T(7);
        aluP(7) <= nineBits(7);
        if nineBits(7 downto 0) = "00000000" then
          aluP(1) <= '1';
        else
          aluP(1) <= '0';
        end if;			
        aluOut <= nineBits(7 downto 0);
      when X"0A" => -- ASL A
        nineBits := "0" & register_A(6 downto 0) & "0";
        aluP(0) <= register_A(7);
        aluP(7) <= nineBits(7);
        if nineBits(7 downto 0) = "00000000" then
          aluP(1) <= '1';
        else
          aluP(1) <= '0';
        end if;			
        aluOut <= nineBits(7 downto 0);
      when X"24" | X"2C" => -- BIT
        nineBits := ("0" & register_A) and ("0" & register_T);
        aluP(6) <= register_T(6);
        aluP(7) <= register_T(7);
        if nineBits(7 downto 0) = "00000000" then
          aluP(1) <= '1';
        else
          aluP(1) <= '0';
        end if;			
        aluOut <= nineBits(7 downto 0);
      when X"C1" | X"C5" | X"C9" | X"CD" | X"D1" |
        X"D5" | X"D9" | X"DD" => -- CMP
        nineBits := ("0" & register_A) + ("0" & aluRight) + 1;
        aluP(0) <= nineBits(8);
        aluP(7) <= nineBits(7);
        if nineBits(7 downto 0) = "00000000" then
          aluP(1) <= '1';
        else
          aluP(1) <= '0';
        end if;			
        aluOut <= nineBits(7 downto 0);
      when X"E0" | X"E4" | X"EC" => -- CPX
        nineBits := ("0" & register_X) + ("0" & aluRight) + 1;
        aluP(0) <= nineBits(8);
        aluP(7) <= nineBits(7);				
        if nineBits(7 downto 0) = "00000000" then
          aluP(1) <= '1';
        else
          aluP(1) <= '0';
        end if;			
        aluOut <= nineBits(7 downto 0);
      when X"C0" | X"C4" | X"CC" => -- CPY
        nineBits := ("0" & register_Y) + ("0" & aluRight) + 1;
        aluP(0) <= nineBits(8);
        aluP(7) <= nineBits(7);				
        if nineBits(7 downto 0) = "00000000" then
          aluP(1) <= '1';
        else
          aluP(1) <= '0';
        end if;			
        aluOut <= nineBits(7 downto 0);
      when X"C6" | X"CE" | X"D6" | X"DE" => -- DEC
        nineBits := ("0" & register_T) - 1;
        aluP(7) <= nineBits(7);
        if nineBits(7 downto 0) = "00000000" then
          aluP(1) <= '1';
        else
          aluP(1) <= '0';
        end if;			
        aluOut <= nineBits(7 downto 0);
      when X"CA" => -- DEX
        nineBits := ("0" & register_X) - 1;
        aluP(7) <= nineBits(7);
        if nineBits(7 downto 0) = "00000000" then
          aluP(1) <= '1';
        else
          aluP(1) <= '0';
        end if;			
        aluOut <= nineBits(7 downto 0);
      when X"88" => -- DEY
        nineBits := ("0" & register_Y) - 1;
        aluP(7) <= nineBits(7);
        if nineBits(7 downto 0) = "00000000" then
          aluP(1) <= '1';
        else
          aluP(1) <= '0';
        end if;			
        aluOut <= nineBits(7 downto 0);
      when X"41" | X"45" | X"49" | X"4D" | X"51" |
        X"55" | X"59" | X"5D" => -- EOR
        nineBits := ("0" & register_A) xor ("0" & aluRight);
        aluP(7) <= nineBits(7);
        if nineBits(7 downto 0) = "00000000" then
          aluP(1) <= '1';
        else
          aluP(1) <= '0';
        end if;			
        aluOut <= nineBits(7 downto 0);
      when X"E6" | X"EE" | X"F6" | X"FE" => -- INC
        nineBits := ("0" & aluRight) + 1;
        aluP(7) <= nineBits(7);
        if nineBits(7 downto 0) = "00000000" then
          aluP(1) <= '1';
        else
          aluP(1) <= '0';
        end if;			
        aluOut <= nineBits(7 downto 0);
      when X"E8" => -- INX
        nineBits := ("0" & register_X) + 1;
        aluP(7) <= nineBits(7);
        if nineBits(7 downto 0) = "00000000" then
          aluP(1) <= '1';
        else
          aluP(1) <= '0';
        end if;			
        aluOut <= nineBits(7 downto 0);
      when X"C8" => -- INY
        nineBits := ("0" & register_Y) + 1;
        aluP(7) <= nineBits(7);
        if nineBits(7 downto 0) = "00000000" then
          aluP(1) <= '1';
        else
          aluP(1) <= '0';
        end if;			
        aluOut <= nineBits(7 downto 0);
      when X"46" | X"4E" | X"56" | X"5E" => -- LSR
        nineBits := "00" & register_T(7 downto 1);
        aluP(0) <= register_T(0);
        aluP(7) <= nineBits(7);
        if nineBits(7 downto 0) = "00000000" then
          aluP(1) <= '1';
        else
          aluP(1) <= '0';
        end if;			
        aluOut <= nineBits(7 downto 0);
      when X"4A" => -- LSR A
        nineBits := "00" & register_A(7 downto 1);
        aluP(0) <= register_A(0);
        aluP(7) <= nineBits(7);
        if nineBits(7 downto 0) = "00000000" then
          aluP(1) <= '1';
        else
          aluP(1) <= '0';
        end if;			
        aluOut <= nineBits(7 downto 0);
      when X"01" | X"05" | X"09" | X"0D" | X"11" |
        X"15" | X"19" | X"1D" => -- ORA
        nineBits := ("0" & register_A) or ("0" & register_T);
        aluP(7) <= nineBits(7);
        if nineBits(7 downto 0) = "00000000" then
          aluP(1) <= '1';
        else
          aluP(1) <= '0';
        end if;			
        aluOut <= nineBits(7 downto 0);
      when X"26" | X"2E" | X"36" | X"3E" => -- ROL
        nineBits := "0" & register_T(6 downto 0) & aluPIn(0);
        aluP(0) <= register_T(7);
        aluP(7) <= nineBits(7);
        if nineBits(7 downto 0) = "00000000" then
          aluP(1) <= '1';
        else
          aluP(1) <= '0';
        end if;			
        aluOut <= nineBits(7 downto 0);
      when X"2A" => -- ROL A
        nineBits := "0" & register_A(6 downto 0) & aluPIn(0);
        aluP(0) <= register_A(7);
        aluP(7) <= nineBits(7);
        if nineBits(7 downto 0) = "00000000" then
          aluP(1) <= '1';
        else
          aluP(1) <= '0';
        end if;			
        aluOut <= nineBits(7 downto 0);
      when X"66" | X"6E" | X"76" | X"7E" => -- ROR
        nineBits := "0" & aluPIn(0) & register_T(7 downto 1);
        aluP(0) <= register_T(0);
        aluP(7) <= nineBits(7);
        if nineBits(7 downto 0) = "00000000" then
          aluP(1) <= '1';
        else
          aluP(1) <= '0';
        end if;			
        aluOut <= nineBits(7 downto 0);
      when X"6A" => -- ROR A
        nineBits := "0" & aluPIn(0) & register_A(7 downto 1);
        aluP(0) <= register_A(0);
        aluP(7) <= nineBits(7);
        if nineBits(7 downto 0) = "00000000" then
          aluP(1) <= '1';
        else
          aluP(1) <= '0';
        end if;			
        aluOut <= nineBits(7 downto 0);
      when X"00" | X"08" => -- feed through P
        nineBits := "0" & aluPIn;
        if nineBits(7 downto 0) = "00000000" then
          aluP(1) <= '1';
        else
          aluP(1) <= '0';
        end if;
        ninebits(4) := irq_ff;
        aluOut <= nineBits(7 downto 0);
      when X"48" | X"81" | X"85" | X"8D" | X"91" |
        X"95" | X"99" | X"9D" | X"A8" | X"AA" => -- feed through A
        nineBits := "0" & register_A;
        aluP(7) <= nineBits(7);
        if nineBits(7 downto 0) = "00000000" then
          aluP(1) <= '1';
        else
          aluP(1) <= '0';
        end if;			
        aluOut <= nineBits(7 downto 0);
      when X"86" | X"8A" | X"8E" | X"96" | X"9A" => -- feed through X
        nineBits := "0" & register_X;
        aluP(7) <= nineBits(7);
        if nineBits(7 downto 0) = "00000000" then
          aluP(1) <= '1';
        else
          aluP(1) <= '0';
        end if;			
        aluOut <= nineBits(7 downto 0);
      when X"84" | X"8C" | X"94" | X"98" => -- feed through Y
        nineBits := "0" & register_Y;
        aluP(7) <= nineBits(7);
        if nineBits(7 downto 0) = "00000000" then
          aluP(1) <= '1';
        else
          aluP(1) <= '0';
        end if;			
        aluOut <= nineBits(7 downto 0);
      when others => -- feed through
        nineBits := "0" & aluRight;
        aluP(7) <= nineBits(7);
        if nineBits(7 downto 0) = "00000000" then
          aluP(1) <= '1';
        else
          aluP(1) <= '0';
        end if;			
        aluOut <= nineBits(7 downto 0);
    end case;
    
  end process;		

  mainStateMachine : process(clk)
    variable nineBits : unsigned(8 downto 0);
  begin
    if rising_edge(clk) then
      if pre_phase_zero = '0' then
        case cpuCycle is
          when getOpcode =>
            opcode <= D_IN;
            aluPIn <= register_P;
            nmi_ff <= nmi_n;
            irq_ff <= irq_n or register_P(2);
            if (nmi_ff = '1') and (nmi_n = '0') then
              opcode <= X"00";
            elsif (irq_n or register_P(2)) = '1' then
              register_PC <= register_PC +1;
            else
              opcode <= X"00";
            end if;
          when secondFetch =>
            register_T <= D_IN;
            register_L <= D_IN;
            register_H <= (others => '0');
          when stackRead | stackLow | stackHigh =>
            register_T <= D_IN;
          when readH_Pc =>
            register_H <= D_IN;
          when readPch_Pc | readPch_Hl =>
            register_PC(7 downto 0) <= register_T;
            register_PC(15 downto 8) <= D_IN;
          when indX_L | indY_L =>
            register_L <= D_IN;
          when indX_H | indY_H =>
            register_H <= D_IN;
          when useHl =>
            if rmw(2) = '1' then
              register_T <= D_IN;
            end if;
          when others =>
            null;
        end case;
      else
        case cpuCycle is
          when getOpcode =>
            cpuCycle <= secondFetch;
          when secondFetch =>
                                        -- rmw = "---";
            case opcode is
              when X"00" => -- BRK
                cpuCycle <= pushPch;
              when X"EA" => -- NOP
                cpuCycle <= getOpcode;
              when X"09" | X"29" | X"49" | X"69" |
                X"A9" | X"E9" => -- ADC / AND / EOR / ORA / LDA imm
                register_A <= aluOut;
                register_P <= aluP;
                register_PC <= register_PC +1;
                cpuCycle <= getOpcode;
              when X"A2" => -- LDX imm
                register_X <= aluOut;
                register_P <= aluP;
                register_PC <= register_PC +1;
                cpuCycle <= getOpcode;
              when X"A0" => -- LDY imm
                register_Y <= aluOut;
                register_P <= aluP;
                register_PC <= register_PC +1;
                cpuCycle <= getOpcode;
              when X"C0" | X"C9" | X"E0" => -- CMP, CPX, CPY imm
                register_P <= aluP;
                register_PC <= register_PC +1;
                cpuCycle <= getOpcode;
              when X"0D" | X"19" | X"1D" |
                X"2C" | X"2D" | X"39" | X"3D" |
                X"4D" | X"59" | X"5D" |
                X"6D" | X"79" | X"7D" |
                X"AC" | X"AD" | X"AE" | X"B9" |
                X"BC" | X"BD" | X"BE" |
                X"CC" | X"CD" | X"D9" | X"DD" |
                X"EC" | X"ED" | X"F9" | X"FD" => -- read abs / abs,X / abs,Y
                rmw <= "100";
                register_PC <= register_PC + 1;
                cpuCycle <= readH_Pc;
              when X"0E" | X"1E" | X"2E" | X"3E" | X"4E" |
                X"5E" | X"6E" | X"7E" | X"CE" | X"DE" |
                X"EE" | X"FE" => -- read/modify/write abs / abs,X / abs,Y
                rmw <= "111";
                register_PC <= register_PC + 1;
                cpuCycle <= readH_Pc;
              when X"8C" | X"8D" | X"8E" | X"99" |
                X"9D" => -- write abs / abs,X / abs,Y
                rmw <= "001";
                register_PC <= register_PC + 1;
                cpuCycle <= readH_Pc;
              when X"05" | X"24" | X"25" | X"45" | X"65" | X"A4" | X"A5" |
                X"A6" | X"C4" | X"C5" | X"E4" | X"E5" => -- read zeropage
                rmw <= "100";
                register_PC <= register_PC + 1;
                cpuCycle <= useHl;
              when X"06" | X"26" | X"46" | X"66" |
                X"C6" | X"E6" => -- read/modify/write zeropage
                rmw <= "111";
                register_PC <= register_PC + 1;
                cpuCycle <= useHl;
              when X"84" | X"85" | X"86" => -- write zeropage
                rmw <= "001";
                register_PC <= register_PC + 1;
                cpuCycle <= useHl;
              when X"15" | X"35" | X"55" | X"75" | X"B4" |
                X"B5" | X"D5" | X"F5" => -- read zeropage,x
                rmw <= "100";
                register_PC <= register_PC + 1;
                cpuCycle <= addXtoL;
              when X"16" | X"36" | X"56" | X"76" |
                X"D6" | X"F6" => -- read/modufy/write zeropage,x
                rmw <= "111";
                register_PC <= register_PC + 1;
                cpuCycle <= addXtoL;
              when X"94" | X"95" => -- write zeropage,x
                rmw <= "001";
                register_PC <= register_PC + 1;
                cpuCycle <= addXtoL;
              when X"B6" => -- read zeropage,y
                rmw <= "100";
                register_PC <= register_PC + 1;
                cpuCycle <= addYtoL;
              when X"96" => -- write zeropage,y
                rmw <= "001";
                register_PC <= register_PC + 1;
                cpuCycle <= addYtoL;
              when X"01" | X"21" | X"41" | X"61" | X"A1" |
                X"C1" | X"E1"  => -- read zeropage indirect,x
                rmw <= "100";
                register_PC <= register_PC + 1;
                cpuCycle <= indX_dummy;
              when X"81" => -- write zeropage indirect,x
                rmw <= "001";
                register_PC <= register_PC + 1;
                cpuCycle <= indX_dummy;
              when X"11" | X"31" | X"51" | X"71" | X"B1" |
                X"D1" | X"F1" => -- read zeropage indirect,y
                rmw <= "100";
                register_PC <= register_PC + 1;
                cpuCycle <= indY_L;
              when X"91" => -- write zeropage indirect,y
                rmw <= "001";
                register_PC <= register_PC + 1;
                cpuCycle <= indY_L;

                                        --
                                        -- Accumulator
              when X"0A" | X"2A" | X"4A" | X"6A" => -- ASL, ROL, LSR, ROR
                register_A <= aluOut;
                register_P <= aluP;
                cpuCycle <= getOpcode;

                                        --
                                        -- Jump instructions
              when X"20" => -- JSR abs
                register_PC <= register_PC +1;
                cpuCycle <= prepare_S;
              when X"4C" => -- JMP abs
                register_PC <= register_PC +1;
                cpuCycle <= readPch_Pc;
              when X"6C" => -- JMP absIndirect
                rmw <= "100";
                register_PC <= register_PC +1;
                cpuCycle <= readH_Pc;

                                        --
                                        -- Stack
              when X"28" | X"40" | X"60" | X"68" => -- PLA, RTI, RTS, PLP
                cpuCycle <= prepare_S;
              when X"08" | X"48" => -- PHP, PHA
                cpuCycle <= stackWrite;

                                        --
                                        -- Relative
              when X"10" => -- BPL
                register_PC <= register_PC +1;
                if register_P(7) = '0' then
                  cpuCycle <= branch;
                else
                  cpuCycle <= getOpcode;
                end if;
              when X"30" => -- BMI
                register_PC <= register_PC +1;
                if register_P(7) = '1' then
                  cpuCycle <= branch;
                else
                  cpuCycle <= getOpcode;
                end if;
              when X"50" => -- BVC
                register_PC <= register_PC +1;
                if register_P(6) = '0' then
                  cpuCycle <= branch;
                else
                  cpuCycle <= getOpcode;
                end if;
              when X"70" => -- BVS
                register_PC <= register_PC +1;
                if register_P(6) = '1' then
                  cpuCycle <= branch;
                else
                  cpuCycle <= getOpcode;
                end if;
              when X"90" => -- BCC
                register_PC <= register_PC +1;
                if register_P(0) = '0' then
                  cpuCycle <= branch;
                else
                  cpuCycle <= getOpcode;
                end if;
              when X"B0" => -- BCS
                register_PC <= register_PC +1;
                if register_P(0) = '1' then
                  cpuCycle <= branch;
                else
                  cpuCycle <= getOpcode;
                end if;
              when X"D0" => -- BNE
                register_PC <= register_PC +1;
                if register_P(1) = '0' then
                  cpuCycle <= branch;
                else
                  cpuCycle <= getOpcode;
                end if;
              when X"F0" => -- BEQ
                register_PC <= register_PC +1;
                if register_P(1) = '1' then
                  cpuCycle <= branch;
                else
                  cpuCycle <= getOpcode;
                end if;

                                        --
                                        -- Implied increment/decrement
              when X"88" | X"C8" => -- DEY, INY
                register_Y <= aluOut;
                register_P <= aluP;
                cpuCycle <= getOpcode;
              when X"CA" | X"E8" => -- DEX, INX
                register_X <= aluOut;
                register_P <= aluP;
                cpuCycle <= getOpcode;


                                        --
                                        -- Implied (transfer)
              when X"8A" | X"98" => -- TXA, TYA
                register_A <= aluOut;
                register_P <= aluP;
                cpuCycle <= getOpcode;
              when X"9A" => -- TXS
                register_S <= aluOut;
                cpuCycle <= getOpcode;
              when X"AA" | X"BA" => -- TAX, TSX
                register_X <= aluOut;
                register_P <= aluP;
                cpuCycle <= getOpcode;
              when X"A8" => -- TAY
                register_Y <= aluOut;
                register_P <= aluP;
                cpuCycle <= getOpcode;

                                        --
                                        -- Implied (set/reset flags)
              when X"18" | X"38" | X"58" | X"78" | X"B8" | X"D8" | X"F8" =>
                register_P <= aluP;
                cpuCycle <= getOpcode;
              when others =>
                null;
            end case;
          when prepare_S =>
                                        -- rmw = "---";
            case opcode is
              when X"20" => -- JSR abs (3)
                cpuCycle <= pushPch;
              when X"60" => -- RTS
                register_S <= register_S + 1;
                cpuCycle <= stackLow;
              when X"28" | X"40" | X"68" => -- PLA, RTI, PLP
                register_S <= register_S + 1;
                cpuCycle <= stackRead;
              when others =>
                null;
            end case;
          when stackRead => -- PLP, PLA, RTI_P
                                        -- rmw = "---";
            case opcode is
              when X"28" => -- PLP
                register_P <= register_T;
                cpuCycle <= getOpcode;
              when X"40" => -- RTI
                register_P <= register_T;
                register_S <= register_S + 1;
                cpuCycle <= stackLow;
              when X"68" => -- PLA
                register_A <= aluOut;
                register_P <= aluP;
                cpuCycle <= getOpcode;
              when others =>
                null;
                --	cpuCycle <= getOpcode;
            end case;
          when stackWrite => -- PHP, PHA
                                         -- rmw = "---";
            register_S <= register_S - 1;
            case opcode is
              when X"00" => -- BRK or IRQ
                -- !!! quick hack to make irq work
                if nmi_ff = '0' then
                  register_PC <= X"FFFA";
                else
                  register_PC <= X"FFFE";
                end if;
                opcode <= X"4C";
                register_P(2) <= '1'; -- set irq flag
                cpuCycle <= secondFetch;
              when others =>
                cpuCycle <= getOpcode;
            end case;
          when stackLow =>
                                        -- rmw = "---";
            register_S <= register_S + 1;
            register_PC(7 downto 0) <= register_T;
            cpuCycle <= stackHigh;
          when stackHigh =>
                                        -- rmw = "---";
            register_PC(15 downto 8) <= register_T;
            case opcode is
              when X"60" => -- RTS
                cpuCycle <= incrementPc;
              when others =>
                cpuCycle <= getOpcode;
            end case;
          when pushPch =>
                                        -- JSR abs (4)
                                        -- rmw = "---";
            register_S <= register_S - 1;
            cpuCycle <= pushPcl;
          when pushPcl =>
                                        -- JSR abs (5)
                                        -- rmw = "---";
            register_S <= register_S - 1;
            case opcode is
              when X"00" =>
                cpuCycle <= stackWrite;
              when others =>
                cpuCycle <= readPch_Pc;
            end case;
          when readPch_Pc =>
                                        -- JSR abs (6)
                                        -- JMP abs (3)
                                        -- rmw = "---";
            cpuCycle <= getOpcode;
          when readPch_Hl =>
                                        -- JMP indirect
                                        -- rmw = "---";
            cpuCycle <= getOpcode;
          when addXtoL => -- zeropage,X
            register_L <= register_T + register_X;
            cpuCycle <= useHl;
          when addYtoL => -- zeropage,Y
            register_L <= register_T + register_Y;
            cpuCycle <= useHl;
          when readH_Pc =>
            register_PC <= register_PC + 1;
            cpuCycle <= useHl;
            case opcode is
              when X"1D" | X"1E" | X"3D" | X"3E" | X"5D" | X"5E" | X"7E" |
                X"9D" | X"BC" | X"BD" | X"DD" | X"DE" | X"7D" |
                X"FD" | X"FE" => -- abs,X
                ninebits := ("0" & register_L) + ("0" & register_X);
                register_L <= ninebits(7 downto 0);
                if ninebits(8) = '1' then
                  cpuCycle <= fixH;
                end if;
              when X"19" | X"39" | X"59" | X"79" | X"99" | X"B9" | X"BE" |
                X"D9" | X"F9" => -- abs,Y
                ninebits := ("0" & register_L) + ("0" & register_Y);
                register_L <= ninebits(7 downto 0);
                if ninebits(8) = '1' then
                  cpuCycle <= fixH;
                end if;
              when others =>
                null;
            end case;
          when indX_dummy =>
            register_T <= register_T + register_X;
            cpuCycle <= indx_L;
          when indX_L =>
            register_T <= register_T + 1;
            cpuCycle <= indx_H;
          when indX_H =>
            cpuCycle <= useHl;
          when indY_L =>
            register_T <= register_T + 1;
            cpuCycle <= indY_H;
          when indY_H =>
            ninebits := ("0" & register_L) + ("0" & register_Y);
            register_L <= ninebits(7 downto 0);
            if ninebits(8) = '1' then
              cpuCycle <= fixH;
            else
              cpuCycle <= useHl;
            end if;
          when fixH =>
            register_H <= register_H + 1;
            cpuCycle <= useHl;
          when useHl =>
            case opcode is
              when X"01" | X"05" | X"0D" |
                X"11" | X"15" | X"19" | X"1D" |
                X"21" | X"25" | X"2D" |
                X"31" | X"35" | X"39" | X"3D" |
                X"41" | X"45" | X"4D" |
                X"51" | X"55" | X"59" | X"5D" |
                X"61" | X"65" | X"6D" |
                X"71" | X"75" | X"79" | X"7D" |
                X"A1" | X"A5" | X"AD" |
                X"B1" | X"B5" | X"B9" | X"BD" |
                X"E1" | X"E5" | X"ED" |
                X"F1" | X"F5" | X"F9" | X"FD" =>
                register_A <= aluOut;
                register_P <= aluP;
              when X"A6" | X"AE" | X"B6" | X"BE" =>
                register_X <= aluOut;
                register_P <= aluP;
              when X"A4" | X"AC" | X"B4" | X"BC" =>
                register_Y <= aluOut;
                register_P <= aluP;
              when X"24" | X"2C" |
                X"C1" | X"C4" | X"C5" | X"CC" | X"CD" |
                X"D1" | X"D5" | X"D9" | X"DD" | X"E4" | X"EC" => -- BIT, CMP
                register_P <= aluP;
              when others =>
                null;
            end case;
            case rmw is
              when "111" =>
                rmw <= "011";
              when "011" =>
                register_P <= aluP;
                rmw <= "001";
              when others =>
                                        -- rmw = "---";
                if opcode = X"6C" then
                  register_L <= register_L + 1;
                  cpuCycle <= readPch_Hl;
                else
                  cpuCycle <= getOpcode;
                end if;
            end case;
          when incrementPc =>
                                        -- rmw = "---";
            register_PC <= register_PC + 1;
            cpuCycle <= getOpcode;
          when branch =>
                                        -- rmw = "---";
            ninebits := ("0" & register_PC(7 downto 0)) + ("0" & register_T);
            register_PC(7 downto 0) <= ninebits(7 downto 0);
            if ninebits(8) = register_T(7) then
              cpuCycle <= getOpcode;
            else
              -- process page crossing during branch in an extra cycle
              cpuCycle <= branchFixPch;
            end if;
          when branchFixPch =>
                                        -- rmw = "---";
            register_PC(15 downto 8) <=
              register_PC(15 downto 8) +
              (register_T(7) & register_T(7) & register_T(7) &
               register_T(7) & register_T(7) & register_T(7) &
               register_T(7) & "1");
            cpuCycle <= getOpcode;
          when others =>
            null;
        end case;
      end if;
      if reset = '1' then
        opcode <= X"4C";
        register_PC <= X"FFFC";
        cpuCycle <= secondFetch;
      end if;
      register_P(5) <= '1';
    end if;

  end process;

  procWriteEnable: process(cpuCycle, rmw)
  begin
    case cpuCycle is
      when pushPch | pushPcl | stackWrite =>
        writeEnable <= '1';
      when useHl =>
        if rmw = "011" or rmw = "001" then
          writeEnable <= '1';
        else
          writeEnable <= '0';
        end if;
      when others =>
        writeEnable <= '0';
    end case;
  end process;

  cpuBus: process(aluOut, cpuCycle, register_S, register_PC,
                  register_H, register_L, register_T)
  begin
    D_OUT <= aluOut;
    case cpuCycle is
      when prepare_S | stackRead | stackWrite | stackLow | stackHigh =>
        A <= "00000001" & register_S;
      when pushPch =>
        D_OUT <= register_PC(15 downto 8);
        A <= "00000001" & register_S;
      when pushPcl =>
        D_OUT <= register_PC(7 downto 0);
        A <= "00000001" & register_S;
      when useHl | readPch_Hl | addXtoL | fixH =>
        A <= register_H & register_L;
      when indX_dummy | indY_L | indY_H | indX_L | indX_H =>
        A <= "00000000" & register_T;
      when others =>
        A <= register_PC;
    end case;
  end process;

  R_W_N <= not writeEnable;
  
  pcDebugOut <= register_PC;
  opcodeDebugOut <= opcode;
  
end Behavioral;
