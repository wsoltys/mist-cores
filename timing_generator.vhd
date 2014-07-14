-------------------------------------------------------------------------------
--
-- Apple ][ Timing logic
--
-- Stephen A. Edwards, sedwards@cs.columbia.edu
--
-- Taken more-or-less verbatim from the schematics in the
-- Apple ][ reference manual
--
-- This takes a 14.31818 MHz master clock and divides it down to generate
-- the various lower-frequency signals (e.g., 7M, phase 0, colorburst)
-- as well as horizontal and vertical blanking and sync signals for the video
-- and the video addresses.
--
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity timing_generator is
  
  port (
    CLK_14M        : in  std_logic;     -- 14.31818 MHz master clock
    CLK_7M         : out std_logic;
    Q3	           : out std_logic;    -- 2 MHz signal in phase with PHASE_ZERO
    RAS_N          : out std_logic;
    CAS_N          : out std_logic;
    AX             : out std_logic;
    PHASE_ZERO     : out std_logic;     -- 1.0 MHz processor clock
    PRE_PHASE_ZERO : out std_logic;     -- One 14M cycle before
    COLOR_REF      : out std_logic;     -- 3.579545 MHz colorburst

    TEXT_MODE      : in std_logic;
    PAGE2          : in std_logic;
    HIRES          : in std_logic;

    VIDEO_ADDRESS  : out unsigned(15 downto 0);
    H0             : out std_logic;
    VA             : out std_logic;      -- Character row address
    VB             : out std_logic;
    VC             : out std_logic;
    V2             : out std_logic;
    V4             : out std_logic;
    HBL		   : out std_logic;      -- Horizontal blanking
    VBL		   : out std_logic;      -- Vertical blanking
    BLANK          : out std_logic;      -- Composite blanking
    LDPS_N         : out std_logic;
    LD194          : out std_logic
  );

end timing_generator;

architecture rtl of timing_generator is

  signal RAS_N_sig : std_logic := '0';
  signal AX_sig : std_logic := '0';
  signal CAS_N_sig : std_logic := '0';
  signal Q3_sig : std_logic := '0';
  signal CLK_7M_sig : std_logic := '0';
  signal COLOR_REF_sig : std_logic := '0';

  signal next_Q3, next_CAS_N, next_AX, next_RAS_N : std_logic;

  signal horizontal_count : std_logic;   -- When to increase horizontal count
  
  signal PRE_PHASE_ZERO_sig : std_logic := '0';
  signal PHASE_ZERO_sig : std_logic := '0';

  signal HCOUNT : unsigned(6 downto 0) := "0000000";
  signal VCOUNT : unsigned(8 downto 0) := "011111010";

  -- Horizontal counter signals
  signal H1, H2, H3, H4, H5 : std_logic;
  signal HPE_N : std_logic;

  -- Vertical counter signals
  signal VA_I, VB_I, VC_I : std_logic;        -- Row in each character
  signal V0, V1, V3, V5 : std_logic;  -- Character row

  signal HBL_I, VBL_I : std_logic;  -- Horizontal and Vertical Blanking
  signal H_SYNC, V_SYNC, SYNC : std_logic;    -- Horizontal and Vertical Sync
  signal LD194_I : std_logic;

  signal COLOR_BURST : std_logic;
  
begin

  feed_C2: process (Q3_sig, CAS_N_sig, AX_sig, RAS_N_sig, COLOR_REF_sig,
                    PHASE_ZERO_sig, HPE_N)
  begin  -- process feed_C2  
    if Q3_sig = '1' then         -- shift
      next_Q3 <= CAS_N_sig;
      next_CAS_N <= AX_sig;
      next_AX <= RAS_N_sig;
      next_RAS_N <= '0';
    else                         -- load
      next_Q3 <= RAS_N_sig;
      next_RAS_N <= AX_sig;
      next_CAS_N <= AX_sig;
      next_AX <= not (not COLOR_REF_sig and
                      (not AX_sig and not CAS_N_sig) and
                      PHASE_ZERO_sig and not HPE_N);
    end if;
  end process;

  -- A four-bit presettable shift register
  C2_74S195: process (CLK_14M)
  begin
    if rising_edge(CLK_14M) then
      Q3_sig <= next_Q3;
      CAS_N_sig <= next_CAS_N;      
      AX_sig <= next_AX;
      RAS_N_sig <= next_RAS_N;
    end if;
  end process;

  -- The main clock signal generator: a quad D flip-flop
  B1_74S175 : process (CLK_14M)
  begin
    if rising_edge(CLK_14M) then
      COLOR_REF_sig <= CLK_7M_sig xor COLOR_REF_sig;
      CLK_7M_sig <= not CLK_7M_sig;
      PHASE_ZERO_sig <= PRE_PHASE_ZERO_sig;
      if AX_sig = '1' then
        PRE_PHASE_ZERO_sig <= not (Q3_sig xor PHASE_ZERO_sig);  -- B1 pin 10
      end if;
    end if;
  end process;

  PRE_PHASE_ZERO <= PRE_PHASE_ZERO_sig;

  LDPS_N <= not (PHASE_ZERO_sig and not AX_sig and not CAS_N_sig);
  LD194_I <= not (PHASE_ZERO_sig and not AX_sig and not CAS_N_sig and
                not CLK_7M_sig);

  LD194 <= LD194_I;

  CLK_7M <= CLK_7M_sig;
  Q3 <= Q3_sig;
  AX <= AX_sig;
  RAS_N <= RAS_N_sig;
  CAS_N <= CAS_N_sig;
  PHASE_ZERO <= PHASE_ZERO_sig;
  COLOR_REF <= COLOR_REF_sig;

  -- True the cycle before the rising edge of LDPS_N: emulates
  -- the effects of using LDPS_N as the clock for the video counters
  horizontal_count <= PHASE_ZERO_sig and not AX_sig and next_AX;

  -- Four four-bit presettable binary counters
  -- Seven-bit horizontal counter counts 0, 40, 41, ..., 7F (65 states)
  -- Nine-bit vertical counter counts $FA .. $1FF  (262 states)
  D11D12D13D14_74LS161 : process (CLK_14M)
  begin
    if rising_edge(CLK_14M) then
      if horizontal_count = '1' then
        if HPE_N = '0' then
          HCOUNT <= "1000000";
        else
          HCOUNT <= HCOUNT + 1;
          if HCOUNT = "1111111" then
            VCOUNT <= VCOUNT + 1;
            if VCOUNT = "111111111" then
              VCOUNT <= "011111010";
            end if;
          end if;
        end if;
      end if;
    end if;
    
  end process;

  H0 <= HCOUNT(0);
  H1 <= HCOUNT(1);
  H2 <= HCOUNT(2);
  H3 <= HCOUNT(3);
  H4 <= HCOUNT(4);
  H5 <= HCOUNT(5);

  -- Used to extend one 1 MHz cycle out of 65 by enough to keep
  -- the 3.579545 MHz colorburst frequency in phase for each line
  HPE_N <= HCOUNT(6);

  VA_I <= VCOUNT(0);
  VA <= VA_I;
  VB_I <= VCOUNT(1);
  VB <= VB_I;
  VC_I <= VCOUNT(2);
  VC <= VC_I;
  V0 <= VCOUNT(3);
  V1 <= VCOUNT(4);
  V2 <= VCOUNT(5);
  V3 <= VCOUNT(6);
  V4 <= VCOUNT(7);
  V5 <= VCOUNT(8);

  HBL_I <= not (H5 or (H3 and H4));
  HBL <= HBL_I;
  VBL_I <= V3 and VCOUNT(7);
  VBL <= VBL_I;

  BLANK <= HBL_I or VBL_I;

  V_SYNC <= VBL_I and VCOUNT(5) and not V1 and not V0 and
            not VC_I and (H4 or H3 or H5);
  H_SYNC <= HBL_I and H3 and not H2;

  SYNC <= not (V_SYNC or H_SYNC);

  COLOR_BURST <= HBL_I and H2 and H3 and (COLOR_REF_sig or TEXT_MODE);

  -- Video address calculation
  VIDEO_ADDRESS(2 downto 0) <= HCOUNT(2 downto 0);
  VIDEO_ADDRESS(6 downto 3) <= (not H5    &     V3 &        H4 &  H3) +
                               (VCOUNT(7) & not H5 & VCOUNT(7) & '1') +
                               (                         "000" &  V3);
  VIDEO_ADDRESS(9 downto 7) <= VCOUNT(5 downto 3);
  VIDEO_ADDRESS(14 downto 10) <=
    (             "00" & HBL_I & PAGE2 & not PAGE2) when HIRES = '0' else
    (PAGE2 & not PAGE2 &  VC_I &  VB_I &      VA_I);

  VIDEO_ADDRESS(15) <= '0';
  
end rtl;
