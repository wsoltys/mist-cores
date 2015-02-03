--
-- mist_top.vhd.vhd
--
-- sidplayer toplevel for the MiST board
-- https://github.com/wsoltys/mist-cores
--
-- original sources: 
-- http://papilio.cc/index.php?n=Playground.C64SID
-- sid infos: 
-- http://cpansearch.perl.org/src/LALA/Audio-SID-3.11/SID_file_format.txt
--
-- Copyright (c) 2015 W. Soltys <wsoltys@gmail.com>
--
-- This source file is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published
-- by the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This source file is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

entity mist_top is

  port (
    -- Clocks
    
    CLOCK_27    : in std_logic_vector(1 downto 0); -- 27 MHz


    -- SDRAM
    SDRAM_nCS : out std_logic; -- Chip Select
    SDRAM_DQ : inout std_logic_vector(15 downto 0); -- SDRAM Data bus 16 Bits
    SDRAM_A : out std_logic_vector(12 downto 0); -- SDRAM Address bus 13 Bits
    SDRAM_DQMH : out std_logic; -- SDRAM High Data Mask
    SDRAM_DQML : out std_logic; -- SDRAM Low-byte Data Mask
    SDRAM_nWE : out std_logic; -- SDRAM Write Enable
    SDRAM_nCAS : out std_logic; -- SDRAM Column Address Strobe
    SDRAM_nRAS : out std_logic; -- SDRAM Row Address Strobe
    SDRAM_BA : out std_logic_vector(1 downto 0); -- SDRAM Bank Address
    SDRAM_CLK : out std_logic; -- SDRAM Clock
    SDRAM_CKE: out std_logic; -- SDRAM Clock Enable
    
    -- SPI
    SPI_SCK : in std_logic;
    SPI_DI : in std_logic;
    SPI_DO : out std_logic;
    SPI_SS2 : in std_logic;
    SPI_SS3 : in std_logic;
    CONF_DATA0 : in std_logic;

    -- VGA output
    

    VGA_HS,                                             -- H_SYNC
    VGA_VS : out std_logic;                             -- V_SYNC
    VGA_R,                                              -- Red[5:0]
    VGA_G,                                              -- Green[5:0]
    VGA_B : out std_logic_vector(5 downto 0);           -- Blue[5:0]
    
    -- Audio
    AUDIO_L,
    AUDIO_R : out std_logic;
    
    -- LEDG
    LED : out std_logic

    );
  
end mist_top;

architecture rtl of mist_top is


  type RAMtoSIDState is (
        stInit,
        stVersion,
        stVersion2,
        stDelay0,
        stDelay1,
        stDelay2,
        stSync,
        stWait1,
        stWait2,
        stAddr,
        stData,
        stWrite,
        stIdle
  );

  constant CONF_STR : string := "SID;SID;";

  function to_slv(s: string) return std_logic_vector is 
    constant ss: string(1 to s'length) := s; 
    variable rval: std_logic_vector(1 to 8 * s'length); 
    variable p: integer; 
    variable c: integer; 
  
  begin 
    for i in ss'range loop
      p := 8 * i;
      c := character'pos(ss(i));
      rval(p - 7 to p) := std_logic_vector(to_unsigned(c,8)); 
    end loop; 
    return rval; 

  end function; 



  component user_io
    generic ( STRLEN : integer := 0 );
  
    port ( SPI_CLK, SPI_SS_IO, SPI_MOSI :in std_logic;
           SPI_MISO : out std_logic;
           conf_str : in std_logic_vector(8*STRLEN-1 downto 0);
           joystick_0 : out std_logic_vector(5 downto 0);
           joystick_1 : out std_logic_vector(5 downto 0);
           joystick_analog_0 : out std_logic_vector(15 downto 0);
           joystick_analog_1 : out std_logic_vector(15 downto 0);
           status:    out std_logic_vector(7 downto 0);
           SWITCHES : out std_logic_vector(1 downto 0);
           BUTTONS : out std_logic_vector(1 downto 0);
           sd_sdhc : in std_logic;
           ps2_clk : in std_logic;
           ps2_kbd_clk : out std_logic;
           ps2_kbd_data : out std_logic
         );

  end component user_io;
  
  component data_io is
        port(sck: in std_logic;
             ss: in std_logic;
             sdi: in std_logic;
             downloading: out std_logic;
             size: out std_logic_vector(15 downto 0);
             clk: in std_logic;
             we: in std_logic;
             a: in std_logic_vector(14 downto 0);
             din: in std_logic_vector(7 downto 0);
             dout: out std_logic_vector(7 downto 0));
    end component;
  
  component osd
    port ( pclk, sck, ss, sdi, hs_in, vs_in, scanline_ena_h : in std_logic;
           red_in, blue_in, green_in : in std_logic_vector(5 downto 0);
           red_out, blue_out, green_out : out std_logic_vector(5 downto 0);
           hs_out, vs_out : out std_logic
         );
  end component osd;

  signal clk32m, clk25m, clk12m, clk4m, clk1m, clk12k  : std_logic;

  signal flash_clk : unsigned(22 downto 0) := (others => '0');
  signal power_on_reset : std_logic := '1';
  signal force_reset : std_logic := '0';
  signal reset : std_logic;
  
  signal switches   : std_logic_vector(1 downto 0);
  signal buttons    : std_logic_vector(1 downto 0);
  signal joy        : std_logic_vector(5 downto 0);
  signal joy0       : std_logic_vector(5 downto 0);
  signal joy1       : std_logic_vector(5 downto 0);
  signal joy_an0    : std_logic_vector(15 downto 0);
  signal joy_an1    : std_logic_vector(15 downto 0);
  signal joy_an     : std_logic_vector(15 downto 0);
  signal status     : std_logic_vector(7 downto 0);
  signal ps2Clk     : std_logic;
  signal ps2Data    : std_logic;
  signal audio      : std_logic;
  
  signal pll_locked : std_logic;
  
  signal VGA_R_O  : std_logic_vector(5 downto 0);
  signal VGA_G_O  : std_logic_vector(5 downto 0);
  signal VGA_B_O  : std_logic_vector(5 downto 0);
  signal VGA_HS_O : std_logic;
  signal VGA_VS_O : std_logic;
  
  signal stSIDnow               : RAMtoSIDState := stInit;
  signal stSIDnext              : RAMtoSIDState := stInit;
  signal sid_addr               : std_logic_vector(4 downto 0) := (others => '0');
  signal sid_dout               : std_logic_vector(7 downto 0) := (others => '0');
  signal sid_din                : std_logic_vector(7 downto 0) := (others => '0');
  signal sid_we                 : std_logic := '0';
  signal sid_px                 : std_logic := '0';
  signal sid_py                 : std_logic := '0';
  signal cycle_cnt              : std_logic_vector(20 downto 0) := (others => '0');
  
  signal audio_pwm              : std_logic := '0';
  
  signal downl    : std_logic := '0';
  signal downlr   : std_logic := '0';
  signal go       : std_logic := '0';
  signal size     : std_logic_vector(15 downto 0) := (others=>'0');
  signal ram_do   : std_logic_vector(7 downto 0);
  signal ram_ao   : std_logic_vector(14 downto 0);
  
  signal sid_ver  : std_logic_vector(7 downto 0) := (others => '0');

begin


  pll : entity work.mist_pll
    port map (
      inclk0 => CLOCK_27(0),
      c0     => clk32m,
      c1     => clk25m,
      c2     => clk12k,
      locked => pll_locked
      );
      
  divosd : entity work.clk_div
    generic map (
      DIVISOR => 2
    )
    port map (
      clk    => clk25m,
      reset  => '0',
      clk_en => clk12m
    );
    
  div4m : entity work.clk_div
    generic map (
      DIVISOR => 8
    )
    port map (
      clk    => clk32m,
      reset  => '0',
      clk_en => clk4m
    );
    
  div1m : entity work.clk_div
    generic map (
      DIVISOR => 4
    )
    port map (
      clk    => clk4m,
      reset  => '0',
      clk_en => clk1m
    );
      
      
-- VGA Output

  vga : entity work.VGASYNC
    port map (
      CLK   => clk25m,
      HSYNC => VGA_HS_O,
      VSYNC => VGA_VS_O,
      R     => VGA_R_O,
      G     => VGA_G_O,
      B     => VGA_B_O
    );
    
  -----------------------------------------------------------------------------
  -- SID 6581
  -----------------------------------------------------------------------------
    --
    -- Implementation of SID sound chip
    --
  u_sid6581 : entity work.sid6581
    port map (
            clk_1mhz    => clk1m,       -- main SID clock
            clk32       => clk32m,      -- main clock signal
            clk_DAC     => clk32m,      -- DAC clock signal, must be as high as possible for the best results
            reset       => reset,       -- high active reset signal (reset when reset = '1')
            cs          => '1',         -- "chip select", when this signal is '1' this model can be accessed
            we          => sid_we,      -- when '1' this model can be written to, otherwise access is considered as read
            addr        => sid_addr,    -- address lines (5 bits)
            di          => sid_din,     -- data in (to chip, 8 bits)
            do          => sid_dout,    -- data out (from chip, 8 bits)
            pot_x       => sid_px,      -- paddle input-X
            pot_y       => sid_py,      -- paddle input-Y
            audio_out   => audio_pwm,   -- this line outputs the PWM audio-signal
            audio_data  => open         -- audio out 18 bits
        );
    
    -----------------------------------------------------------------------------
    -- state machine control for ram_to_sid process
    sm_control : process (clk32m, reset)
    begin
        if falling_edge(clk32m) then
            if reset = '1' then
                stSIDnow <= stInit;
                go       <= '0';
            else
                downlr   <= downl;
                stSIDnow <= stSIDnext;
                if downl = '0' and downlr = '1' then
                  stSIDnow <= stInit;
                  go     <= '1';
                end if;
            end if;
        end if;
    end process;
  
    -- copy data from RAM to SID at cycle accurate rate
    -- read pointer cannot overtake write pointer and will block (wait)
    ram_to_sid : process (clk4m, stSIDnow, reset)
    begin
        if reset = '1' then
            ram_ao <= (others => '1');
            stSIDnext   <= stInit;
        elsif rising_edge(clk4m) then
            if go = '1' then
                case stSIDnow is
                    when stInit     =>
                        sid_we          <= '0';
                        ram_ao          <= (others => '0');
                        cycle_cnt       <= (others => '0');
                        sid_ver         <= (others => '0');
                        stSIDnext       <= stVersion;
                    when stVersion =>
                        sid_we          <= '0';
                        if ram_ao = x"0005" then
                          stSIDnext     <= stVersion2;
                        else
                          ram_ao        <= ram_ao + 1;
                        end if;              
                    when stVersion2 =>
                        sid_we          <= '0';
                        sid_ver         <= ram_do;
                        ram_ao          <= ram_ao + 1;
                        stSIDnext       <= stDelay0;
                    when stDelay0   =>
                        sid_we          <= '0';
                        if ((ram_ao = x"0076" and (sid_ver(1 downto 0) = "01")) or ((ram_ao = x"007C") and (sid_ver(1 downto 0) = "10"))) then
                          stSIDnext     <= stDelay1;
                        else
                          ram_ao        <= ram_ao + 1;
                        end if;
                    when stDelay1   =>
                        sid_we          <= '0';
                        cycle_cnt(17 downto 10) <= ram_do;  -- delay high
                        ram_ao          <= ram_ao + 1;
                        stSIDnext       <= stDelay2;
                    when stDelay2   =>
                        cycle_cnt(9 downto 2)  <= ram_do;       -- delay low
                        ram_ao          <= ram_ao + 1;
                        stSIDnext       <= stAddr;
                    when stAddr     =>
                        sid_addr        <= ram_do(4 downto 0);  -- address
                        ram_ao          <= ram_ao + 1;
                        stSIDnext       <= stData;
                    when stData     =>
                        sid_din         <= ram_do;                          -- value
                        ram_ao          <= ram_ao + 1;
                        stSIDnext       <= stSync;
                    when stSync     =>
                        if cycle_cnt = x"0000" then
                            stSIDnext <= stWrite;
                        else
                            cycle_cnt <= cycle_cnt - 1;             -- wait cycles x4 (since this runs at clk04)
                            stSIDnext <= stSync;
                        end if;
                    when stWrite    =>
                        sid_we      <= '1';
                        stSIDnext   <= stDelay1;
                    when others     => null;
                end case;
            end if;
        end if;
    end process;
    
    
-- MiST interfaces
  
  user_io_d : user_io
    generic map (STRLEN => CONF_STR'length)
    
    port map ( 
      SPI_CLK => SPI_SCK,
      SPI_SS_IO => CONF_DATA0,    
      SPI_MISO => SPI_DO,    
      SPI_MOSI => SPI_DI,       
      conf_str => to_slv(CONF_STR),
      status => status,   
      joystick_0 => joy0,   
      joystick_1 => joy1,
      joystick_analog_0 => joy_an0,
      joystick_analog_1 => joy_an1,
      SWITCHES => switches,   
      BUTTONS => buttons,
      sd_sdhc => '1',
      ps2_clk => clk12k,
      ps2_kbd_clk => ps2Clk,
      ps2_kbd_data => ps2Data
    );
    
  osd_inst : osd
    port map (
      pclk => clk12m,
      sdi => SPI_DI,
      sck => SPI_SCK,
      ss => SPI_SS3,
      red_in => VGA_R_O,
      green_in => VGA_G_O,
      blue_in => VGA_B_O,
      hs_in => not VGA_HS_O,
      vs_in => not VGA_VS_O,
      scanline_ena_h => '0',
      red_out => VGA_R,
      green_out => VGA_G,
      blue_out => VGA_B,
      hs_out => VGA_HS,
      vs_out => VGA_VS
    );
    
  data_io_inst: data_io
    port map(SPI_SCK, SPI_SS2, SPI_DI, downl, size, clk32m, '0', ram_ao, (others=>'0'), ram_do);
    
  SDRAM_nCAS  <= '1'; -- disable sdram
  AUDIO_L   <= audio_pwm;
  AUDIO_R   <= audio_pwm;
  
  reset <= not pll_locked or buttons(1) or status(0);

end rtl;