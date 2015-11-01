library ieee ;
  use ieee.std_logic_1164.all ;
  use ieee.std_logic_unsigned.all;
  use ieee.numeric_std.all;
  
  
entity MOCKINGBOARD is
  port (
  
    I_ADDR            : in std_logic_vector(7 downto 0);
    I_DATA            : in std_logic_vector(7 downto 0);
    O_DATA            : in std_logic_vector(7 downto 0);
    
    I_RW_L            : in std_logic;
    O_IRQ_L           : out std_logic;
    O_NMI_L           : out std_logic;
    I_IOSEL_L         : in std_logic;
    I_RESET_L         : in std_logic;
    
    O_AUDIO_L         : out std_logic;
    O_AUDIO_R         : out std_logic;
    CLK               : in std_logic
    );
 end;
 
 
 architecture RTL of MOCKINGBOARD is
 
  signal o_pb_l           : std_logic_vector(7 downto 0);
  signal o_pb_r           : std_logic_vector(7 downto 0);
  
  signal i_psg_r          : std_logic_vector(7 downto 0);
  signal i_psg_l          : std_logic_vector(7 downto 0);
  signal o_psg_r          : std_logic_vector(7 downto 0);
  signal o_psg_l          : std_logic_vector(7 downto 0);
  signal o_psg_ar         : std_logic_vector(7 downto 0);
  signal o_psg_al         : std_logic_vector(7 downto 0);
  
  signal o_data_l          : std_logic_vector(7 downto 0);
  signal o_data_r          : std_logic_vector(7 downto 0);
  
  signal data_oe_l        : std_logic;
  
begin

  O_DATA <= o_data_l when data_oe_l = '0' else o_data_r;

-- Left Channel Combo

  m6522_left : M6522
    port map (
      I_RS        => I_ADDR(3 downto 0);
      I_DATA      => I_DATA,
      O_DATA      => o_data_l,
      O_DATA_OE_L => data_oe_l,
  
      I_RW_L      => I_RW_L,
      I_CS1       => not I_ADDR(7),
      I_CS2_L     => I_IOSEL_L,
  
      O_IRQ_L     => O_IRQ_L,
      -- port a
      I_CA1       => '0',
      I_CA2       => '0',
      O_CA2       => open,
      O_CA2_OE_L  => open,
  
      I_PA        => o_psg_l,
      O_PA        => i_psg_l,
      O_PA_OE_L   => open,
  
      -- port b
      I_CB1       => '0',
      O_CB1       => open,
      O_CB1_OE_L  => open,
  
      I_CB2       => '0',
      O_CB2       => open,
      O_CB2_OE_L  => open,
  
      I_PB        => (others => '0'),
      O_PB        => o_pb_l,
      O_PB_OE_L   => open,
  
      I_P2_H      =>
      RESET_L     => I_RESET_L,
      ENA_4       =>
      CLK         => CLK
      );
      
      
  psg_left : YM2149
    port map (
      -- data bus
      I_DA        => i_psg_l,
      O_DA        => o_psg_l,
      O_DA_OE_L   => open,
      -- control
      I_A9_L      => '0', -- /A9 pulled down internally
      I_A8        => '1',
      I_BDIR      => o_pb_l(1),
      I_BC2       => '1',
      I_BC1       => o_pb_l(0),
      I_SEL_L     => '1', -- /SEL is high for AY-3-8912 compatibility
    
      O_AUDIO     => o_psg_al,
      -- port a
      I_IOA       => (others => '0'), -- port A unused
      O_IOA       => open,
      O_IOA_OE_L  => open,
      -- port b
      I_IOB       => (others => '0'), -- port B unused
      O_IOB       => open,
      O_IOB_OE_L  => open,
      --
      ENA         => 
      RESET_L     => o_pb_l(2),
      CLK         => CLK
      );

  dac_l : sigma_delta_dac
    port map (
      CLK		=> CLK,
      RESET 	=> I_RESET_L,
      DACin 	=> o_psg_al,
      DACout	=> O_AUDIO_L
      );


-- Right Channel Combo

  m6522_right : M6522
    port map (
      I_RS        => I_ADDR(3 downto 0);
      I_DATA      => I_DATA,
      O_DATA      => o_data_r,
      O_DATA_OE_L => open,
  
      I_RW_L      => I_RW_L,
      I_CS1       => I_ADDR(7),
      I_CS2_L     => I_IOSEL_L,
  
      O_IRQ_L     => O_NMI_L,
      -- port a
      I_CA1       => '0',
      I_CA2       => '0',
      O_CA2       => open,
      O_CA2_OE_L  => open,
  
      I_PA        => o_psg_r,
      O_PA        => i_psg_r,
      O_PA_OE_L   => open,
  
      -- port b
      I_CB1       => '0',
      O_CB1       => open,
      O_CB1_OE_L  => open,
  
      I_CB2       => '0',
      O_CB2       => open,
      O_CB2_OE_L  => open,
  
      I_PB        => (others => '0'),
      O_PB        => o_pb_r,
      O_PB_OE_L   => open,
  
      I_P2_H      =>
      RESET_L     => I_RESET_L,
      ENA_4       =>
      CLK         =>
      );
      
      
  psg_right : YM2149
    port map (
      -- data bus
      I_DA        => i_psg_r,
      O_DA        => o_psg_r,
      O_DA_OE_L   => open,
      -- control
      I_A9_L      => '0', -- /A9 pulled down internally
      I_A8        => '1',
      I_BDIR      => o_pb_r(1),
      I_BC2       => '1',
      I_BC1       => o_pb_r(0),
      I_SEL_L     => '1', -- /SEL is high for AY-3-8912 compatibility
    
      O_AUDIO     => o_psg_ar,
      -- port a
      I_IOA       => (others => '0'), -- port A unused
      O_IOA       => open,
      O_IOA_OE_L  => open,
      -- port b
      I_IOB       => (others => '0'), -- port B unused
      O_IOB       => open,
      O_IOB_OE_L  => open,
      --
      ENA         => 
      RESET_L     => o_pb_r(2),
      CLK         =>
      );
      
  dac_r : sigma_delta_dac
    port map (
      CLK		=> CLK,
      RESET 	=> I_RESET_L,
      DACin 	=> o_psg_ar,
      DACout	=> O_AUDIO_R
      );

end architecture RTL;