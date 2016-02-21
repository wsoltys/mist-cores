library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity block_io is

  port (
    
    -- link to user_io for io controller
    io_lba          : out std_logic_vector(31 downto 0);
    io_rd           : out std_logic;
    io_wr           : out std_logic;
    io_ack          : in  std_logic;
    io_conf         : out std_logic;
    io_sdhc         : out std_logic;
    
    -- data coming in from io controller
    io_din          : in  std_logic_vector(7 downto 0);
    io_din_strobe   : in  std_logic;
    
    -- data going out to io controller
    io_dout         : out std_logic_vector(7 downto 0);
    io_dout_strobe  : in  std_logic;
    
    
    clk             : in  std_logic;
    clk2            : in  std_logic;
    reset           : in  std_logic;
    status_o        : out std_logic_vector(7 downto 0);
    
    -- Track buffer Interface ------------------
    ram_write_addr  : out unsigned(12 downto 0);
    ram_di          : out unsigned(7 downto 0);
    ram_we          : out std_logic;
    change          : in  std_logic;             -- Force reload as disk may have changed
    track           : in  unsigned(5 downto 0);  -- Track number (0-34)
    busy            : out std_logic
    

  );

end block_io;

architecture rtl of block_io is

  type states is (INIT, IDLE, PREPARE_BLOCK, READ_BLOCK);

  signal state : states:= IDLE;
  signal buffer_wptr   : unsigned (12 downto 0);
  signal addr          : std_logic_vector (12 downto 0);
  signal ram_din       : std_logic_vector (7 downto 0);
  signal current_track : unsigned(5 downto 0);
  signal io_rd_req     : std_logic;
  signal we            : std_logic;
  signal io_din_strobe2: std_logic;
  signal test          : std_logic := '0';
  signal sector        : unsigned(3 downto 0);
  signal busy_rd       : std_logic;
  signal io_rd_b       : std_logic;
  --signal track         : unsigned(5 downto 0) := (others=>'0');
  
  signal status : std_logic_vector(7 downto 0);
  signal reload : std_logic;

begin

  status_o <= status;
  io_wr   <= '0'; -- we don't write yet
  io_dout <= (others => '0');
  io_conf <= '0';
  io_sdhc <= '1';
  --ram_we  <= '0';
  
  ram_write_addr <= unsigned(addr);
  ram_di         <= unsigned(ram_din);
  ram_we         <= we;
  
--  tcache : entity work.trackcache
--    port map (
--      address => addr,
--      clock   => clk,
--      data    => ram_din,
--      wren    => we
--      --q       => open
--    );

  -- set reload flag whenever "change" rises and clear it once the	
	-- state machine starts reloading the track
	process(change, state)
	begin
	   if(state = READ_BLOCK) then
			reload <= '0';
		else
			if rising_edge(change) then
				reload <= '1';
			end if;
		end if;
	end process;
    
  process(io_din_strobe)
  begin
    if reset = '0' then
      if rising_edge(io_din_strobe) then
        addr <= std_logic_vector(resize(buffer_wptr+(sector*X"0200"), addr'length));
        ram_din <= io_din;
        --we <= '1';
      end if;
    end if;
  end process;
  
  process(io_din_strobe, io_rd_req)
  begin
    if reset = '0' then
      if io_rd_req = '1' then
        buffer_wptr <= (others=>'0');
      elsif falling_edge(io_din_strobe) then
        buffer_wptr <= buffer_wptr + 1;
      end if;
    end if;
  end process;
  
  process(io_rd_req, io_ack)
  begin
    if reset = '0' then
      if io_rd_req = '1' and io_ack = '0' then
        io_rd_b <= '1';
      elsif rising_edge(io_ack) then
        io_rd_b <= '0';
      end if;
    end if;
  end process;
  
  io_rd <= io_rd_b;
  
  process(io_rd_b, buffer_wptr)
  begin
    if io_rd_b = '1' or io_ack = '1' then
      busy_rd <= '1';
    else
      busy_rd <= '0';
    end if;
  end process;
  
  process(clk)
  begin
    if rising_edge(clk) then
      we <= '0';
      if reset = '1' then
        current_track <= (others=>'1');
        io_rd_req <= '0';
        test <= '0';
        sector <= (others=>'0');
        state <= IDLE;
        status <= x"00";
      else
        
        io_rd_req <= '0';
        
        case state is
          when IDLE =>
            status <= x"01";
            if reload = '1' or track /= current_track then
              current_track <= track;
              state <= PREPARE_BLOCK;
              busy <= '1';
            end if;
            busy <= '0';
            
          when PREPARE_BLOCK =>
            status <= x"02";
            io_rd_req <= '1';
            io_lba <= std_logic_vector(resize (current_track * X"1A00" + sector * X"0200" srl 9, io_lba'length));
            state <= READ_BLOCK;
          
          when READ_BLOCK =>
            we <= '1';
            status <= x"03";
            if busy_rd = '0' and sector <= 11 then
              sector <= sector + 1;
              state <= PREPARE_BLOCK;
            elsif busy_rd = '0' and sector = 12 then
              sector <= (others=>'0');
              state <= IDLE;
            end if;            
             
          when others => null;
        end case;
          
      end if;
    end if;
  end process;

end rtl;