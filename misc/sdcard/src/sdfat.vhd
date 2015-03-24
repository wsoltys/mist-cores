library ieee, XESS;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use XESS.SdCardPckg.all;


ENTITY sdfat32 IS
  PORT(
    clk50     : in std_logic;
    reset     : in std_logic;
    
    -- to sd
    sd_cs     : out std_logic;
    sclk_o    : out std_logic;
    miso_i    : in std_logic;
    mosi_o    : out std_logic;
    
    -- to ram
    ram_addr  : out std_logic_vector(23 downto 0);
    ram_d     : out std_logic_vector(7 downto 0);
    ram_wr    : out std_logic;
    ram_busy  : out std_logic;
    
    -- file to load
    filename_i: in std_logic_vector(11*8-1 downto 0);
    file_ram_a: in std_logic_vector(23 downto 0);
    next_step : in std_logic;
    
    error_o   : out std_logic_vector(7 downto 0)
  );
END sdfat32;

ARCHITECTURE MAIN of sdfat32 IS

  type states is (INIT, READ_BLOCK1, READ_BLOCK2, FAT_CHECK, PARTITION_TYPE, CHECK_LBA, CHECK_PARTITION, READ_DIR1, COMPARE1, READ_FAT1, READ_FAT2, READ_FILE1, READ_FILE2, READ_FILE3, IDLE, ERROR);
  
  signal state : states := INIT;
  signal return_state : states;
  signal counter : unsigned(8 downto 0) := (others=>'0');

  signal clk_s : std_logic;
  signal rd_is : std_logic := '0';
  signal wr_is : std_logic := '0';
  signal continue_is : std_logic;
  signal addr_is : std_logic_vector(31 downto 0);
  signal data_is : std_logic_vector(7 downto 0);
  signal hndShk_is : std_logic;
  signal reset_is : std_logic;
  signal data_os : std_logic_vector(7 downto 0);
  signal busy_os : std_logic;
  signal busy_os_r: std_logic;
  signal hndShk_os : std_logic;
  signal error_os : std_logic_vector(15 downto 0);
  
  type block_ram is array(0 to 511) of unsigned(7 downto 0);
  signal block_mem : block_ram;
  
   
  signal search_end : std_logic;
  signal next_step_r: std_logic;
  
  
  -- FAT constants and signals
  constant card_type : CardType_t := SDHC_CARD_E;
  constant SIGNATURE_POSITION : integer :=  510;
  constant PARTITION1_TYPECODE_LOCATION : integer :=  450;
  constant PARTITION1_LBA_BEGIN_LOCATION: integer := 454;
  constant FAT_SECTOR_SIZE: integer := 512;
  
  signal block_addr : unsigned(31 downto 0) := (others=>'0');
  signal valid_partition : std_logic := '0';
  signal lba_begin       : unsigned(31 downto 0) := (others=>'0');
  signal fat_begin_lba   : unsigned(31 downto 0) := (others=>'0');
  signal cluster_begin_lba: unsigned(31 downto 0) := (others=>'0');
  signal root_dir_first_cluster: unsigned(31 downto 0) := (others=>'0');
  signal sectors_per_cluster: unsigned(7 downto 0) := (others=>'0');
  signal current_cluster: integer;
  signal current_pos: integer range 0 to 512;
  signal next_cluster: unsigned(31 downto 0) := (others=>'0');
  
  signal file_size: unsigned(31 downto 0) := (others=>'0');
  signal dir_offset: unsigned(31 downto 0) := (others=>'0');
  signal file_cluster: unsigned(31 downto 0) := (others=>'0');
  signal sector_cnt: integer range 0 to 512;
  signal cluster_cnt: integer range 0 to 16384;
  signal entry_cnt: integer range 0 to 10;
  signal sd_factor: integer range 0 to FAT_SECTOR_SIZE := 1;
  
  signal ram_addr_o  : unsigned(23 downto 0);
  
  signal filename : unsigned(11*8-1 downto 0);
  
  -- error states
  -- 01: Error in READ_BLOCK2. Read more than 512 bytes
  -- 02: MBR didn't passed FAT magic byte check
  -- 03: no FAT32 partition found
  -- 04: partition didn't passed FAT magic byte or 2 FAT or 512 bytes per sector check
  -- 05: file not found
  signal error_i : std_logic_vector(7 downto 0);
  
--  attribute keep: boolean;
--  attribute keep of byte1: signal is true;

 

BEGIN
  
  ram_addr <= std_logic_vector(ram_addr_o);
  error_o  <= error_i;
  
  -- SDHC cards use block-addressing, SD cards use byte-addressing
  sd_factor <= 1 when card_type = SDHC_CARD_E else FAT_SECTOR_SIZE;

  fsm : process(clk50, reset)
  begin
  
    if rising_edge(clk50) then
      if reset = '1' then
        state   <= INIT;
        rd_is   <= '0';
        counter <= (others=>'0');
        block_addr <= (others=>'0');
        error_i   <= (others=>'0');
      else
        next_step_r <= next_step;
        busy_os_r <= busy_os;
        case state is
          when INIT =>
            rd_is   <= '0';
            counter <= (others=>'0');
            valid_partition <= '0';
            block_addr <= (others=>'0');
            return_state <= FAT_CHECK;
            lba_begin <= (others=>'0');
            sectors_per_cluster <= (others=>'0');
            fat_begin_lba <= (others=>'0');
            cluster_begin_lba <= (others=>'0');
            root_dir_first_cluster <= (others=>'0');
            dir_offset <= (others=>'0');
            sector_cnt <= 0;
            entry_cnt <= 0;
            search_end <= '0';
            file_cluster <= (others=>'0');
            error_i   <= (others=>'0');
            if busy_os = '0' then
              state <= IDLE;
            end if;
        
          when READ_BLOCK1 =>
            addr_is <= std_logic_vector(block_addr);
            counter <= (others=>'0');
            rd_is   <= '1';
            if busy_os_r = '0' and busy_os = '1' then
              state <= READ_BLOCK2;
            end if;
              
          when READ_BLOCK2 =>
            rd_is   <= '0';
            if hndShk_os = '1' and hndShk_is = '0' then
              block_mem(to_integer(counter)) <= unsigned(data_os);
              counter  <= counter + 1;
              hndShk_is <= '1';
            elsif hndShk_os = '0' then
              hndShk_is <= '0';
            end if;
            if counter = FAT_SECTOR_SIZE then
              hndShk_is <= '0';
              error_i <= x"01";
              state <= ERROR;
            end if;
            if busy_os_r = '1' and busy_os = '0' then
              hndShk_is <= '0';
              state <= return_state;     
            end if;
          
          when FAT_CHECK =>
            if block_mem(SIGNATURE_POSITION) = x"55" and block_mem(SIGNATURE_POSITION+1) = x"AA" then
              state <= PARTITION_TYPE;
            else
              error_i <= x"02";
              state <= ERROR;
            end if;
           
          when PARTITION_TYPE =>
            -- we only support fat32 here
            case block_mem(PARTITION1_TYPECODE_LOCATION) is
              when x"0B" => valid_partition <= '1';
                            state <= CHECK_LBA;
              when x"0C" => valid_partition <= '1';
                            state <= CHECK_LBA;
              when x"00" => valid_partition <= '0';
                            state <= CHECK_LBA;
              when others => error_i <= x"03"; 
                             state <= ERROR;

            end case;
            
          when CHECK_LBA =>
            if valid_partition = '1' then
              lba_begin <= block_mem(PARTITION1_LBA_BEGIN_LOCATION+3) & block_mem(PARTITION1_LBA_BEGIN_LOCATION+2) & block_mem(PARTITION1_LBA_BEGIN_LOCATION+1) & block_mem(PARTITION1_LBA_BEGIN_LOCATION+0);
              block_addr <= resize((block_mem(PARTITION1_LBA_BEGIN_LOCATION+3) & block_mem(PARTITION1_LBA_BEGIN_LOCATION+2) & block_mem(PARTITION1_LBA_BEGIN_LOCATION+1) & block_mem(PARTITION1_LBA_BEGIN_LOCATION+0)) * sd_factor, block_addr'length);
            else
              lba_begin <= (others=>'0');
              block_addr <= (others=>'0');
            end if;
            -- load the first sector of the first partition into block memory
            return_state <= CHECK_PARTITION;
            state <= READ_BLOCK1;
          when CHECK_PARTITION =>
            -- check for a valid partition
            if block_mem(11) = x"00" and block_mem(12) = x"02" and block_mem(16) = x"02" and block_mem(SIGNATURE_POSITION) = x"55" and block_mem(SIGNATURE_POSITION+1) = x"AA" then
              sectors_per_cluster <= block_mem(13);
              fat_begin_lba <= resize(lba_begin + (block_mem(15) & block_mem(14)), fat_begin_lba'length);
              cluster_begin_lba <= resize(lba_begin + (block_mem(15) & block_mem(14)) + (block_mem(16)*(block_mem(39) & block_mem(38) & block_mem(37) & block_mem(36))), cluster_begin_lba'length);
              root_dir_first_cluster <= block_mem(47) & block_mem(46) & block_mem(45) & block_mem(44);
              -- compute block address: addr = (cluster_begin_lba + (root_dir_first_cluster-2)*sectors_per_cluster)*FAT_SECTOR_SIZE
              block_addr <= resize((lba_begin + (block_mem(15) & block_mem(14)) + (block_mem(16)*(block_mem(39) & block_mem(38) & block_mem(37) & block_mem(36)))
                                 + ((block_mem(47) & block_mem(46) & block_mem(45) & block_mem(44))-2)
                                 * block_mem(13))*sd_factor, block_addr'length);
              state <= READ_BLOCK1;
              return_state <= READ_DIR1;
              filename <= unsigned(filename_i);
            else
              error_i <= x"04";
              state <= ERROR;
            end if;
          when READ_DIR1 => 
            if sector_cnt = FAT_SECTOR_SIZE then
              block_addr <= block_addr + sd_factor;
              return_state <= READ_DIR1;
              state <= READ_BLOCK1;
              sector_cnt <= 0;
              entry_cnt <= 0;
            else
              state <= COMPARE1;
            end if;
            
          when COMPARE1 =>            
              if block_mem(sector_cnt + entry_cnt) = filename(87-(entry_cnt*8) downto 80-(entry_cnt*8)) then
                if entry_cnt = 10 then
                  -- filename found
                  file_size <= block_mem(sector_cnt+31) & block_mem(sector_cnt+30) & block_mem(sector_cnt+29) & block_mem(sector_cnt+28);
                  next_cluster <= block_mem(sector_cnt+21) & block_mem(sector_cnt+20) & block_mem(sector_cnt+27) & block_mem(sector_cnt+26);
                  ram_addr_o <= unsigned(file_ram_a);
                  state <= READ_FILE1;
                else
                  entry_cnt <= entry_cnt + 1;
                  state <= READ_DIR1;
                end if;
              else
                -- next entry
                sector_cnt <= sector_cnt + 32;
                entry_cnt <= 0;
                if block_mem(sector_cnt) = x"00" then
                  error_i <= x"05";
                  state <= ERROR;
                else
                  state <= READ_DIR1;
                end if;
              end if;
          when READ_FAT1 =>
            block_addr <= resize((fat_begin_lba + (current_cluster/128))*sd_factor, block_addr'length);
            current_pos <= (current_cluster mod 128)*4;
            next_cluster <= (others=>'0');
            return_state <= READ_FAT2;
            state <= READ_BLOCK1;
          when READ_FAT2 =>
            next_cluster <= block_mem(current_pos+3) & block_mem(current_pos+2) & block_mem(current_pos+1) & block_mem(current_pos);
            state <= READ_FILE1;
          when READ_FILE1 =>
            block_addr <= resize((cluster_begin_lba + (next_cluster - 2)*sectors_per_cluster)*sd_factor, block_addr'length);
            current_cluster <= to_integer(next_cluster);        
--            if next_step_r = '1' and next_step = '0' then
              state <= READ_FILE2;
 --           end if;
            cluster_cnt <= 0;
          when READ_FILE2 =>
            addr_is <= std_logic_vector(block_addr);
            rd_is   <= '1';
            if busy_os_r = '0' and busy_os = '1' then
              state <= READ_FILE3;
            end if;
          when READ_FILE3 =>
            rd_is   <= '0';
            if file_size = x"00" then
              hndShk_is <= '0';
              ram_wr <= '0';
              state <= IDLE;
            elsif hndShk_os = '1' and hndShk_is = '0' then
              ram_addr_o <= ram_addr_o + 1;
              ram_d <= data_os;
              ram_wr <= '1';
              cluster_cnt <= cluster_cnt + 1;
              file_size <= file_size - 1;
              hndShk_is <= '1';
            elsif hndShk_os = '0' then
              hndShk_is <= '0';
              ram_wr <= '0';
            else
              ram_wr <= '0';
            end if;
            if busy_os_r = '1' and busy_os = '0' then
              if cluster_cnt < ((FAT_SECTOR_SIZE*to_integer(sectors_per_cluster))-1) then
                block_addr <= block_addr + sd_factor;
                state <= READ_FILE2;
              else
                state <= READ_FAT1;
              end if;
            end if;
          when IDLE => 
            if next_step = '1' then
              state <= READ_BLOCK1;
            end if;
          when ERROR => null;
          
          when others => null;
        end case;
      end if;
    end if;
  
  end process;



--**********************************************************************
-- SD card controller module.
--**********************************************************************
  u3 : entity work.SdCardCtrl
    generic map (
      FREQ_G => 50.35,
      CARD_TYPE_G => card_type,
      --INIT_SPI_FREQ_G => 24.0,
      SPI_FREQ_G  => 24.0
    )
    port map (
      clk_i       => clk50,
      -- Host interface.
      reset_i     => reset,
      rd_i        => rd_is,
      wr_i        => wr_is,
      continue_i  => continue_is,
      addr_i      => addr_is,
      data_i      => data_is,
      data_o      => data_os,
      busy_o      => busy_os,
      hndShk_i    => hndShk_is,
      hndShk_o    => hndShk_os,
      error_o     => error_os,
      -- I/O signals to the external SD card.
      cs_bo       => sd_cs,
      sclk_o      => sclk_o,
      mosi_o      => mosi_o,
      miso_i      => miso_i
    );

END MAIN;