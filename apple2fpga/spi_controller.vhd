-------------------------------------------------------------------------------
--
-- SD/MMC interface (SPI-style) for the Apple ][ Emulator
--
-- Stephen A. Edwards (sedwards@cs.columbia.edu)
--
-------------------------------------------------------------------------------
--
-- One of 1024 disk images may be selected, which reqires a total of
-- 1024 * 227.5 K = 227.5 MB
--
-- Each image is 0x38E00 bytes long
--
-- 0011 1000 1110 0000 0000
--
-- 0x40000 - 0x8000 + 0x1000 - 0x200

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity spi_controller is
  
  port (
    CS_N           : out std_logic;     -- MMC chip select
    MOSI           : out std_logic;     -- Data to card (master out slave in)
    MISO           : in  std_logic;     -- Data from card (master in slave out)
    SCLK           : out std_logic;     -- Card clock
    
    ram_write_addr : out unsigned(13 downto 0);
    ram_di         : out unsigned(7 downto 0);
    ram_we         : out std_logic;
    
    track          : in  unsigned(5 downto 0);  -- 0 - 34
    image          : in  unsigned(9 downto 0);  -- Which disk image to read
   
    CLK_14M        : in  std_logic;    -- System clock
    reset          : in  std_logic);

end spi_controller;

architecture rtl of spi_controller is

  type states is (RESET_STATE,
                  RESET_CLOCKS1,
                  RESET_CLOCKS2,
                  RESET_SEND_CMD0,
                  RESET_SEND_CMD1,
                  RESET_CHECK_CMD1,
                  RESET_SEND_SET_BLOCKLEN,
                  IDLE,
                  READ_TRACK,
                  READ_BLOCK,
                  READ_BLOCK_WAIT,
                  READ_BLOCK_DATA,
                  READ_BLOCK_CRC,
                  SEND_CMD,
                  RECEIVE_BYTE_WAIT,
                  RECEIVE_BYTE,
                  NEW_TRACK);

  signal state, return_state : states;
  signal sclk_sig : std_logic;
  signal counter : unsigned(7 downto 0);
  signal byte_counter : unsigned(7 downto 0);
  
  signal current_track : unsigned(5 downto 0);
  signal current_image : unsigned(9 downto 0);

  signal write_addr : unsigned(13 downto 0);

  signal command : unsigned(47 downto 0);
  signal recv_byte : unsigned(7 downto 0);

  signal address : unsigned(31 downto 0);
    
begin

  ram_write_addr <= write_addr;

  SCLK <= sclk_sig;

  fsm_ff : process(CLK_14M)
  begin
    if rising_edge(CLK_14M) then
      ram_we <= '0';
      if reset = '1' then
        state <= RESET_STATE;
        current_track <= (others => '1');  -- deliberately out of range
        current_image <= (others => '1');
        sclk_sig <= '0';
        CS_N <= '1';
        command <= (others => '1');
        counter <= (others => '0');
        byte_counter <= (others => '0');
        write_addr <= (others => '0');
      else
        case state is
          
          when RESET_STATE =>
            counter <= TO_UNSIGNED(160,8);
            state <= RESET_CLOCKS1;

            -- output a series of clock signals to wake up the chip
          when RESET_CLOCKS1 =>
            if counter = 0 then
              counter <= TO_UNSIGNED(32, 8);
              CS_N <= '0';
              state <= RESET_CLOCKS2;
            else
              counter <= counter - 1;
              sclk_sig <= not sclk_sig;
            end if;
            
          when RESET_CLOCKS2 =>
            if counter = 0 then
              state <= RESET_SEND_CMD0;
            else
              counter <= counter - 1;
              sclk_sig <= not sclk_sig;
            end if;

            -- Send CMD0 : GO_IDLE_STATE
          when RESET_SEND_CMD0 =>
            command <= x"400000000095";
            counter <= TO_UNSIGNED(55, 8);
            return_state <= RESET_SEND_CMD1;
            state <= SEND_CMD;

            -- Send CMD1 : SEND_OP_CMD
          when RESET_SEND_CMD1 =>
            command <= x"410000000001";
            counter <= TO_UNSIGNED(55, 8);
            return_state <= RESET_CHECK_CMD1;
            state <= SEND_CMD;

          when RESET_CHECK_CMD1 =>
            if recv_byte = x"00" then
              state <= RESET_SEND_SET_BLOCKLEN;
            else
              state <= RESET_SEND_CMD1;              
            end if;

          when RESET_SEND_SET_BLOCKLEN =>
            command <= x"500000010001";  -- CMD16: SET_BLOCKLEN (256)
            counter <= TO_UNSIGNED(47, 8);
            return_state <= IDLE;
            state <= SEND_CMD;
              
          when IDLE =>
            if track /= current_track or image /= current_image then
              -- Multiply image by $38E00 and track by $1A00
              address <= ("0000" & image & "000000000000000000") -
                         (           image &  "000000000000000") +
                         (               image & "000000000000") -
                         (                 image  & "000000000") +
                         (                 track  & "000000000") +
                         (               track &  "00000000000") +
                         (              track  & "000000000000");
              write_addr <= (others => '0');
              state <= READ_TRACK;
            end if;
            current_track <= track;
            current_image <= image;

          when READ_TRACK =>
            if write_addr = x"1A00" then
              state <= IDLE;
            else
              state <= READ_BLOCK;
            end if;
            
          -- Set address, write_addr before entering
          when READ_BLOCK =>
            command <= x"51" & address & x"01";  -- READ_SINGLE_BLOCK
            counter <= TO_UNSIGNED(47, 8);
            return_state <= READ_BLOCK_WAIT;
            state <= SEND_CMD;

            -- Wait for a 0 to signal the start of the block,
            -- then read the first byte
          when READ_BLOCK_WAIT =>
            if sclk_sig = '1' and MISO = '0' then
              state <= READ_BLOCK_DATA;
              byte_counter <= x"FF";
              counter <= TO_UNSIGNED(7, 8);
              return_state <= READ_BLOCK_DATA;
              state <= RECEIVE_BYTE;
            end if;
            sclk_sig <= not sclk_sig;            

          when READ_BLOCK_DATA =>
            ram_we <= '1';
            write_addr <= write_addr + 1;
            if byte_counter = x"00" then
              counter <= TO_UNSIGNED(7, 8);
              return_state <= READ_BLOCK_CRC;
              state <= RECEIVE_BYTE;
            else
              byte_counter <= byte_counter - 1;
              return_state <= READ_BLOCK_DATA;
              counter <= TO_UNSIGNED(7, 8);
              state <= RECEIVE_BYTE;
            end if;

          when READ_BLOCK_CRC =>
            counter <= TO_UNSIGNED(7, 8);
            return_state <= READ_TRACK;
            address <= address + x"100";
            state <= RECEIVE_BYTE;

          -- Send the command.  Set counter=47 and return_state first
          when SEND_CMD =>
            if sclk_sig = '1' then
              if counter = 0 then
                state <= RECEIVE_BYTE_WAIT;
              else
                counter <= counter - 1;
                command <= command(46 downto 0) & "1";
              end if;
            end if;
            sclk_sig <= not sclk_sig;

          -- Wait for a "0", indicating the first bit of a response.
          -- Set return_state first
          when RECEIVE_BYTE_WAIT =>
            if sclk_sig = '1' then
              if MISO = '0' then
                recv_byte <= (others => '0');
                counter <= TO_UNSIGNED(6, 8);  -- Already read bit 7
                state <= RECEIVE_BYTE;
              end if;
            end if;
            sclk_sig <= not sclk_sig;

          -- Receive a byte.  Set counter to 7 and return_state before entry
          when RECEIVE_BYTE =>
            if sclk_sig = '1' then
              recv_byte <= recv_byte(6 downto 0) & MISO;
              if counter = 0 then
                state <= return_state;
                ram_di <= recv_byte(6 downto 0) & MISO;
              else
                counter <= counter - 1;
              end if;
            end if;
            sclk_sig <= not sclk_sig;
                        
          when others => null;
        end case;
      end if;
    end if;
  end process fsm_ff;

  MOSI <= command(47);

end rtl;

