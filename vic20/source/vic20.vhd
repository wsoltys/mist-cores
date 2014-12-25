--
-- A simulation model of VIC20 hardware
-- Copyright (c) MikeJ - March 2003
--
-- All rights reserved
--
-- Redistribution and use in source and synthezised forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- Redistributions of source code must retain the above copyright notice,
-- this list of conditions and the following disclaimer.
--
-- Redistributions in synthesized form must reproduce the above copyright
-- notice, this list of conditions and the following disclaimer in the
-- documentation and/or other materials provided with the distribution.
--
-- Neither the name of the author nor the names of other contributors may
-- be used to endorse or promote products derived from this software without
-- specific prior written permission.
--
-- THIS CODE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
-- THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
-- PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.
--
-- You are responsible for any legal issues arising from your use of this code.
--
-- The latest version of this file can be found at: www.fpgaarcade.com
--
-- Email vic20@fpgaarcade.com
--
--
-- Revision list
--
-- version 001 initial release

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_arith.all;
  use ieee.std_logic_unsigned.all;
  --use IEEE.numeric_std.ALL;
  
  use work.pkg_vic20.all;

entity VIC20 is
  port (
    I_PS2_CLK           : in    std_logic;
    I_PS2_DATA          : in    std_logic;

    AUDIO_OUT         : out   std_logic_vector(3 downto 0);
    VIDEO_R_OUT       : out   std_logic_vector(3 downto 0);
    VIDEO_G_OUT       : out   std_logic_vector(3 downto 0);
    VIDEO_B_OUT       : out   std_logic_vector(3 downto 0);

    HSYNC_OUT         : out   std_logic;
    VSYNC_OUT         : out   std_logic;
	
    CLK_8_o           : out   std_logic;
    
    IO_WE             : in    std_logic;
    IO_ADDR           : in    std_logic_vector(15 downto 0);
    IO_DOUT           : in    std_logic_vector(7 downto 0);
    IO_DOWNL          : in    std_logic;
    FORCERESET        : out   std_logic;
    IO_IS_PRG         : in    std_logic;
    SCANDOUBLER       : in    std_logic := '1';
    EXP8KP            : in    std_logic := '1';
    EXP3K             : in    std_logic := '0';
    RESET_B           : in    std_logic;

    RESET_L           : in    std_logic;
    CLK_40            : in    std_logic;
    
    JOYSTICK          : in   std_logic_vector(4 downto 0)

    );
end;

architecture RTL of VIC20 is

    signal reset_dll_h        : std_logic;

    signal clk_4              : std_logic;

    signal clk_8              : std_logic;
    signal ena_4              : std_logic;
   -- signal c_ena              : std_logic;

    signal clk_diviner        : integer range 0 to 10;

    signal delay_count        : std_logic_vector(7 downto 0) := (others => '0');

    signal clk_cnt            : std_logic_vector(2 downto 0);
    -- cpu
    signal c_addr             : std_logic_vector(23 downto 0);
    signal c_din              : std_logic_vector(7 downto 0);
    signal c_dout             : std_logic_vector(7 downto 0);
    signal c_rw_l             : std_logic;
    signal c_irq_l            : std_logic;
    signal c_nmi_l            : std_logic;
    --
    signal io_sel_l           : std_logic_vector(3 downto 0);
    signal blk_sel_l          : std_logic_vector(7 downto 0);
    signal ram_sel_l          : std_logic_vector(7 downto 0);

    -- vic
    signal vic_addr           : std_logic_vector(13 downto 0);
    signal vic_oe_l           : std_logic;
    signal vic_dout           : std_logic_vector( 7 downto 0);
    signal vic_din            : std_logic_vector(11 downto 0);
    signal p2_h               : std_logic;
    signal ena_1mhz           : std_logic;
    signal vic_audio          : std_logic_vector( 3 downto 0);

    signal via1_dout          : std_logic_vector( 7 downto 0);
    signal via2_dout          : std_logic_vector( 7 downto 0);


    -- video system
    signal v_addr             : std_logic_vector(13 downto 0);
    signal v_data             : std_logic_vector( 7 downto 0);
    signal v_data_oe_l        : std_logic;
    signal v_data_read_mux    : std_logic_vector( 7 downto 0);
    signal v_data_read_muxr   : std_logic_vector( 7 downto 0);
    signal v_rw_l             : std_logic;
    signal col_ram_sel_l      : std_logic;

    -- ram
    signal ram0_dout          : std_logic_vector(7 downto 0);
    signal ram1_dout          : std_logic_vector(7 downto 0);
    signal ram2_dout          : std_logic_vector(7 downto 0);
    signal ram3_dout          : std_logic_vector(7 downto 0);
    signal ram4_dout          : std_logic_vector(7 downto 0);
    signal ram5_dout          : std_logic_vector(7 downto 0);
    signal ram6_dout          : std_logic_vector(7 downto 0);
    signal ram7_dout          : std_logic_vector(7 downto 0);

    --
    signal col_ram_dout       : std_logic_vector(7 downto 0);

    signal char_rom_dout      : std_logic_vector(7 downto 0);
    signal basic_rom_dout     : std_logic_vector(7 downto 0);
    signal kernal_rom_dout    : std_logic_vector(7 downto 0);

    signal ext_rom_din        : std_logic_vector(7 downto 0);
    signal expansion_din      : std_logic_vector(7 downto 0);
    signal expansion_nmi_l    : std_logic;
    signal expansion_irq_l    : std_logic;

    -- VIAs
    signal via1_nmi_l         : std_logic;
    signal via1_pa_in         : std_logic_vector(7 downto 0);
    signal via1_pa_out        : std_logic_vector(7 downto 0);

    signal via2_irq_l         : std_logic;

    signal cass_write         : std_logic;
    signal cass_read          : std_logic;
    signal cass_motor         : std_logic;
    signal cass_sw            : std_logic;

    signal keybd_col_out      : std_logic_vector(7 downto 0);
    signal keybd_col_in       : std_logic_vector(7 downto 0);
    signal keybd_col_oe_l     : std_logic_vector(7 downto 0);
    signal keybd_row_in       : std_logic_vector(7 downto 0);
    signal keybd_restore      : std_logic;

    signal joy                : std_logic_vector(3 downto 0);
    signal light_pen          : std_logic;

    signal serial_srq_in      : std_logic;
    signal serial_atn_out_l   : std_logic; -- the vic does not listen to atn_in
    signal serial_clk_out_l   : std_logic;
    signal serial_clk_in      : std_logic;
    signal serial_data_out_l  : std_logic;
    signal serial_data_in     : std_logic;

    -- user port
    signal user_port_cb1_in   : std_logic;
    signal user_port_cb1_out  : std_logic;
    signal user_port_cb1_oe_l : std_logic;
    signal user_port_cb2_in   : std_logic;
    signal user_port_cb2_out  : std_logic;
    signal user_port_cb2_oe_l : std_logic;
    signal user_port_in       : std_logic_vector(7 downto 0);
    signal user_port_out      : std_logic_vector(7 downto 0);
    signal user_port_oe_l     : std_logic_vector(7 downto 0);


    signal video_r            : std_logic_vector(3 downto 0);
    signal video_g            : std_logic_vector(3 downto 0);
    signal video_b            : std_logic_vector(3 downto 0);
    signal hsync              : std_logic;
    signal vsync              : std_logic;
  --  signal csync              : std_logic;
    signal video_r_x2         : std_logic_vector(3 downto 0);
    signal video_g_x2         : std_logic_vector(3 downto 0);
    signal video_b_x2         : std_logic_vector(3 downto 0);
    signal hsync_x2           : std_logic;
    signal vsync_x2           : std_logic;
    

    signal cart_data          : std_logic_vector(7 downto 0);
    signal cart_data_temp     : std_logic_vector(7 downto 0);
    
    signal downlr             : std_logic;
    signal cart_switch        : std_logic;
    signal io_load_addr       : std_logic_vector(15 downto 0) := (others=>'0');
    signal io_res_addr       : std_logic_vector(15 downto 0) := (others=>'0');
    signal io_blk_addr        : std_logic_vector(12 downto 0);
    signal io_blk_dout        : std_logic_vector(7 downto 0);
    signal io_blk1_we          : std_logic := '0';
    signal io_blk2_we          : std_logic := '0';
    signal io_blk5_we          : std_logic := '0';
    signal io_ram_addr        : std_logic_vector(9 downto 0);
    signal io_ram_dout        : std_logic_vector(7 downto 0);
    signal io_ram1_we          : std_logic := '0';
    signal io_ram2_we          : std_logic := '0';
    signal io_ram3_we          : std_logic := '0';
    signal io_ram4_we          : std_logic := '0';
    signal io_ram5_we          : std_logic := '0';
    signal io_ram6_we          : std_logic := '0';
    signal io_ram7_we          : std_logic := '0';
    
    signal vic_cart_dout      : std_logic_vector(7 downto 0);
    
    signal blk1_dout: std_logic_vector(7 downto 0);
    signal blk2_dout: std_logic_vector(7 downto 0);
    
    attribute keep: boolean;
    attribute keep of io_load_addr: signal is true;
    attribute keep of IO_ADDR: signal is true;


begin
  --
  -- IO connect these to the outside world if you wish ...
  --


  p_expansion : process(blk_sel_l, cart_data)
  begin
    expansion_din <= x"FF";
    if (blk_sel_l(5) = '0') then
      expansion_din <= cart_data;
    end if;
  end process;
  
  expansion_nmi_l <= '1';
  expansion_irq_l <= '1';


  -- user port
  user_port_cb1_in <= '0';
  user_port_cb2_in <= '0';
  user_port_in <= x"00";

  -- tape
  cass_read <= '0';
  --<= cass_write;
  --<= cass_motor
  cass_sw <= '1'; -- motor off

  -- serial
  serial_srq_in <= '0';
  serial_clk_in <= '0';
  serial_data_in <= '0';
  -- <= serial_atn_out_l;
  -- <= serial_clk_out_l;
  -- <= serial_data_out_l

  -- Joystick
  --  "1111"; -- 0 up, 1 down, 2 left,  3 right
  -- lightpen for fire button
  joy <= JOYSTICK(3 downto 0);
  light_pen <= JOYSTICK(4);
  --
  --
  --
 

 clk_8 <= CLK_40;
 

 CLK_8_o <= clk_8;

 clkdiv2: process(clk_8)
 begin
   if clk_8'event AND clk_8 = '1' then
     ena_4 <= not ena_4;
     clk_4 <= not clk_4;
   end if;
 end process;

  cpu : entity work.T65
      port map (
		Mode    => "00",
		Res_n   => RESET_L,
		Clk     => clk_4,
		Enable  => ena_1mhz,
    Rdy     => '1',
		Abort_n => '1',
		IRQ_n   => c_irq_l,
		NMI_n   => c_nmi_l,
		SO_n    => '1',
		R_W_n   => c_rw_l,
		Sync    => open,
		EF      => open,
		MF      => open,
		XF      => open,
		ML_n    => open,
		VP_n    => open,
		VDA     => open,
		VPA     => open,
		A       => c_addr,
		DI      => c_din,
		DO      => c_dout

      );

  vic : entity work.VIC20_VIC
    generic map (
      K_OFFSET        => "10000"
      )
    port map (
      I_RW_L          => v_rw_l,

      I_ADDR          => v_addr(13 downto 0),
      O_ADDR          => vic_addr(13 downto 0),

      I_DATA          => vic_din,
      O_DATA          => vic_dout,
      O_DATA_OE_L     => vic_oe_l,
      --
      O_AUDIO         => vic_audio,

      O_VIDEO_R       => video_r,
      O_VIDEO_G       => video_g,
      O_VIDEO_B       => video_b,

      O_HSYNC         => hsync,
      O_VSYNC         => vsync,
      O_COMP_SYNC_L   => open, --csync,
      --
      --
      I_LIGHT_PEN     => light_pen,
      I_POTX          => '0',
      I_POTY          => '0',

      O_ENA_1MHZ      => ena_1mhz,
      O_P2_H          => p2_h,
      ENA_4           => ena_4,
      CLK             => clk_8
      );

  AUDIO_OUT(3 downto 0) <= vic_audio;

  via1 : entity work.M6522
    port map (
      I_RS            => c_addr(3 downto 0),
      I_DATA          => v_data(7 downto 0),
      O_DATA          => via1_dout,
      O_DATA_OE_L     => open,

      I_RW_L          => c_rw_l,
      I_CS1           => c_addr(4),
      I_CS2_L         => io_sel_l(0),

      O_IRQ_L         => via1_nmi_l, -- note, not open drain

      I_CA1           => keybd_restore,
      I_CA2           => cass_motor,
      O_CA2           => cass_motor,
      O_CA2_OE_L      => open,

      I_PA            => via1_pa_in,
      O_PA            => via1_pa_out,
      O_PA_OE_L       => open,

      -- port b
      I_CB1           => user_port_cb1_in,
      O_CB1           => user_port_cb1_out,
      O_CB1_OE_L      => user_port_cb1_oe_l,

      I_CB2           => user_port_cb2_in,
      O_CB2           => user_port_cb2_out,
      O_CB2_OE_L      => user_port_cb2_oe_l,

      I_PB            => user_port_in,
      O_PB            => user_port_out,
      O_PB_OE_L       => user_port_oe_l,

      I_P2_H          => p2_h,
      RESET_L         => RESET_L,-- reset_l_sampled,
      ENA_4           => ena_4,
      CLK             => clk_8
      );


  serial_atn_out_l <= via1_pa_out(7);
  via1_pa_in(7) <= via1_pa_out(7);
  via1_pa_in(6) <= cass_sw;
  via1_pa_in(5) <= light_pen;
  via1_pa_in(4) <= joy(2);
  via1_pa_in(3) <= joy(1);
  via1_pa_in(2) <= joy(0);
  via1_pa_in(1) <= serial_data_in;
  via1_pa_in(0) <= serial_clk_in;

  via2 : entity work.M6522
    port map (
      I_RS            => c_addr(3 downto 0),
      I_DATA          => v_data(7 downto 0),
      O_DATA          => via2_dout,
      O_DATA_OE_L     => open,

      I_RW_L          => c_rw_l,
      I_CS1           => c_addr(5),
      I_CS2_L         => io_sel_l(0),

      O_IRQ_L         => via2_irq_l, -- note, not open drain

      I_CA1           => cass_read,
      I_CA2           => serial_clk_out_l,
      O_CA2           => serial_clk_out_l,
      O_CA2_OE_L      => open,

      I_PA            => keybd_row_in,
      O_PA            => open,
      O_PA_OE_L       => open,

      -- port b
      I_CB1           => serial_srq_in,
      O_CB1           => open,
      O_CB1_OE_L      => open,

      I_CB2           => serial_data_out_l,
      O_CB2           => serial_data_out_l,
      O_CB2_OE_L      => open,

      I_PB            => keybd_col_in,
      O_PB            => keybd_col_out,
      O_PB_OE_L       => keybd_col_oe_l,

      I_P2_H          => p2_h,
      RESET_L         => RESET_L, --reset_l_sampled,
      ENA_4           => ena_4,
      CLK             => clk_8
      );

  p_keybd_col_in : process(keybd_col_out, keybd_col_oe_l, joy)
  begin
    for i in 0 to 6 loop
      keybd_col_in(i) <= keybd_col_out(i);
    end loop;

    if (keybd_col_oe_l(7) = '0') then
      keybd_col_in(7) <= keybd_col_out(7);
    else
      keybd_col_in(7) <= joy(3);
    end if;
  end process;
  cass_write <= keybd_col_out(3);

  keybd : entity work.VIC20_PS2_IF
    port map (

      PS2_CLK       => I_PS2_CLK,
      PS2_DATA      => I_PS2_DATA,

      COL_IN           => keybd_col_out,
      ROW_OUT           => keybd_row_in,
      RESTORE       => keybd_restore,

      ENA_1MHZ      => ena_1mhz,
      P2_H          => p2_h,
      RESET_L         => RESET_L, --reset_l_sampled,
      CLK_4           => ena_4
      --CLK             => clk_8
      --SCAN            => scan_out
      );

  p_irq_resolve : process(expansion_irq_l, expansion_nmi_l,
                          via2_irq_l, via1_nmi_l)
  begin
    c_irq_l <= '1';
    if (expansion_irq_l = '0') or (via2_irq_l = '0') then
      c_irq_l <= '0';
    end if;

    c_nmi_l <= '1';
    if (expansion_nmi_l = '0') or (via1_nmi_l = '0') then
      c_nmi_l <= '0';
    end if;
  end process;

  --
  -- decode
  --
  p_io_addr_decode : process(c_addr)
  begin

    io_sel_l <= "1111";
    if (c_addr(15 downto 13) = "100") then
      case c_addr(12 downto 10) is
        when "000" => io_sel_l <= "1111";
        when "001" => io_sel_l <= "1111";
        when "010" => io_sel_l <= "1111";
        when "011" => io_sel_l <= "1111";
        when "100" => io_sel_l <= "1110";
        when "101" => io_sel_l <= "1101"; -- col
        when "110" => io_sel_l <= "1011";
        when "111" => io_sel_l <= "0111";
        when others => null;
      end case;
    end if;
  end process;

  p_blk_addr_decode : process(c_addr)
  begin
    blk_sel_l <= "11111111";
    case c_addr(15 downto 13) is
      when "000" => blk_sel_l <= "11111110";
      when "001" => blk_sel_l <= "11111101";
      when "010" => blk_sel_l <= "11111011";
      when "011" => blk_sel_l <= "11110111";
      when "100" => blk_sel_l <= "11101111";
      when "101" => blk_sel_l <= "11011111"; -- Cart
      when "110" => blk_sel_l <= "10111111"; -- basic
      when "111" => blk_sel_l <= "01111111"; -- kernal
      when others => null;
    end case;
  end process;

  p_v_mux : process(c_addr, c_dout, c_rw_l, p2_h, vic_addr, v_data_read_mux,
                         blk_sel_l, io_sel_l)
  begin
    -- simplified data source mux
    if (p2_h = '0') then
      v_addr(13 downto 0) <= vic_addr(13 downto 0);
      v_data <= v_data_read_mux(7 downto 0);
      v_rw_l <= '1';
      col_ram_sel_l <= '1'; -- colour ram has dedicated mux for vic, so disable
    else
      v_addr(13 downto 0) <= blk_sel_l(4) & c_addr(12 downto 0);
      v_data <= c_dout;
      v_rw_l <= c_rw_l;
      col_ram_sel_l <= io_sel_l(1);
    end if;

  end process;

  p_ram_addr_decode : process(v_addr, blk_sel_l, p2_h)
  begin
    ram_sel_l <= "11111111";
    if ((p2_h = '1') and (blk_sel_l(0) = '0')) or
       ((p2_h = '0') and (v_addr(13) = '1')) then
      case v_addr(12 downto 10) is
        when "000" => ram_sel_l <= "11111110";
        when "001" => ram_sel_l <= "11111101";
        when "010" => ram_sel_l <= "11111011";
        when "011" => ram_sel_l <= "11110111";
        when "100" => ram_sel_l <= "11101111";
        when "101" => ram_sel_l <= "11011111";
        when "110" => ram_sel_l <= "10111111";
        when "111" => ram_sel_l <= "01111111";
        when others => null;
      end case;
    end if;
  end process;



  p_vic_din_mux : process(p2_h, col_ram_dout, v_data)
  begin
    if (p2_h = '0') then
      vic_din(11 downto 8) <= col_ram_dout(3 downto 0);
    else
      vic_din(11 downto 8) <= v_data(3 downto 0);
    end if;

    vic_din(7 downto 0) <= v_data(7 downto 0);
  end process;


  p_v_read_mux : process(col_ram_sel_l, ram_sel_l, blk_sel_l, vic_oe_l, v_addr,
                         col_ram_dout, ram0_dout, ram1_dout, ram2_dout, ram3_dout, ram4_dout, ram5_dout, ram6_dout, ram7_dout,
                         vic_dout, char_rom_dout, EXP3K,
                         v_data_read_muxr)
  begin
    -- simplified data read mux
    -- nasty if statement but being lazy
    -- these are exclusive, but the tools may not spot this.

    v_data_oe_l <= '1';
    if (col_ram_sel_l = '0') then
      v_data_read_mux <= "0000" & col_ram_dout(3 downto 0);
      v_data_oe_l     <= '0';
    elsif (vic_oe_l = '0') then
      v_data_read_mux <= vic_dout;
      v_data_oe_l     <= '0';
    elsif (ram_sel_l(0) = '0') then
      v_data_read_mux <= ram0_dout;
      v_data_oe_l     <= '0';
    elsif (ram_sel_l(1) = '0' and EXP3K = '1') then
      v_data_read_mux <= ram1_dout;
      v_data_oe_l     <= '0';
    elsif (ram_sel_l(2) = '0' and EXP3K = '1') then
      v_data_read_mux <= ram2_dout;
      v_data_oe_l     <= '0';
    elsif (ram_sel_l(3) = '0' and EXP3K = '1') then
      v_data_read_mux <= ram3_dout;
      v_data_oe_l     <= '0';
    elsif (ram_sel_l(4) = '0') then
      v_data_read_mux <= ram4_dout;
      v_data_oe_l     <= '0';
    elsif (ram_sel_l(5) = '0') then
      v_data_read_mux <= ram5_dout;
      v_data_oe_l     <= '0';
    elsif (ram_sel_l(6) = '0') then
      v_data_read_mux <= ram6_dout;
      v_data_oe_l     <= '0';
    elsif (ram_sel_l(7) = '0') then
      v_data_read_mux <= ram7_dout;
      v_data_oe_l     <= '0';
    elsif (v_addr(13 downto 12) = "00") then
      v_data_read_mux <= char_rom_dout;
      v_data_oe_l     <= '0';
    else
      -- emulate floating bus
      v_data_read_mux <= v_data_read_muxr;
    end if;

  end process;

  p_v_bus_hold : process
  begin
    wait until rising_edge(clk_4);
    v_data_read_muxr <= v_data_read_mux;
  end process;

  p_cpu_read_mux : process(p2_h, c_addr, io_sel_l, ram_sel_l, blk_sel_l,
                           v_data_read_mux, via1_dout, via2_dout, v_data_oe_l,
                           basic_rom_dout, kernal_rom_dout, expansion_din,
                           blk1_dout, blk2_dout, EXP8KP)
  begin

    if (p2_h = '0') then -- vic is on the bus
      --c_din <= "XXXXXXXX";
      c_din <= "00000000";
    elsif (io_sel_l(0) = '0') and (c_addr(4) = '1') then -- blk4
      c_din <= via1_dout;
    elsif (io_sel_l(0) = '0') and (c_addr(5) = '1') then -- blk4
      c_din <= via2_dout;
    elsif (blk_sel_l(1) = '0' and EXP8KP = '1') then
      c_din <= blk1_dout;
    elsif (blk_sel_l(2) = '0' and EXP8KP = '1') then
      c_din <= blk2_dout;
    elsif (blk_sel_l(5) = '0') then
      c_din <= expansion_din;
    elsif (blk_sel_l(6) = '0') then
      c_din <= basic_rom_dout;
    elsif (blk_sel_l(7) = '0') then
      c_din <= kernal_rom_dout;
    elsif (v_data_oe_l = '0') then
      c_din <= v_data_read_mux;
    else
      c_din <= "11111111";
    end if;
  end process;

  --
  -- IO upload
  --
  
  process(clk_8)
  begin
    if falling_edge(clk_8) then
      
      downlr <= IO_DOWNL;
      
      io_ram1_we <= '0';
      io_ram2_we <= '0';
      io_ram3_we <= '0';
      io_ram4_we <= '0';
      io_ram5_we <= '0';
      io_ram6_we <= '0';
      io_ram7_we <= '0';
      io_blk1_we <= '0';
      io_blk2_we <= '0';
      io_blk5_we <= '0';
      
      if(IO_DOWNL = '0') then
        forceReset <= '0';
        if(RESET_B = '1') then
          cart_switch <= '0';
        end if;
      else
        
        if IO_IS_PRG = '1' then
          if (io_addr = "0000000000000000") then
            io_load_addr(7 downto 0) <= io_dout;
          elsif (io_addr = "0000000000000001") then
            io_load_addr(15 downto 8) <= io_dout;
          else
            io_res_addr <= io_load_addr + io_addr - 2;
            
            if io_res_addr < "0010000000000000" then
              -- main memory
              io_ram_addr  <= io_res_addr(9 downto 0);
              io_ram_dout <= io_dout;
              if io_res_addr < "0000100000000000" then
                io_ram1_we <= io_we;
              elsif io_res_addr < "0000110000000000" then
                io_ram2_we <= io_we;
              elsif io_res_addr < "0001000000000000" then
                io_ram3_we <= io_we;
              elsif io_res_addr < "0001010000000000" then
                io_ram4_we <= io_we;
              elsif io_res_addr < "0001100000000000" then
                io_ram5_we <= io_we;
              elsif io_res_addr < "0001110000000000" then
                io_ram6_we <= io_we;
              elsif io_res_addr < "0010000000000000" then
                io_ram7_we <= io_we;
              end if;
            elsif io_res_addr < "0100000000000000" then
              -- blk1 $2000 to $3FFF
              io_blk_addr <= io_res_addr(12 downto 0);
              io_blk_dout <= io_dout;
              io_blk1_we  <= io_we;
            elsif io_res_addr < "0110000000000000" then
              -- blk2 $4000 to $5FFF
              io_blk_addr <= io_res_addr(12 downto 0);
              io_blk_dout <= io_dout;
              io_blk2_we  <= io_we;
            elsif io_res_addr >= "1010000000000000" then
              -- Cartridge ROM blk5 $A000 to $BFFF
              io_blk5_we <= io_we;
              io_blk_addr <= io_res_addr(12 downto 0);
              io_blk_dout <= io_dout;
            end if;
            
          end if;
        else
          -- ROM Cartridges without start bytes at $A000 (blk5)
          io_blk5_we <= io_we;
          io_blk_addr <= io_addr(12 downto 0);
          io_blk_dout <= io_dout;
        end if;
      end if;
      
      if(IO_DOWNL = '0' and downlr = '1' and (IO_IS_PRG = '0' or io_res_addr(15 downto 13) = "101")) then
        cart_switch <= '1';
        forceReset <= '1';
      end if;
    end if;
  end process;
  
  
  --
  -- main memory
  --

  -- BASIC working memory
  rams0 : entity work.VIC20_RAMS
    port map (
      V_ADDR => v_addr(9 downto 0),
      DIN    => v_data,
      DOUT   => ram0_dout,
      V_RW_L => v_rw_l,
      CS_L  => ram_sel_l(0),
      CLK    => ena_4
      );

  -- Screen color memory
  col_ram : entity work.VIC20_RAMS
    port map (
      V_ADDR => v_addr(8 downto 0),
      DIN    => v_data,
      DOUT   => col_ram_dout,
      V_RW_L => v_rw_l,
      CS_L   => col_ram_sel_l,
      CLK    => ena_4
      );
      
  -- ram select 1 1kb
  ram1_inst : entity work.dpram
    generic map
    (
      widthad_a	=> 10
    )
    port map
    (
      clock_a	=> ena_4,
      address_a	=> v_addr(9 downto 0),
      wren_a	=> not (ram_sel_l(1) or v_rw_l) and EXP3K,
      data_a	=> v_data,
      q_a	=> ram1_dout,
      
      clock_b => clk_8,
      address_b => io_ram_addr,
      wren_b => io_ram1_we,
      data_b => io_ram_dout
    );
    
  -- ram select 2 1kb
  ram2_inst : entity work.dpram
    generic map
    (
      widthad_a	=> 10
    )
    port map
    (
      clock_a	=> ena_4,
      address_a	=> v_addr(9 downto 0),
      wren_a	=> not (ram_sel_l(2) or v_rw_l) and EXP3K,
      data_a	=> v_data,
      q_a	=> ram2_dout,
      
      clock_b => clk_8,
      address_b => io_ram_addr,
      wren_b => io_ram2_we,
      data_b => io_ram_dout
    );
    
  -- ram select 3 1kb
  ram3_inst : entity work.dpram
    generic map
    (
      widthad_a	=> 10
    )
    port map
    (
      clock_a	=> ena_4,
      address_a	=> v_addr(9 downto 0),
      wren_a	=> not (ram_sel_l(3) or v_rw_l) and EXP3K,
      data_a	=> v_data,
      q_a	=> ram3_dout,
      
      clock_b => clk_8,
      address_b => io_ram_addr,
      wren_b => io_ram3_we,
      data_b => io_ram_dout
    );
      
  -- ram select 4 1kb $1000 to $13FF ($1000-$11FF screen memory)
  ram4_inst : entity work.dpram
    generic map
    (
      widthad_a	=> 10
    )
    port map
    (
      clock_a	=> ena_4,
      address_a	=> v_addr(9 downto 0),
      wren_a	=> not (ram_sel_l(4) or v_rw_l),
      data_a	=> v_data,
      q_a	=> ram4_dout,
      
      clock_b => clk_8,
      address_b => io_ram_addr,
      wren_b => io_ram4_we,
      data_b => io_ram_dout
    );
    
  -- ram select 5 1kb ram from $1400 to $17FF
  ram5_inst : entity work.dpram
    generic map
    (
      widthad_a	=> 10
    )
    port map
    (
      clock_a	=> ena_4,
      address_a	=> v_addr(9 downto 0),
      wren_a	=> not (ram_sel_l(5) or v_rw_l),
      data_a	=> v_data,
      q_a	=> ram5_dout,
      
      clock_b => clk_8,
      address_b => io_ram_addr,
      wren_b => io_ram5_we,
      data_b => io_ram_dout
    );
    
  -- main memory ram 6, $1800 to $1BFF
  ram6_inst : entity work.dpram
    generic map
    (
      widthad_a	=> 10
    )
    port map
    (
      clock_a	=> ena_4,
      address_a	=> v_addr(9 downto 0),
      wren_a	=> not (ram_sel_l(6) or v_rw_l),
      data_a	=> v_data,
      q_a	=> ram6_dout,
      
      clock_b => clk_8,
      address_b => io_ram_addr,
      wren_b => io_ram6_we,
      data_b => io_ram_dout
    );
    
  -- main memory ram 7, $1C00 to $1FFF
  ram7_inst : entity work.dpram
    generic map
    (
      widthad_a	=> 10
    )
    port map
    (
      clock_a	=> ena_4,
      address_a	=> v_addr(9 downto 0),
      wren_a	=> not (ram_sel_l(7) or v_rw_l),
      data_a	=> v_data,
      q_a	=> ram7_dout,
      
      clock_b => clk_8,
      address_b => io_ram_addr,
      wren_b => io_ram7_we,
      data_b => io_ram_dout
    );
    
   -- blk1 cartridge 8kb rom at $2000
  blk1_inst : entity work.dpram
    generic map
    (
      widthad_a	=> 13
    )
    port map
    (
      clock_a	=> ena_4,
      address_a	=> c_addr(12 downto 0),
      wren_a	=> not (blk_sel_l(1) or v_rw_l) and EXP8KP,
      data_a	=> v_data,
      q_a	=> blk1_dout,
      
      clock_b => clk_8,
      address_b => io_blk_addr,
      wren_b => io_blk1_we,
      data_b => io_blk_dout
    );

  -- blk2 cartridge 8kb rom at $4000
  blk2_inst : entity work.dpram
    generic map
    (
      widthad_a	=> 13
    )
    port map
    (
      clock_a	=> ena_4,
      address_a	=> c_addr(12 downto 0),
      wren_a	=> not (blk_sel_l(2) or v_rw_l) and EXP8KP,
      data_a	=> v_data,
      q_a	=> blk2_dout,
      
      clock_b => clk_8,
      address_b => io_blk_addr,
      wren_b => io_blk2_we,
      data_b => io_blk_dout
    );
    
  --
  -- cart slot 0xA000-0xBFFF (8K)
  --
    
  -- blk5 cartridge 8kb rom at $A000
  blk5_inst : entity work.dpram
    generic map
    (
      widthad_a	=> 13
    )
    port map
    (
      clock_a	=> ena_4,
      address_a	=> c_addr(12 downto 0),
      wren_a	=> '0',
      data_a	=> "ZZZZZZZZ",
      q_a	=> vic_cart_dout,
      
      clock_b => clk_8,
      address_b => io_blk_addr,
      wren_b => io_blk5_we,
      data_b => io_blk_dout
    );
      


  --
  -- roms
  --


  char_rom : VIC20_CHAR_ROM
    port map (
      CLK         => clk_4,
      ADDR        => v_addr(11 downto 0),
      DATA        => char_rom_dout
      );

  --  use the following if your have enough space for internal roms
  basic_rom : VIC20_BASIC_ROM
    port map (
      CLK         => clk_4,
      ADDR        => c_addr(12 downto 0),
      DATA        => basic_rom_dout
      );

  kernal_rom : VIC20_KERNAL_ROM
    port map (
      CLK         => clk_4,
      ADDR        => c_addr(12 downto 0),
      DATA        => kernal_rom_dout
      );


  --
  -- scan doubler
  --
  u_dblscan : entity work.VIC20_DBLSCAN
    port map (
      I_R               => video_r,
      I_G               => video_g,
      I_B               => video_b,
      I_HSYNC           => hsync,
      I_VSYNC           => vsync,
      --
      O_R               => video_r_x2,
      O_G               => video_g_x2,
      O_B               => video_b_x2,
      O_HSYNC           => hsync_x2,
      O_VSYNC           => vsync_x2,
      --
      ENA_X2            => '1',
      ENA               => ena_4,
      CLK               => clk_8
    );
  
  
  p_video_ouput : process
  begin
    wait until rising_edge(clk_8);

    if cart_switch = '1' then
      cart_data <= vic_cart_dout;
    end if;

    if SCANDOUBLER = '1' then
      VIDEO_R_OUT <= video_r_x2;
      VIDEO_G_OUT <= video_g_x2;
      VIDEO_B_OUT <= video_b_x2;
      HSYNC_OUT   <= hSync_X2;
      VSYNC_OUT   <= vSync_X2;
    else
      VIDEO_R_OUT <= video_r;
      VIDEO_G_OUT <= video_g;
      VIDEO_B_OUT <= video_b;
      HSYNC_OUT   <= hSync;
      VSYNC_OUT   <= vSync;
    end if;
  end process;

end RTL;
