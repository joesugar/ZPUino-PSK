--
--  PSK Transmitter for ZPUINO
-- 
--  Version: 1.0
--
--  Copyright 2014 J. Consugar
-- 
--  The FreeBSD license
--  
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions
--  are met:
--  
--  1. Redistributions of source code must retain the above copyright
--     notice, this list of conditions and the following disclaimer.
--  2. Redistributions in binary form must reproduce the above
--     copyright notice, this list of conditions and the following
--     disclaimer in the documentation and/or other materials
--     provided with the distribution.
--  
--  THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY
--  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
--  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
--  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
--  ZPU PROJECT OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
--  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
--  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
--  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
--  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
--  STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
--  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
--  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--  
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.zpu_config.all;
use work.zpupkg.all;
use work.zpuinopkg.all;

entity zpuino_psk is
  generic (
    pskwidth: integer := 8;
    
    -- Constants associated with the phase shifter.
    NUMBER_OF_SHIFTS : integer := 7;
    IQ_BUS_WIDTH : integer := 8
  );
  port (
    -- Wishbone signals.
    -- ZPUino has a 32 bit word size.
    wb_clk_i: in std_logic;     -- FPGA clock signal
    wb_rst_i: in std_logic;     -- reset signal
    wb_dat_o: out std_logic_vector(wordSize-1 downto 0);      -- data out signal
    wb_dat_i: in std_logic_vector(wordSize-1 downto 0);       -- data in signal
    wb_adr_i: in std_logic_vector(maxIObit downto minIObit);  -- read/write address
    wb_we_i:  in std_logic;     -- write enable
    wb_cyc_i: in std_logic;
    wb_stb_i: in std_logic;
    wb_ack_o: out std_logic;
    wb_inta_o:out std_logic;

    -- Other required signals.
    tx:       out std_logic_vector(pskwidth-1 downto 0)
  );
end entity zpuino_psk;

architecture behave of zpuino_psk is
  --
  -- Define the accumulator associated with the NCO
  --
  component zpuino_dds_acc is
    port (
      clk:    in  std_logic;
      reset:  in  std_logic;
      inc_hi: in  std_logic_vector(7 downto 0);
      inc_lo: in  std_logic_vector(23 downto 0);
      carry:  out std_logic;
      q:      out std_logic_vector(7 downto 0)
  );
  end component zpuino_dds_acc;

  -- 
  -- Define the ROM used to hold the NCO values.
  --
  component zpuino_dds_rom is
    port (
      addr_i    : in  std_logic_vector(7 downto 0); -- 8 bits wide
      data_o    : out signed(7 downto 0)            -- 8 bits wide
  );
  end component zpuino_dds_rom;
  
  --
  -- PSK phase accumulator.
  --
  component zpuino_phase_acc is
    port (
    clk:             in  std_logic;
    reset:           in  std_logic;
    data_out_enable: in  std_logic;
    q:               out std_logic_vector(7 downto 0);
    carry:           out std_logic  
  );
  end component zpuino_phase_acc;
  
  --
  -- PSK phase shifter
  --
  component zpuino_phase_shifter is
  port (
    clk:             in  std_logic;
    reset:           in  std_logic;
    i_data_in:       in  signed(7 downto 0);
    q_data_in:       in  signed(7 downto 0);
    phase_in:        in  std_logic_vector(NUMBER_OF_SHIFTS downto 0);
    i_data_out:      out signed(7 downto 0);
    q_data_out:      out signed(7 downto 0);
    phase_out:       out std_logic_vector(NUMBER_OF_SHIFTS downto 0)
  );
  end component zpuino_phase_shifter;

  -- Signals associated with the NCO accumulator.
  signal acc_reg_o      : std_logic_vector(7 downto 0);             -- register to hold accumulator value
  signal acc_inc_hi_i   : std_logic_vector(7 downto 0);             -- upper accumulator increment.
  signal acc_inc_lo_i   : std_logic_vector(23 downto 0);            -- lower accumulator increment.

  -- Signals associated with the DDS rom.
  signal dds_rom_addr_i : std_logic_vector(7 downto 0);             -- psk rom address
  signal dds_rom_o      : signed(7 downto 0);                       -- rom output

  -- Signals associated with the phase accumulator.
  signal phase_acc_count: std_logic_vector(7 downto 0);             -- phase accumulator output.
  signal phase_acc_carry: std_logic;                                -- phase accumulator carry signal.

  -- Signals associated with the phase shifter.
  signal i_data_in      : signed(IQ_BUS_WIDTH-1 downto 0);
  signal q_data_in      : signed(IQ_BUS_WIDTH-1 downto 0);
  signal phase_in       : std_logic_vector(NUMBER_OF_SHIFTS downto 0);
  signal i_data_out     : signed(IQ_BUS_WIDTH-1 downto 0);
  signal q_data_out     : signed(IQ_BUS_WIDTH-1 downto 0);
  signal phase_out      : std_logic_vector(NUMBER_OF_SHIFTS downto 0);
  
  -- Connecting signals.
  signal psk_dat_o      : std_logic_vector(pskwidth - 1 downto 0);  -- psk output signal

begin
  --
  -- Declare component instances.
  --
  -- Instance of the NCO accumulator
  --
  dds_acc: zpuino_dds_acc
  port map (
    clk     => wb_clk_i,            -- wishbone clock signal
    reset   => wb_rst_i,            -- wishbone reset signal
    inc_hi  => acc_inc_hi_i,        -- 7 downto 0
    inc_lo  => acc_inc_lo_i,        -- 23 downto 0
    carry   => open,
    q       => acc_reg_o            -- 7 downto 0
  );
  
  --
  -- Instance of the NCO rom.
  --
  dds_rom: zpuino_dds_rom
  port map (
    addr_i  => dds_rom_addr_i,      -- 7 downto 0
    data_o  => dds_rom_o            -- 7 downto 0
  );
      
  --
  -- Instance of the phase accumulator.
  --
  phase_acc: zpuino_phase_acc
  port map (
    clk => wb_clk_i,                -- wishbone clock signal
    reset => wb_rst_i,              -- wishbone reset signal
    data_out_enable => '1',
    q => phase_acc_count,           -- phase acc output
    carry => phase_acc_carry        -- phase acc carry
  );
  
  --
  -- Instance of the phase shifter.
  --
  phase_shfiter: zpuino_phase_shifter
  port map (
    clk => wb_clk_i,                -- wishbone clock signal
    reset => wb_rst_i,              -- wishbone reset signal
    i_data_in => i_data_in,         -- in-phase data in
    q_data_in => q_data_in,         -- quadrature data in
    phase_in => phase_in,           -- phase shift in
    i_data_out => i_data_out,       -- in-phase data out
    q_data_out => q_data_out,       -- quadrature data out
    phase_out => phase_out          -- phase shift out
  );
  
  --
  -- Start the actual code here.
  --
  -- Acknowledge all tranfers 
  --
  wb_ack_o <= wb_stb_i and wb_cyc_i; 
  
  -- 
  -- Tie interrupt to '0', we never interrupt 
  --
  wb_inta_o <= '0';
  
  --
  -- Connect accumulator register output to the ROM address lines.
  --
  dds_rom_addr_i <= acc_reg_o;
  
  -- 
  -- Outgoing signals
  -- Data out is taken from the upper bits of the rom data.
  --
  psk_dat_o(pskwidth - 1 downto 0) <= 
    std_logic_vector(dds_rom_o(7 downto 7 - pskwidth + 1));
  tx <= psk_dat_o;
  
  --
  -- DDS processing loop.
  --
  process(wb_clk_i, wb_rst_i)
  begin
    if (wb_rst_i = '1') then
      --
      -- Reset signal is set.
      --
      i_data_in <= (others => '0');
      q_data_in <= (others => '0');
      phase_in  <= (others => '0');
      
    elsif (rising_edge(wb_clk_i)) then
      --
      -- On the rising edge of the clock...
      --
      if (wb_cyc_i='1' and wb_stb_i='1' and wb_we_i='1') then
        -- 
        -- Store the increment value.
        --
        phase_in  <= 
            wb_dat_i(NUMBER_OF_SHIFTS+2*IQ_BUS_WIDTH downto 2*IQ_BUS_WIDTH);
        i_data_in <= 
            signed(wb_dat_i(2*IQ_BUS_WIDTH-1 downto  IQ_BUS_WIDTH));
        q_data_in <= 
            signed(wb_dat_i(IQ_BUS_WIDTH-1  downto  0));
      end if;
    end if;
  end process;
  
  --
  -- Put the output on the address out lines.
  --
  process(wb_adr_i, phase_out, i_data_out, q_data_out)
  begin
    wb_dat_o <= (others => '0');
    wb_dat_o(NUMBER_OF_SHIFTS+2*IQ_BUS_WIDTH downto 2*IQ_BUS_WIDTH) <= 
        phase_out;
    wb_dat_o(2*IQ_BUS_WIDTH-1 downto  IQ_BUS_WIDTH) <= 
        std_logic_vector(i_data_out);
    wb_dat_o(IQ_BUS_WIDTH-1  downto  0) <= 
        std_logic_vector(q_data_out);
  end process;
  
end behave;
