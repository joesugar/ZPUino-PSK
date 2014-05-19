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
use std.textio.all;

library work;
use work.zpu_config.all;
use work.zpupkg.all;
use work.zpuinopkg.all;

entity zpuino_psk is
  generic (
    pskwidth: integer := 8;
    
    -- Constants associated with the phase shifter.
    NUMBER_OF_SHIFTS : integer := 7;
    IQ_BUS_WIDTH : integer := 8;
    PSK_ROM_ADDRESS_WIDTH : integer := 8;
    PSK_DATA_WIDTH : integer := 8;          -- NUMBER_OF_SHIFTS+1
    DDS_ROM_DATA_WIDTH : integer := 8;
    DDS_ROM_ADDRESS_WIDTH : integer := 8;
    DDS_INC_HI_WIDTH : integer := 8
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
    tx: out std_logic_vector(1 downto 0);
    debug : out std_logic_vector(7 downto 0)
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
    inc_hi: in  std_logic_vector(DDS_INC_HI_WIDTH-1 downto 0);
    inc_lo: in  std_logic_vector(31-DDS_INC_HI_WIDTH downto 0);
    carry:  out std_logic;
    q:      out std_logic_vector(DDS_ROM_ADDRESS_WIDTH-1 downto 0)
  );
  end component zpuino_dds_acc;

  -- 
  -- Define the ROM used to hold the NCO values.
  --
  component zpuino_dds_rom is
  port (
    addr_i    : in  std_logic_vector(DDS_ROM_ADDRESS_WIDTH-1 downto 0); -- 8 bits wide
    data_o    : out signed(DDS_ROM_DATA_WIDTH-1 downto 0) -- 8 bits wide
  );
  end component zpuino_dds_rom;
  
  --
  -- PSK phase accumulator.
  --
  component zpuino_phase_acc is
  port (
    clk:             in  std_logic;
    reset:           in  std_logic;
    serial_data_in:  in  std_logic;
    q:               out unsigned(DDS_ROM_ADDRESS_WIDTH-1 downto 0);
    inversion:       out std_logic
  );
  end component zpuino_phase_acc;
  
  --
  -- PSK phase shifter
  --
  component zpuino_phase_shifter is
  port (
    clk:             in  std_logic;
    reset:           in  std_logic;
    i_data_in:       in  signed(DDS_ROM_DATA_WIDTH-1 downto 0);
    q_data_in:       in  signed(DDS_ROM_DATA_WIDTH-1 downto 0);
    phase_in:        in  std_logic_vector(NUMBER_OF_SHIFTS downto 0);
    i_data_out:      out signed(DDS_ROM_DATA_WIDTH-1 downto 0);
    q_data_out:      out signed(DDS_ROM_DATA_WIDTH-1 downto 0);
    phase_out:       out std_logic_vector(NUMBER_OF_SHIFTS downto 0)
  );
  end component zpuino_phase_shifter;

  --
  -- Simple D2A converter
  --
  component simple_sigmadelta is
  generic (
    BITS: integer := 8
  );
  port (
    clk:      in std_logic;
    rst:      in std_logic;
    data_in:  in std_logic_vector(BITS-1 downto 0);
    data_out: out std_logic
    );
  end component simple_sigmadelta;
  
  -- Signals associated with the DDS NCO accumulator.
  signal dds_acc_reg_o    : std_logic_vector(DDS_ROM_ADDRESS_WIDTH-1 downto 0); -- register to hold accumulator value
  signal dds_acc_inc_hi_i : std_logic_vector(DDS_INC_HI_WIDTH-1 downto 0);      -- upper accumulator increment.
  signal dds_acc_inc_lo_i : std_logic_vector(31-DDS_INC_HI_WIDTH downto 0);     -- lower accumulator increment.

  -- Signals associated with the DDS rom.
  signal dds_rom_addr_i   : std_logic_vector(DDS_ROM_ADDRESS_WIDTH-1 downto 0); -- psk rom address
  signal dds_rom_o        : signed(DDS_ROM_DATA_WIDTH-1 downto 0);    -- rom output

  -- Signals associated with the phase accumulator.
  signal psk_phase_acc_count  : unsigned(7 downto 0);         -- phase accumulator output.
  signal psk_phase_inversion  : std_logic;
  signal psk_serial_data_in   : std_logic;
  
  -- Signals associated with the phase shifter.
  signal i_data_in        : signed(IQ_BUS_WIDTH-1 downto 0);
  signal q_data_in        : signed(IQ_BUS_WIDTH-1 downto 0);
  signal phase_in         : std_logic_vector(NUMBER_OF_SHIFTS downto 0);
  signal i_data_out       : signed(IQ_BUS_WIDTH-1 downto 0);
  signal q_data_out       : signed(IQ_BUS_WIDTH-1 downto 0);
  signal phase_out        : std_logic_vector(NUMBER_OF_SHIFTS downto 0);
  
  -- Signals associated with the audio out
  signal i_audio_out      : std_logic_vector(IQ_BUS_WIDTH-1 downto 0);
  signal q_audio_out      : std_logic_vector(IQ_BUS_WIDTH-1 downto 0);
  
  -- Signals associated with the phase shift ROM.
  signal phase_shift_index: unsigned(PSK_ROM_ADDRESS_WIDTH-1 downto 0);
  
  -- Connecting signals.
  signal psk_dat_o        : std_logic_vector(pskwidth - 1 downto 0);  -- psk output signal

  --
  -- Declarations used to define the array of ROM data.
  --
  type rom_array is array(2**PSK_ROM_ADDRESS_WIDTH-1 downto 0) 
      of std_logic_vector(NUMBER_OF_SHIFTS downto 0);
      
  impure function rom_init(filename : string) return rom_array is
    file rom_file : text open read_mode is filename;
    variable rom_line : line;
    variable rom_value : bit_vector(NUMBER_OF_SHIFTS downto 0);
    variable temp : rom_array;
  begin
    for rom_index in 0 to 2**PSK_ROM_ADDRESS_WIDTH-1 loop
      readline(rom_file, rom_line);
      read(rom_line, rom_value);
      temp(rom_index) := to_stdlogicvector(rom_value);
    end loop;
    return temp;
  end function;
  
  constant phase_shift_rom_array : rom_array := rom_init(filename =>
      "/home/joseph/Papilio/ZPUino-HDL/zpu/hdl/zpuino/phase_shift_rom.txt");    
  
begin
  --
  -- Declare component instances.
  --
  
  --
  -- REGION  DDS NCO
  --
  -- Instance of the NCO accumulator
  --
  dds_acc: zpuino_dds_acc
  port map (
    clk     => wb_clk_i,            -- wishbone clock signal
    reset   => wb_rst_i,            -- wishbone reset signal
    inc_hi  => dds_acc_inc_hi_i,        -- 7 downto 0
    inc_lo  => dds_acc_inc_lo_i,        -- 23 downto 0
    carry   => open,
    q       => dds_acc_reg_o            -- 7 downto 0
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
  -- END DDS NCO
  --
  
  --
  -- REGION PSK PHASE ACCUMULATOR
  --
  -- Instance of the phase accumulator.
  --
  psk_phase_acc: zpuino_phase_acc
  port map (
    clk => wb_clk_i,                -- wishbone clock signal
    reset => wb_rst_i,              -- wishbone reset signal
    serial_data_in => psk_serial_data_in,
    q => psk_phase_acc_count,       -- phase acc output
    inversion => psk_phase_inversion
  );
  --
  -- END PSK PHASE ACCUMULATOR
  --
  
  --
  -- REGION CORDIC PHASE SHIFTER 
  --
  -- Instance of the phase shifter.
  --
  phase_shifter: zpuino_phase_shifter
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
  -- END CORDIC PHASE SHIFTER
  -- 
  
  --
  -- REGION D2A CONVERTERS
  --
  -- Instance of a simple 2 channel D2A converter.
  --
  psk_d2a_i: simple_sigmadelta
  generic map (
    BITS => 8
  ) 
  port map (
    clk       => wb_clk_i,
    rst       => wb_rst_i,
    data_in   => std_logic_vector(i_audio_out),
    data_out  => tx(1)
  );

  psk_d2a_q: simple_sigmadelta
  generic map (
    BITS => 8
  ) 
  port map (
    clk       => wb_clk_i,
    rst       => wb_rst_i,
    data_in   => std_logic_vector(q_audio_out),
    data_out  => tx(0)
  );
  --
  -- END D2A CONVERTERS
  --
      
  --
  -- REGION ARCHITECTURE CODE
  --
  -- Acknowledge all tranfers per the wishbone spec.
  --
  wb_ack_o <= wb_stb_i and wb_cyc_i; 
  
  -- 
  -- Tie interrupt to '0', we never interrupt 
  --
  wb_inta_o <= '0';
  
  --
  -- Connect accumulator register output to the ROM address lines.
  --
  dds_rom_addr_i <= dds_acc_reg_o;  
  q_data_in <= X"00";
  i_data_in <= dds_rom_o;
  
  -- debug line
  debug(0) <= psk_phase_inversion;
  
  --
  -- DDS processing loop.
  --
  process(wb_clk_i, wb_rst_i)
    variable address      : unsigned(PSK_ROM_ADDRESS_WIDTH-1 downto 0);
    variable mapped_phase : std_logic_vector(NUMBER_OF_SHIFTS downto 0); 
  begin
    if (wb_rst_i = '1') then
      --
      -- Reset signal is set.
      --
      psk_serial_data_in <= '0';
      phase_in  <= (others => '0');
      dds_acc_inc_hi_i <= (others => '0');
      dds_acc_inc_lo_i <= (others => '0');
      
    elsif (rising_edge(wb_clk_i)) then
      --
      -- On the rising edge of the clock...
      --
      if (wb_cyc_i='1' and wb_stb_i='1' and wb_we_i='1') then
        case wb_adr_i(4 downto 2) is
          when "000" =>
            -- 
            -- Store the incoming phase value to the phase shifter.
            --
            psk_serial_data_in <= wb_dat_i(0);
          when "001" =>
            --
            -- Update the DDS increment value.
            --
            dds_acc_inc_hi_i <= 
                std_logic_vector(wb_dat_i(31 downto 31-DDS_INC_HI_WIDTH+1));
            dds_acc_inc_lo_i <= 
                std_logic_vector(wb_dat_i(31-DDS_INC_HI_WIDTH downto 0));
          when others =>
        end case;
      end if;

      -- Load the phase from the accumulator to the phase shifter.
      address := psk_phase_acc_count;
      mapped_phase := phase_shift_rom_array(to_integer(address));     
      phase_in(NUMBER_OF_SHIFTS downto 1) <= 
          mapped_phase(NUMBER_OF_SHIFTS downto 1);
      phase_in(0) <= 
          psk_phase_inversion;
          
    end if;
  end process;
  
  --
  -- Put the output on the address out lines.
  --
  process(wb_adr_i, i_data_out, q_data_out)
  begin
    wb_dat_o <= (others => '0');
    wb_dat_o(NUMBER_OF_SHIFTS+2*IQ_BUS_WIDTH downto 2*IQ_BUS_WIDTH) <= 
        phase_out;
    wb_dat_o(2*IQ_BUS_WIDTH-1 downto IQ_BUS_WIDTH) <= 
        std_logic_vector(i_data_out);
    wb_dat_o(IQ_BUS_WIDTH-1 downto  0) <= 
        std_logic_vector(q_data_out);
        
    i_audio_out <= std_logic_vector(i_data_out + "10000000");
    q_audio_out <= std_logic_vector(q_data_out + "10000000");
  end process;
  --
  -- END ARCHITECTURE CODE
  --
  
end behave;
