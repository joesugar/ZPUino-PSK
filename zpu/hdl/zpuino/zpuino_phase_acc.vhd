--
--  Phase Accumulator
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
library ieee;
use ieee.std_logic_1164.ALL;
use ieee.numeric_std.ALL;
use ieee.math_real.all;

--
-- Core entity definition
-- clk             - system clock
-- reset           - system reset
-- serial_data_in  - indicates if 1 or 0 is being transmitted
-- q               - outgoing counter data
-- inversion       - flips state each time the one shot is triggered
--
entity zpuino_phase_acc is
  generic (
    N_HI:   integer := 8;       -- number of hi acc bits
    N_LO:   integer := 24;      -- number of lo acc bits
    M_HI:   integer := 256;     -- hi acc modulus 
    M_LO:   integer := 12000;   -- lo acc modulus 
    PSK_0:  std_logic := '0';
    PSK_1:  std_logic := '1'
  );              
  port (
    clk:             in  std_logic;
    reset:           in  std_logic;
    serial_data_in:  in  std_logic;
    q:               out unsigned(N_HI-1 downto 0);
    inversion:       out std_logic;
    uart_clock:      out std_logic
  );
end zpuino_phase_acc;

--
-- Core architecture
--
architecture arch of zpuino_phase_acc is
  --
  -- Counter registers.
  --
  signal r_reg_hi:  unsigned(N_HI-1 downto 0);  -- 7 downto 0
  signal r_reg_lo:  unsigned(N_LO-1 downto 0);  -- 23 downto 0
  
  signal r_reg_hi_next: unsigned(N_HI-1 downto 0);
  signal r_reg_lo_next: unsigned(N_LO-1 downto 0);

  signal r_reg_out:   unsigned(N_HI-1 downto 0);  -- register to hold out count 
  signal r_inversion: std_logic;
  --
  -- Generic signals
  --
  signal data_out_enable: std_logic;  -- Output increments when set hi
begin
  --
  -- Internally connected signals.
  --
  q <= r_reg_out;           -- Connect block data out to accumulator data out.
  inversion <= r_inversion; -- Connect inversion data to the out port.
  
  --
  -- Process to set output signals.
  -- For the most part it's moving the next-state signals
  -- to the appropriate state signals.
  --
  process(clk, reset)
    variable carry : integer;
    variable temp_lo : unsigned(N_LO-1 downto 0);
    variable temp_hi : unsigned(N_HI-1 downto 0);
  begin
    if (reset = '1') then
      --
      -- Reset output pins.
      --
      r_reg_hi <= (others => '0');
      r_reg_lo <= (others => '0');
      r_reg_hi_next <= r_reg_hi + 1;
      r_reg_lo_next <= r_reg_lo + 1;
    elsif rising_edge(clk) then
      -- 
      -- Rising edge of the clock moves states to the output pins.
      --
      r_reg_lo <= r_reg_lo_next;
      r_reg_hi <= r_reg_hi_next;
      
      --
      -- Next state calculations.
      -- Next value of the low register.
      --
      temp_lo := r_reg_lo_next + 1;
      carry := 0;
      if (temp_lo = M_LO) then
        temp_lo := to_unsigned(0, N_LO);
        carry := 1;
      end if;
      r_reg_lo_next <= temp_lo;
      
      temp_hi := r_reg_hi_next + carry;
      if (temp_hi = M_HI) then
        temp_hi := to_unsigned(0, N_HI);
      end if;
      r_reg_hi_next <= temp_hi;
    end if;
  end process;
  
  --
  -- Process for sampling the serial data stream.
  --
  process(clk, reset)
  begin
    if (reset = '1') then
      data_out_enable <= '0';
      r_inversion <= '0';
    elsif rising_edge(clk) then
      --
      -- Serial data sample flag.  This flag is set so the serial
      -- data is sampled on the leading edge of the PSK bit period.
      --
      if ((r_reg_lo_next = 0) and (r_reg_hi_next = 0)) then
        --
        -- Calculate the new inversion flag.  If the bit just 
        -- finished was a '1' then you want to keep the same
        -- inversion.  Otherwise, invert it.
        --
        if (data_out_enable = '0') then
          r_inversion <= not(r_inversion);
        else
          r_inversion <= r_inversion;
        end if;
        --
        -- The data out enable flag matches the data being sent.
        --
        data_out_enable <= serial_data_in;
      end if;
    end if;
  end process;
  
  --
  -- Set the output data.
  --
  r_reg_out <= r_reg_hi when (data_out_enable = PSK_0) else (others => '0');
  uart_clock <= '1' when ((r_reg_lo = 0) and (r_reg_hi = 0)) else '0';
  
end arch;
      
