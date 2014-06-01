--
--  CORDIC Phase Shifter
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
use ieee.std_logic_1164.ALL;
use ieee.numeric_std.ALL;
use ieee.math_real.ALL;
use ieee.std_logic_misc.ALL;
--
-- Core entity definition.
--
entity zpuino_phase_shifter is
  generic (
    NUMBER_OF_SHIFTS : integer := 7;
    BUS_WIDTH : integer := 8
  );
  port (
    clk:             in  std_logic;
    reset:           in  std_logic;
    i_data_in:       in  signed(BUS_WIDTH-1 downto 0);
    q_data_in:       in  signed(BUS_WIDTH-1 downto 0);
    phase_in:        in  std_logic_vector(NUMBER_OF_SHIFTS downto 0);
    i_data_out:      out signed(BUS_WIDTH-1 downto 0);
    q_data_out:      out signed(BUS_WIDTH-1 downto 0);
    phase_out:       out std_logic_vector(NUMBER_OF_SHIFTS downto 0)
  );
end zpuino_phase_shifter;

--
-- Core description.
--
architecture arch of zpuino_phase_shifter is
  --
  -- Descriptor for data describing a shift block.
  --
  type shift_block is record
    i_data     : signed(BUS_WIDTH+NUMBER_OF_SHIFTS-1 downto 0);
    q_data     : signed(BUS_WIDTH+NUMBER_OF_SHIFTS-1 downto 0);
    phase      : std_logic_vector(NUMBER_OF_SHIFTS   downto 0);
    shift_mask : std_logic_vector(NUMBER_OF_SHIFTS   downto 0);
  end record shift_block;
  constant shift_block_default : shift_block := (
    i_data => (others => '0'),
    q_data => (others => '0'),
    phase  => (others => '0'),
    shift_mask => (others => '0')
  );
  type shift_block_array is array(NUMBER_OF_SHIFTS downto 0) of shift_block;
    
  --
  -- Internal signals.
  --
  signal shift_stream : shift_block_array;
begin
  --
  -- Initialize the shift masks.
  -- 0 bit of the shift mask is always 0.
  -- Bits for the phase shifts go from the lowest order to the highest order.
  -- Lowest order bits are the largest angles.
  -- 0 indicates the angle at this point is negative.
  -- 1 indicates the angle at this point is positive.
  --
  process (reset) is 
    variable shift_mask_init : integer := 1;
  begin
    if (reset = '1') then
      --
      -- Index 0 of the shift stream will hold the incoming data.
      --
      shift_stream(0).shift_mask <= 
          std_logic_vector(to_unsigned(0, NUMBER_OF_SHIFTS));
          
      --
      -- Indices 1 to NUMBER_OF_SHIFTS will contain the shift masks
      -- for each of the stages.
      --
      shift_mask_init := 1;
      for i in 1 to NUMBER_OF_SHIFTS loop
        shift_stream(i).shift_mask <= 
            std_logic_vector(to_unsigned(shift_mask_init, NUMBER_OF_SHIFTS)) & "0";
        shift_mask_init := shift_mask_init * 2;
      end loop;
    end if;
  end process;
  
  --
  -- Connect the last stage to the outputs.
  --
  i_data_out <= shift_stream(NUMBER_OF_SHIFTS).i_data(
      BUS_WIDTH+NUMBER_OF_SHIFTS-1 downto NUMBER_OF_SHIFTS);
  q_data_out <= shift_stream(NUMBER_OF_SHIFTS).q_data(
      BUS_WIDTH+NUMBER_OF_SHIFTS-1 downto NUMBER_OF_SHIFTS);
  phase_out  <= shift_stream(NUMBER_OF_SHIFTS).phase;
      
  --
  -- Clocked process.
  -- Read in the data and pass it through the pipeline.
  --
  process(clk, reset)
    variable shift_right_i : signed(BUS_WIDTH+NUMBER_OF_SHIFTS-1 downto 0);
    variable shift_right_q : signed(BUS_WIDTH+NUMBER_OF_SHIFTS-1 downto 0);
  begin
    if (reset = '1') then
      --
      -- Reset the i and q data in the records.
      -- Don't do the first one since that's connected to the input.
      --
      if phase_in(0) = '0' then
        shift_stream(0).i_data <= signed(
            std_logic_vector(i_data_in) & std_logic_vector(to_unsigned(0, NUMBER_OF_SHIFTS)));
        shift_stream(0).q_data <= signed(
            std_logic_vector(q_data_in) & std_logic_vector(to_unsigned(0, NUMBER_OF_SHIFTS)));
      else
        shift_stream(0).i_data <= -signed(
            std_logic_vector(i_data_in) & std_logic_vector(to_unsigned(0, NUMBER_OF_SHIFTS)));
        shift_stream(0).q_data <= -signed(
            std_logic_vector(q_data_in) & std_logic_vector(to_unsigned(0, NUMBER_OF_SHIFTS)));
      end if;
      shift_stream(0).phase <= phase_in;
      
      for i in 1 to NUMBER_OF_SHIFTS loop
        shift_stream(i).i_data <= (others => '0');
        shift_stream(i).q_data <= (others => '0');
        shift_stream(i).phase  <= (others => '0');
      end loop;
    elsif rising_edge(clk) then
      -- 
      -- Pass the data down through the pipeline.
      --
      if phase_in(0) = '0' then
        shift_stream(0).i_data <= signed(
            std_logic_vector(i_data_in) & std_logic_vector(to_unsigned(0, NUMBER_OF_SHIFTS)));      
        shift_stream(0).q_data <= signed(
            std_logic_vector(q_data_in) & std_logic_vector(to_unsigned(0, NUMBER_OF_SHIFTS)));
      else
        shift_stream(0).i_data <= -signed(
            std_logic_vector(i_data_in) & std_logic_vector(to_unsigned(0, NUMBER_OF_SHIFTS)));      
        shift_stream(0).q_data <= -signed(
            std_logic_vector(q_data_in) & std_logic_vector(to_unsigned(0, NUMBER_OF_SHIFTS)));
      end if;      
      shift_stream(0).phase <= phase_in;
  
      for i in 1 to NUMBER_OF_SHIFTS loop
        --
        -- Calculate shifted values.
        -- Does this way to fix problem with using shift_right for
        -- division.
        --
        shift_right_i := shift_right(shift_stream(i-1).i_data, i-1);
        shift_right_q := shift_right(shift_stream(i-1).q_data, i-1);
        if (shift_right_i < 0) then
          shift_right_i := shift_right_i + 1;
        end if;
        if (shift_right_q < 0) then
          shift_right_q := shift_right_q + 1;
        end if;
        
        if or_reduce(shift_stream(i-1).phase and shift_stream(i).shift_mask) = '0' then
          --
          -- Mask is clear so angle to be rotated at this point is negative. 
          -- d = -1.
          --
          shift_stream(i).i_data <= 
              shift_stream(i-1).i_data + shift_right_q;
          shift_stream(i).q_data <= 
              shift_stream(i-1).q_data - shift_right_i;
          shift_stream(i).phase <= 
              shift_stream(i-1).phase;
        else
          --
          -- Mask is set so angle is to be rotated at this point is positive.  
          -- d = 1.
          --
          shift_stream(i).i_data <= 
              shift_stream(i-1).i_data - shift_right_q; 
          shift_stream(i).q_data <= 
              shift_stream(i-1).q_data + shift_right_i;
          shift_stream(i).phase <= 
              shift_stream(i-1).phase;          
        end if;
      end loop;
    end if;
  end process;
end architecture arch;
