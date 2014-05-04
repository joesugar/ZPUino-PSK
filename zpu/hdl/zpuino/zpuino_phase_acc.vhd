library ieee;
use ieee.std_logic_1164.ALL;
use ieee.numeric_std.ALL;
use ieee.math_real.all;

--
-- Core entity definition
-- clk             - system clock
-- reset           - system reset
-- data_out_enable - clocked in at beginning of cycle.  If hi, 
--                   the accumulator count will be mirrored on
--                   the data out pins.  If lo, the data out
--                   pins will remain at 0 during the cycle.
-- q               - data out
-- carry           - set hi when accumulator count is at the
--                   final count of the cycle, lo otherwise.
--
entity zpuino_phase_acc is
  generic (
    N_LO: integer := 24;      -- number of lo acc bits
    N_HI: integer := 8;       -- number of hi acc bits
    M_LO: integer := 12000;   -- lo acc modulus 
    M_HI: integer := 256      -- hi acc modulus 
  );              
  port (
    clk:             in  std_logic;
    reset:           in  std_logic;
    data_out_enable: in  std_logic;
    q:               out std_logic_vector(N_HI-1 downto 0);
    carry:           out std_logic
  );
end zpuino_phase_acc;

--
-- Core architecture
--
architecture arch of zpuino_phase_acc is
  --
  -- Internal signals
  --
  signal r_reg_lo: unsigned(N_LO-1 downto 0); -- current lo accumulator
  signal r_next_lo: unsigned(N_LO downto 0);  -- next lo accumulator value
  signal r_reg_hi: unsigned(N_HI-1 downto 0); -- current hi accumulator
  signal r_next_hi: unsigned(N_HI downto 0);  -- next hi accumulator value
  signal next_carry: std_logic;                -- state machine end of psk bit
  signal data_out_enabled: std_logic;          -- latch for data_out_enable
  signal data_out_latch: std_logic;            -- signal indicates when it is
                                                -- time to latch data_out_enabled
  
begin
  --
  -- Process to set output signals.
  -- For the most part it's moving the next-state signals
  -- to the appropriate state signals.
  --
  process(clk, reset)
  begin
    if (reset = '1') then
      --
      -- Reset output pins.
      --
      r_reg_lo <= (others => '0');
      r_reg_hi <= (others => '0');
      q <= (others => '0');
      data_out_enabled <= '0';
      carry <= '0';
            
    elsif rising_edge(clk) then
      -- 
      -- Rising edge of the clock moves states to the output pins.
      --
      r_reg_lo <= r_next_lo(N_LO-1 downto 0);
      r_reg_hi <= r_next_hi(N_HI-1 downto 0);
      
      if (data_out_enabled = '1') then
        q <= std_logic_vector(r_next_hi(N_HI-1 downto 0));
      else
        q <= (others => '0');
      end if;
      
      if (data_out_latch = '1') then
        data_out_enabled <= data_out_enable;
      else
        data_out_enabled <= data_out_enabled;
      end if;
      
      carry <= next_carry;    
                     
    end if;
  end process;
  
  --
  -- Calculate the next-state signals.
  --
  process(r_reg_lo, reset)
    --
    -- Temporary variable for range check.
    --
    variable temp_lo: unsigned(N_LO downto 0);
    variable temp_hi: unsigned(N_HI downto 0);
    
  begin
    if (reset = '1') then
      -- 
      -- Reset the register variables.
      --
      r_next_lo <= (others => '0');
      r_next_hi <= (others => '0');
    else  
      --
      -- Calculate the next register values.
      --
      temp_lo := ('0' & r_reg_lo) + 1;
      temp_hi := ('0' & r_reg_hi);
    
      --
      -- Set next state values according to the current state
      --
      -- First the registers.
      --
      if (temp_lo = M_LO) then
        -- 
        -- If value equals the modulus then increment the next register
        -- and wrap back to zero.
        --
        r_next_lo <= (others => '0');
        temp_hi := temp_hi + 1;
      
      else
        --
        -- If value is still within the modulus keep the value.
        --
        r_next_lo <= temp_lo;
        temp_hi := temp_hi + 0;
      
      end if;
      
      --
      -- Upper register.
      --
      if (temp_hi = M_HI) then
        --
        -- If value exceeds the modules then wrap around.
        --
        r_next_hi <= (others => '0');
    
      else
        --
        -- If value is still within the modulus keep the value.
        --
        r_next_hi <= temp_hi;
      
      end if;
    end if;
          
    --
    -- End of bit flag.
    --
    if ((r_next_lo = M_LO-1) and (r_next_hi = M_HI-1)) then
      next_carry <= '1';
    else
      next_carry <= '0';
    end if; 
    
    --
    -- Data out latch signal.
    --
    if ((r_next_lo = 0) and (r_next_hi = 0)) then
      data_out_latch <= '1';
    else
      data_out_latch <= '0';
    end if;
  end process;
  
end arch;
      
