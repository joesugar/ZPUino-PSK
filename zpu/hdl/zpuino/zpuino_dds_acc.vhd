library ieee;
use ieee.std_logic_1164.ALL;
use ieee.numeric_std.ALL;
use ieee.math_real.all;

--
-- Core entity definition
--
entity zpuino_dds_acc is
  generic (
    N_LO: integer := 24;      -- number of lo acc bits
    N_HI: integer := 8;       -- number of hi acc bits
    M_LO: integer := 16777216;-- lo acc modulus - 2**24
    M_HI: integer := 256      -- hi acc modulus - 2**8
  );              
  port (
    clk:    in  std_logic;
    reset:  in  std_logic;
    inc_hi: in  std_logic_vector(N_HI-1 downto 0);
    inc_lo: in  std_logic_vector(N_LO-1 downto 0);
    carry:  out std_logic;
    q:      out std_logic_vector(N_HI-1 downto 0)
  );
end zpuino_dds_acc;

--
-- Core architecture
--
architecture arch of zpuino_dds_acc is
  --
  -- Internal signals
  --
  signal r_reg_lo: unsigned(N_LO-1 downto 0);   -- current lo accumulator
  signal r_next_lo: unsigned(N_LO downto 0);    -- next lo accumulator value
  signal r_reg_hi: unsigned(N_HI-1 downto 0);   -- current hi accumulator
  signal r_next_hi: unsigned(N_HI downto 0);    -- next hi accumulator value
  
begin
  --
  -- register process
  --
  process(clk, reset)
  begin
    if (reset = '1') then
      --
      -- Reset is set
      --
      r_reg_lo <= (others => '0');
      r_reg_hi <= (others => '0');
      
    elsif rising_edge(clk) then
      -- 
      -- Rising edge of the clock
      --
      r_reg_lo <= r_next_lo(N_LO-1 downto 0);
      r_reg_hi <= r_next_hi(N_HI-1 downto 0);
      
    end if;
  end process;
  
  --
  -- Next state.
  --
  process(r_reg_lo)
    --
    -- Temporary variable for range check.
    --
    variable temp_lo: unsigned(N_LO downto 0);
    variable temp_hi: unsigned(N_HI downto 0);
    
  begin
    --
    -- Calculate next value.
    --
    temp_lo := ('0' & r_reg_lo) + unsigned('0' & inc_lo);
    temp_hi := ('0' & r_reg_hi);
    
    --
    -- Lower register.
    --
    if (temp_lo > M_LO-1) then
      -- 
      -- If value exceeds the modulus then increment the next register
      -- and wrap around.
      --
      r_next_lo <= temp_lo - M_LO;
      temp_hi := temp_hi + 1;
    else
      --
      -- If value is still within the modulus clear the carry and keep the value.
      --
      r_next_lo <= temp_lo;
      temp_hi := temp_hi + 0;
    end if;
    
    --
    -- Upper register.
    --
    temp_hi := temp_hi + unsigned('0' & inc_hi);
    if (temp_hi > M_HI-1) then
      --
      -- If value exceeds the modules then set the carry and wrap around.
      --
      r_next_hi <= temp_hi - M_HI;
      carry <= '1';
    
    else
      --
      -- If value is still within the modulus clear the carry and keep the value.
      --
      r_next_hi <= temp_hi;
      carry <= '0';
      
    end if;
  end process;
  
  -- 
  -- Outgoing signals.
  --
  q <= std_logic_vector(r_reg_hi);
  
end arch;
      