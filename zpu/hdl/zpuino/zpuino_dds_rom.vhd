--
--  DDS ROM
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
-- Entity prototype.
--
entity zpuino_dds_rom is
  generic (
    ADDR_WIDTH       : integer := 8;        
    DATA_WIDTH       : integer := 8
  );
  port (
    addr_i    : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);          
    data_o    : out signed(DATA_WIDTH - 1 downto 0)
  );
end zpuino_dds_rom;

--
-- Entity architecture.
--
architecture rtl of zpuino_dds_rom is
  -- 
  -- Defined constants.
  -- Memory has 250 8-bit signed values.
  --
  constant MEM_DEPTH : integer := 2**DATA_WIDTH;
  type mem_type is array (0 to MEM_DEPTH - 1) of signed(DATA_WIDTH - 1 downto 0);

  --
  -- Function to define the contents of the ROM
  --
  function init_mem return mem_type is
    constant SCALE : real := 2**(real(DATA_WIDTH - 2));
    variable temp_mem : mem_type;
  begin
    for i in 0 to MEM_DEPTH - 1 loop
      temp_mem(i) := 
        to_signed(integer(SCALE * cos(2.0 * MATH_PI * real(i) / real(MEM_DEPTH))), DATA_WIDTH); 
    end loop;
    return temp_mem;
  end;

  constant mem : mem_type := init_mem;

  begin

  process (addr_i)
  begin
	  data_o <= mem(to_integer(unsigned(addr_i)));
  end process;

end rtl;
