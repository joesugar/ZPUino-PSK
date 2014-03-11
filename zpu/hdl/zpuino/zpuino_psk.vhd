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
    pskwidth: integer := 8
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
  -- Define the ROM used to hold the NCO values.
  --
  component zpuino_psk_rom is
    port (
      addr_i    : in  std_logic_vector(7 downto 0); -- 8 bits wide
      data_o    : out signed(7 downto 0)            -- 8 bits wide
  );
  end component zpuino_psk_rom;
  
  signal psk_dat_o : std_logic_vector(pskwidth - 1 downto 0); -- psk output signal
  signal rom_addr_i : std_logic_vector(7 downto 0);           -- psk rom address
  signal psk_rom_o : signed(7 downto 0);                      -- rom output

begin
  --
  -- Declare component instances.
  --
  -- Instance of the NCO rom.
  --
  psk_rom: zpuino_psk_rom
    port map (
      addr_i  => rom_addr_i,
      data_o  => psk_rom_o
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
  -- Outgoing signals
  -- Data out is taken from the upper bits of the rom data.
  --
  psk_dat_o(pskwidth - 1 downto 0) <= 
    std_logic_vector(psk_rom_o(7 downto 7 - pskwidth + 1));
  tx <= psk_dat_o;    -- Direct connection.
  
  --
  -- Process to set the rom address.
  --
  process(wb_clk_i)
  begin
    --
    -- On the rising edge of the clock...
    ---
    if rising_edge(wb_clk_i) then
      --
      -- If the reset is enabled reset the block
      -- Otherwise process incoming data.
      --
      if wb_rst_i = '1' then
        --
        -- Reset is enabled so set the rom address to 0.
        --
        rom_addr_i <= (others => '0');
    
      elsif wb_cyc_i = '1' and wb_stb_i = '1' and wb_we_i = '1' then
        --
        -- Load the rom address form the incoming data.
        --
        rom_addr_i <= wb_dat_i(7 downto 0);
      
      end if;
    end if;
  end process;
end behave;
