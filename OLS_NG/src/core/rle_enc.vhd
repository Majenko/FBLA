----------------------------------------------------------------------------------
-- rle_enc.vhd
--
-- Copyright (C) 2007 Jonas Diemer
-- 
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or (at
-- your option) any later version.
--
-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License along
-- with this program; if not, write to the Free Software Foundation, Inc.,
-- 51 Franklin St, Fifth Floor, Boston, MA 02110, USA
--
----------------------------------------------------------------------------------
--
-- Details: http://www.sump.org/projects/analyzer/
--
-- Run Length Encoder
--
-- If enabled, encode the incoming data with the following scheme:
-- The MSB (bit 31) is used as a flag for the encoding.
-- If the MSB is clear, the datum represents "regular" data
--  if set, the datum represents the number of repetitions of the previous data
--
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity rle_enc is
generic (
    MEMORY_DEPTH : integer := 6
    ); 
port (
   clock    : in  std_logic;
   reset    : in  std_logic;
   dataIn   : in  std_logic_vector(31 downto 0);
   validIn  : in  std_logic;
   enable   : in  std_logic;
   dataOut  : out std_logic_vector(31 downto 0);
   validOut : out std_logic
   );
end rle_enc;

--**************************************************************************************************
ARCHITECTURE rtl OF rle_enc IS
--**************************************************************************************************

signal old              : std_logic_vector(MEMORY_DEPTH-2 downto 0);
signal dout             : std_logic_vector(31 downto 0);

signal ctr              : unsigned(MEMORY_DEPTH-2 downto 0);
constant cCtrOverflow   : unsigned(MEMORY_DEPTH-2 downto 0) := (others => '1');

signal ctrOverflow      : std_logic;

signal valid            : std_logic;
signal valout           : std_logic;


--**************************************************************************************************
BEGIN
--**************************************************************************************************

dataOut  <= dataIn  when (enable = '0') else dout;
validOut <= validIn when (enable = '0') else valout;

-- ===  shift register  === --
process(clock, reset)
begin
	if (reset = '1') then
		valid <= '0';
        old   <= (others => '0');
	elsif rising_edge(clock) then
		if (validIn = '1') then
            old(MEMORY_DEPTH-2 downto 0) <= dataIn(MEMORY_DEPTH-2 downto 0);		
            valid <= '1';
		end if;
	end if;
end process;


-- ===  RLE encoder  === --
process(clock, reset)
begin
	if (reset = '1') then
		ctr         <= (others => '0');
        ctrOverflow <= '0';
        dout        <= (others => '0');
        valout      <= '0';
	elsif rising_edge(clock) then
    
		valout      <= '0';                                                             -- default
        
		if (enable = '0') then  
			ctr         <= (others => '0');
            ctrOverflow <= '0';
		elsif ((valid = '1') and (validIn = '1')) then
			if (old(MEMORY_DEPTH-2 downto 0) = dataIn(MEMORY_DEPTH-2 downto 0)) then
				if ((ctr = 0) and (CtrOverflow = '0')) then                             -- write first datum of series
					dout(MEMORY_DEPTH-1)          <= '0';
					dout(MEMORY_DEPTH-2 downto 0) <= dataIn(MEMORY_DEPTH-2 downto 0);   -- discard MSB, which is used for encoding a count
					valout                        <= '1';
				elsif (ctr = cCtrOverflow) then                                         -- ctr overflow
					dout(MEMORY_DEPTH-1)          <= '1';
					dout(MEMORY_DEPTH-2 downto 0) <= std_logic_vector(ctr(MEMORY_DEPTH-2 downto 0));	    -- write count
					valout                        <= '1';
					ctr                           <= (others => '0');                   -- reset count, so "series" starts again. 
                    ctrOverflow                   <= '1';
				end if;
				ctr <= ctr + 1;
			else                                                                        -- series complete, write count (or data, if series was 0 or 1 long)
				valout <= '1';
				ctr    <= (others => '0');
				if (ctr > 1) then                                                       -- TODO: try if /=0 and /=1 is faster than >1
					dout(MEMORY_DEPTH-1) <= '1';
					dout(MEMORY_DEPTH-2 downto 0) <= std_logic_vector(ctr(MEMORY_DEPTH-2 downto 0));
				else
					dout(MEMORY_DEPTH-1) <= '0';
					dout(MEMORY_DEPTH-2 downto 0) <= old(MEMORY_DEPTH-2 downto 0);
				end if;
                ctrOverflow <= '0';
		    end if;
		end if;
	end if;
end process;

--**************************************************************************************************
END rtl;
--**************************************************************************************************