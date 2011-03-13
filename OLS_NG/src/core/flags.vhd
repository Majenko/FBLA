----------------------------------------------------------------------------------
-- flags.vhd
--
-- Copyright (C) 2006 Michael Poppitz
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
-- Flags register.
--
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity flags is
port (
    data            : in  std_logic_vector(10 downto 0);
    clock           : in  std_logic;
    write           : in  std_logic;
    demux           : out std_logic;
    filter          : out std_logic;
    external        : out std_logic;
    inverted        : out std_logic;
    rle             : out std_logic;
    numberScheme    : out std_logic;
    testMode        : out std_logic
    );
end flags;


--**************************************************************************************************
ARCHITECTURE rtl OF flags IS
--**************************************************************************************************


--**************************************************************************************************
BEGIN
--**************************************************************************************************

-- ===  write flags  === --
process (clock)
begin

    --synthesis translate_off
    demux          <= '0';
    filter         <= '0';
    external       <= '0';
    inverted       <= '0';
    rle            <= '1';
    numberScheme   <= '0';
    testMode       <= '1';
    --synthesis translate_on

    if rising_edge(clock) then
        if (write = '1') then
            demux        <= data(0);
            filter       <= data(1);
            external     <= data(6);
            inverted     <= data(7);
            rle          <= data(8);
            numberScheme <= data(9);
            testMode     <= data(10);
        end if;
    end if;
end process;
    
--**************************************************************************************************
END rtl;
--**************************************************************************************************

