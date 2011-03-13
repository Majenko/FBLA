----------------------------------------------------------------------------------
-- prescaler.vhd
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
-- Shared prescaler for transmitter and receiver timings.
-- Used to control the transfer speed.
--
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity prescaler is
generic (
    SCALE   : integer
    );
port ( 
    clock   : in  std_logic;
    reset   : in  std_logic;
    div     : in  std_logic_vector(1 downto 0);
    scaled  : out std_logic
    );
end prescaler;


--**************************************************************************************************
ARCHITECTURE rtl OF prescaler IS
--**************************************************************************************************

signal counter : integer range 0 to (6 * SCALE) - 1;


--**************************************************************************************************
BEGIN
--**************************************************************************************************

process(clock, reset)
begin
    if reset = '1' then
        counter <= 0;
        scaled  <= '0';
    elsif rising_edge(clock) then
        -- if    ((counter =     SCALE - 1) and (div = "00"))     -- 115200
           -- or ((counter = 2 * SCALE - 1) and (div = "01"))     --  57600
           -- or ((counter = 3 * SCALE - 1) and (div = "10"))     --  38400
           -- or ((counter = 6 * SCALE - 1) and (div = "11"))     --  19200
        if    ((counter =     SCALE - 1) and (div = "00"))     --  230400
           or ((counter = 2 * SCALE - 1) and (div = "01"))     --  115200
           or ((counter = 4 * SCALE - 1) and (div = "10"))     --  57600
           or ((counter = 6 * SCALE - 1) and (div = "11"))     --  38400
        then
            counter <= 0;
            scaled  <= '1';
        else
            counter <= counter + 1;
            scaled  <= '0';
        end if;
    end if;
end process;

   
--**************************************************************************************************
END rtl;
--**************************************************************************************************