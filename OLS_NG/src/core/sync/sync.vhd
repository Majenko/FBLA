----------------------------------------------------------------------------------
-- sync.vhd
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
-- Synchronizes input with clock on rising or falling edge and does some
-- optional preprocessing. (Noise filter and demux.)
--
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity sync is
port (
    reset           : in  std_logic;
    clock           : in  std_logic;
    input           : in  std_logic_vector(31 downto 0);
    enableFilter    : in  std_logic;
    enableDemux     : in  std_logic;
    falling         : in  std_logic;
    output          : out std_logic_vector(31 downto 0)
    );
end sync;


--**************************************************************************************************
ARCHITECTURE rtl OF sync IS
--**************************************************************************************************

signal filteredInput                : std_logic_vector(31 downto 0); 
signal demuxedInput                 : std_logic_vector(31 downto 0); 
signal synchronizedInput            : std_logic_vector(31 downto 0); 
signal synchronizedInput180         : std_logic_vector(31 downto 0);


component demux
port (
    input       : in  std_logic_vector(15 downto 0);
    input180    : in  std_logic_vector(15 downto 0);
    clock       : in  std_logic;
    output      : out std_logic_vector(31 downto 0)
    );
end component;

component filter
port (
   input        : in  std_logic_vector(31 downto 0);
   input180     : in  std_logic_vector(31 downto 0);
   clock        : in  std_logic;
   output       : out std_logic_vector(31 downto 0)
   );
end component;


--**************************************************************************************************
BEGIN
--**************************************************************************************************

i_demux: demux 
port map (
   input        => synchronizedInput(15 downto 0),
   input180     => synchronizedInput180(15 downto 0),
   clock        => clock,
   output       => demuxedInput
    );

i_filter: filter 
port map (
   input        => synchronizedInput,
   input180     => synchronizedInput180,
   clock        => clock,
   output       => filteredInput
);

-- ===  synch input guarantees use of iob ff on spartan 3 (as filter and demux do)  === --
process (clock)
begin
    if rising_edge(clock) then
        synchronizedInput <= input;
    end if;
    if falling_edge(clock) then
        synchronizedInput180 <= input;
    end if;
end process;

-- ===  add another pipeline step for input selector to not decrease maximum clock rate  === --
process (reset, clock) 
begin
    if (reset = '1') then
        output <= (others => '0');
    elsif rising_edge(clock) then
        if enableDemux = '1' then
            output <= demuxedInput;
        else
            if enableFilter = '1' then
                output <= filteredInput;
            else
                if falling = '1' then
                    output <= synchronizedInput180;
                else
                    output <= synchronizedInput;
                end if;
            end if;
        end if;
    end if;
end process;

--**************************************************************************************************
END rtl;
--**************************************************************************************************