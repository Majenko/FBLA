----------------------------------------------------------------------------------
-- decoder.vhd
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
-- Takes the opcode from the command received by the receiver and decodes it.
-- The decoded command will be executed for one cycle.
--
-- The receiver keeps the cmd output active long enough so all the
-- data is still available on its cmd output when the command has
-- been decoded and sent out to other modules with the next
-- clock cycle. (Maybe this paragraph should go in receiver.vhd?)
--
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity decoder is
port (
    reset       : in  std_logic;
    clock       : in  std_logic;
    opcode      : in  std_logic_vector(7 downto 0);
    execute     : in  std_logic;
    wrtrigmask  : out std_logic_vector(3 downto 0);
    wrtrigval   : out std_logic_vector(3 downto 0);
    wrtrigcfg   : out std_logic_vector(3 downto 0);
    wrspeed     : out std_logic;
    wrsize      : out std_logic;
    wrFlags     : out std_logic;
    arm         : out std_logic;
    -- testmode    : out std_logic;
    IntReset    : out std_logic
    );
end decoder;

--**************************************************************************************************
ARCHITECTURE rtl OF decoder IS
--**************************************************************************************************

signal exe          : std_logic;
signal exeReg       : std_logic;

--**************************************************************************************************
BEGIN
--**************************************************************************************************

    exe <= execute;
    
p_decode: process(reset, clock)
begin
    if (reset = '1') then
        IntReset   <= '0'; 
        arm        <= '0'; 
        wrspeed    <= '0'; 
        wrsize     <= '0'; 
        wrFlags    <= '0';
        wrtrigmask <= "0000"; 
        wrtrigval  <= "0000"; 
        wrtrigcfg  <= "0000";
        exeReg     <= '0';
        
    elsif rising_edge(clock) then
        IntReset   <= '0'; 
        arm        <= '0'; 
        wrspeed    <= '0'; 
        wrsize     <= '0'; 
        wrFlags    <= '0';
        wrtrigmask <= "0000"; 
        wrtrigval  <= "0000"; 
        wrtrigcfg  <= "0000";
    
        if ((exe and not exeReg) = '1') then
            case opcode is
                -- ===  short commands  === --
                when x"00" => IntReset <= '1';
                when x"01" => arm <= '1';
                -- when x"03" => testmode <= '1';
                -- ===  long commands  === --
                when x"80" => wrspeed <= '1';
                when x"81" => wrsize <= '1';
                when x"82" => wrFlags <= '1';
                when x"C0" => wrtrigmask(0) <= '1';
                when x"C1" => wrtrigval(0) <= '1';
                when x"C2" => wrtrigcfg(0) <= '1';
                when x"C4" => wrtrigmask(1) <= '1';
                when x"C5" => wrtrigval(1) <= '1';
                when x"C6" => wrtrigcfg(1) <= '1';
                when x"C8" => wrtrigmask(2) <= '1';
                when x"C9" => wrtrigval(2) <= '1';
                when x"CA" => wrtrigcfg(2) <= '1';
                when x"CC" => wrtrigmask(3) <= '1';
                when x"CD" => wrtrigval(3) <= '1';
                when x"CE" => wrtrigcfg(3) <= '1';
                when others =>
            end case;
        end if;
    
        exeReg <= exe;
    end if;
end process p_decode;
    
--**************************************************************************************************
END rtl;
--**************************************************************************************************