----------------------------------------------------------------------------------
-- transmitter.vhd
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
-- Takes 32bit (one sample) and sends it out on the serial port.
-- end of transmission is signalled by taking back the busy flag.
-- Supports xon/xoff flow control.
--
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity transmitter is
generic (
    FREQ            : integer;
    RATE            : integer
    );          
port (
    data            : in  std_logic_vector(31 downto 0);
    disabledGroups  : in  std_logic_vector(3 downto 0);
    write           : in  std_logic;
    id              : in  std_logic;
    meta            : in  std_logic;
    xon             : in  std_logic;
    xoff            : in  std_logic;
    clock           : in  std_logic;
    trxClock        : in  std_logic;
    reset           : in  std_logic;
    tx              : out std_logic;
    busy            : out std_logic
    -- pause           : out std_logic
    );
end transmitter;


--**************************************************************************************************
ARCHITECTURE rtl OF transmitter IS
--**************************************************************************************************

-- constant cMetaData_DeviceName    : string := character'val(1) & "Brevia Logic Sniffer" & character'val(0);
-- constant cMetaData_Version       : string := character'val(2) & "v0.01" & character'val(0);

-- constant cMetaData : string := cMetaData_DeviceName & cMetaData_Version;
-- signal ls_MetaCnt : integer range 0 to cMetaData'high-1;

type TX_STATES is (IDLE, SEND, SYNC, POLL);

constant BITLENGTH : integer := FREQ / RATE;
constant cBits     : integer := 10;

signal dataBuffer       : std_logic_vector(31 downto 0);
signal disabledBuffer   : std_logic_vector(3 downto 0);
signal txBuffer         : std_logic_vector(9 downto 0);
signal byte             : std_logic_vector(7 downto 0);
signal counter          : integer range 0 to BITLENGTH;
signal bits             : integer range 0 to cBits-1;
signal state            : TX_STATES;
signal paused           : std_logic;
signal writeByte        : std_logic;
signal byteDone         : std_logic;


--**************************************************************************************************
BEGIN
--**************************************************************************************************

-- pause <= paused;
tx <= txBuffer(0);

-- ===  sends one byte  === --
process(reset, clock)
begin
    if (reset = '1') then 
        counter     <= 0;
        bits        <= 0;
        byteDone    <= '0';
        txBuffer    <= (others => '1');
    
    elsif rising_edge(clock) then
        if (writeByte = '1') then
            counter  <= 0;
            bits     <= 0;
            byteDone <= '0';
            txBuffer <= '1' & byte & "0";
        elsif ( (counter = BITLENGTH) or ((bits = cBits-1) and (counter = BITLENGTH-1)) ) then
            counter  <= 0;
            txBuffer <= '1' & txBuffer(9 downto 1);
            if (bits = cBits-1) then
                byteDone <= '1';
            else
                bits <= bits + 1;
            end if;
        elsif (trxClock = '1') then
            counter <= counter + 1;
        end if;
    end if;
end process;

-- ===  control mechanism for sending a 32 bit word  === --
process(clock, reset)
begin

    if reset = '1' then
        writeByte       <= '0';
        state           <= IDLE;
        dataBuffer      <= (others => '0');
        disabledBuffer  <= (others => '0');
        byte            <= (others => '0');
        busy            <= '0';

    elsif rising_edge(clock) then
        if ((state /= IDLE) or (write = '1') or (paused = '1')) then
            busy <= '1';
        else
            busy <= '0';
        end if;

        case state is
            
            -- ===  when write is '1', data will be available with next cycle  === --
            when IDLE =>
                if (write = '1') then
                    dataBuffer      <= data;
                    disabledBuffer  <= disabledGroups;
                    state           <= SEND;
                elsif id = '1' then
                    dataBuffer      <= x"534C4131";            -- Protokol: SLA1
                    disabledBuffer  <= "0000";
                    state           <= SEND;
                -- elsif (meta = '1') then
                    -- -- dataBuffer      <= x"014D5300";            -- Metadata device name : MS
                    -- dataBuffer      <= x"00534D01";            -- Metadata device name : MS
                    -- disabledBuffer  <= "0000";
                    -- state           <= SEND;
                end if;
            
            when SEND =>
                state <= SYNC;
                        
                if (disabledBuffer(0) = '0') then 
                    byte <= dataBuffer(7 downto 0);
                    disabledBuffer(0) <= '1';
                elsif (disabledBuffer(1) = '0') then 
                    byte <= dataBuffer(15 downto 8);
                    disabledBuffer(1) <= '1';
                elsif (disabledBuffer(2) = '0') then 
                    byte <= dataBuffer(23 downto 16);
                    disabledBuffer(2) <= '1';
                elsif (disabledBuffer(3) = '0') then 
                    byte <= dataBuffer(31 downto 24);
                    disabledBuffer(3) <= '1';
                else 
                    state <= IDLE;
                end if;

            when SYNC =>
                if (trxClock = '1') then
                    writeByte <= '1';
                    state     <= POLL;
                end if;

            when POLL =>
                writeByte <= '0';
                if ((byteDone = '1') and (writeByte = '0') and (paused = '0')) then
                    state <= SEND;
                end if;
            
        end case;
    end if;
end process;

-- ===  set paused mode according to xon/xoff commands  === --
process(clock, reset)
begin
    if reset = '1' then
        paused <= '0';
    elsif rising_edge(clock) then
        if xon = '1' then
            paused <= '0';
        elsif xoff = '1' then
            paused <= '1';
        end if;
    end if;
end process;


-- === nur zur Info ausgeben  === --
assert (1 = 2)                                                                                      -- ohne 'assert' wird bei der Synthese (Synplify) kein Report ausgeben
    report "Transmitter: " &
        "FREQ      = "  & integer'image(FREQ) & ", " &
        "RATE      = "  & integer'image(RATE) & ", " &
        "BITLENGTH = "  & integer'image(BITLENGTH)
    severity NOTE;

--**************************************************************************************************
END rtl;
--**************************************************************************************************