----------------------------------------------------------------------------------
-- controller.vhd
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
-- Controls the capturing & readback operation.
-- 
-- If no other operation has been activated, the controller samples data
-- into the memory. When the run signal is received, it continues to do so
-- for fwd * 4 samples and then sends bwd * 4 samples  to the transmitter.
-- This allows to capture data from before the trigger match which is a nice 
-- feature.
--
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity controller is
port (
    clock           : in  std_logic;
    reset           : in  std_logic;
    input           : in  std_logic_vector(31 downto 0);    
    inputReady      : in  std_logic;
    data            : in  std_logic_vector(31 downto 0);
    wrFlags         : in  std_logic;    
    wrSize          : in  std_logic;
    run             : in  std_logic;
    busy            : in  std_logic;
    send            : buffer std_logic;
    output          : out std_logic_vector(31 downto 0);
    memoryIn        : in  std_logic_vector(31 downto 0);
    memoryOut       : out std_logic_vector(31 downto 0);
    memoryRead      : out std_logic;
    memoryWrite     : out std_logic
    );
end controller;


--**************************************************************************************************
ARCHITECTURE rtl OF controller IS
--**************************************************************************************************

type CONTROLLER_STATES is (SAMPLE, DELAY, WRITEWAIT, READ, READWAIT);

signal fwd                  : std_logic_vector(15 downto 0); 
signal bwd                  : std_logic_vector(15 downto 0);
signal disabledGroups       : std_logic_vector(3 downto 0);
signal ncounter             : unsigned(17 downto 0); 
signal counter              : unsigned(17 downto 0) ;
signal nWaitCnt             : unsigned(3 downto 0); 
signal WaitCnt              : unsigned(3 downto 0);
signal nstate               : CONTROLLER_STATES; 
signal state                : CONTROLLER_STATES;
signal sendReg              : std_logic;


--**************************************************************************************************
BEGIN
--**************************************************************************************************

output    <= memoryIn;
memoryOut <= input;
-- send      <= sendReg;

-- ===  synchronization and reset logic  === --
process(reset, clock) 
begin
    if (reset = '1') then
        state   <= SAMPLE;
        counter <= (others => '0');
        WaitCnt <= (others => '0');
        send    <= '0';
    elsif rising_edge(clock) then
        state   <= nstate;
        counter <= ncounter;
        WaitCnt <= nWaitCnt;
        send    <= sendReg;
    end if;
end process;

-- -- ===  Adjusts memory connections to allow different memory depths  === --
-- process(clock, disabledGroups)
-- begin
    -- if rising_edge(clock) then
        -- case disabledGroups is
            -- when "0011" =>                                              -- Second two channels disabled frees up half the physical memory
                -- if (counter <= x"1000") then                            -- When counter is below the amount of physical memory. This number must match the width of the memory address bus. 2^(address width)
                    -- output(15 downto 0)    <= memoryIn(15 downto 0);
                    -- memoryOut(15 downto 0) <= input(15 downto 0);
                -- else                                                    -- When we run out of memory then switch to free memory.
                    -- output(15 downto 0)     <= memoryIn(31 downto 16);
                    -- memoryOut(31 downto 16) <= input(15 downto 0);
                -- end if;
            -- when others =>
                -- output    <= memoryIn;
                -- memoryOut <= input;
        -- end case;
    -- end if;  
-- end process;

-- ===  FSM to control the controller action  === --
process(state, run, counter, WaitCnt, fwd, inputReady, bwd, busy, send)
begin
    nstate   <= state;
    ncounter <= counter;
    nWaitCnt <= WaitCnt;
    
    sendReg     <= '0';
    memoryRead  <= '0';
    memoryWrite <= '0';
    
    case state is
    
        -- ===  default mode: sample data from input to memory  === --
        when SAMPLE =>
            if (run = '1') then
                nstate <= DELAY;
            end if;
            
            ncounter    <= (others => '0');
            memoryWrite <= not(busy) and inputReady;          -- mit dem Schreiben warten, bis das letzte Byte übertragen worden ist
    
        -- ===  keep sampling for 4 * fwd + 4 samples after run condition  === --
        when DELAY =>
            if (std_logic_vector(counter) = fwd & "11") then
                ncounter <= (others => '0');
                -- nstate      <= READ;
                nWaitCnt <= (others => '1');
                nstate   <= WRITEWAIT;
            else
                if (inputReady = '1') then
                    ncounter <= counter + 1;
                end if;
            end if;
            
            memoryWrite <= inputReady;
    
        when WRITEWAIT =>
            if (WaitCnt = 0) then
                nstate   <= READ;
            else
                nWaitCnt <= WaitCnt - 1;
            end if;   
                
        -- ===  read back 4 * bwd + 4 samples after DELAY  === --
        -- ===  go into wait state after each sample to give transmitter time  === --
        when READ =>
            if (std_logic_vector(counter) = bwd & "11") then
                ncounter <= (others => '0');
                nstate   <= SAMPLE;
            else
                ncounter <= counter + 1;
                nstate   <= READWAIT;
            end if;
            
            memoryRead  <= '1';
            sendReg     <= '1';
    
        -- ===  wait for the transmitter to become ready again  === --
        when READWAIT =>
            if ((busy = '0') and (send = '0') and (sendReg = '0')) then
                nstate <= READ;
            end if;
    end case;
end process;

-- ===  set speed and size registers if indicated  === --
process(reset, clock)
begin
    if (reset = '1') then 
        fwd             <= "0000000000000100";
        bwd             <= "0000000000000100";
        disabledGroups  <= "0000";
    elsif rising_edge(clock) then
    
        if (wrSize = '1') then
            fwd <= data(31 downto 16);                  -- Read Count
            bwd <= data(15 downto 0);                   -- Delay Count
        end if;
        
        if (wrFlags = '1') then
            disabledGroups <= data(5 downto 2);
        end if;
    end if;
end process;
   
   
--**************************************************************************************************
END rtl;
--**************************************************************************************************