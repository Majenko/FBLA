----------------------------------------------------------------------------------
-- sram_bram.vhd
--
-- Copyright (C) 2011 Mario Schrenk
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
-- Simple SRAM interface.
--
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity sram_bram is
generic (
    ADDRESS_WIDTH : integer := 12;
    MEMORY_DEPTH  : integer := 24
    );
port (
    reset   : in  std_logic;
    clock   : in  std_logic;
    write   : in  std_logic;
    input   : in  std_logic_vector(35 downto 0);
    read    : in  std_logic;
    output  : out std_logic_vector(35 downto 0)
    );
end entity sram_bram;


--**************************************************************************************************
ARCHITECTURE rtl OF sram_bram IS
--**************************************************************************************************


-- constant cSRAM_Size         : integer := 131072;               -- Static RAM (128kB extern)

-- constant cBRAM_Size         : integer :=  2048;                -- Block RAM (4 EBRs)
-- constant cBRAM_Size         : integer :=  4096;                -- Block RAM (8 EBRs)
constant cBRAM_Size         : integer :=  4608;                -- Block RAM (9 EBRs)

-- constant cDRAM_Size         : integer :=   128;                -- Distributet RAM (LUTs, Slices)
-- constant cDRAM_Size         : integer :=   160;                -- Distributet RAM (LUTs, Slices)
-- constant cDRAM_Size         : integer :=   176;                -- Distributet RAM (LUTs, Slices)
-- constant cDRAM_Size         : integer :=   224;                -- Distributet RAM (LUTs, Slices)
constant cDRAM_Size         : integer :=   240;                -- Distributet RAM (LUTs, Slices)


-- constant cAddresses         : integer := cBRAM_Size;
constant cAddresses         : integer := cBRAM_Size + cDRAM_Size;
constant cMaxAddress        : integer := cAddresses - 1;


signal ls_BRAM_WriteEnable  : std_logic;
signal ls_DRAM_WriteEnable  : std_logic;

signal li_BRAM_ls_q         : std_logic_vector(35 downto 0);
signal li_DRAM_ls_q         : std_logic_vector(35 downto 0);

signal ls_Address           : unsigned (ADDRESS_WIDTH downto 0) := (others => '0');
signal lsc_MemAddress       : unsigned (ADDRESS_WIDTH downto 0) := (others => '0');
signal lsc_nextReadAddress  : unsigned (ADDRESS_WIDTH downto 0) := (others => '0');
signal lsc_nextwriteAddress : unsigned (ADDRESS_WIDTH downto 0) := (others => '0');

signal ls_WriteData         : std_logic_vector(35 downto 0);

signal ls_WriteReadCnt      : integer range 0 to cAddresses;            -- Zähler für Ausgabe ausblenden, wenn max. Speicherbereich gelesen wurde


--**************************************************************************************************
BEGIN
--**************************************************************************************************

-- -------------------------------------------------
-- -- Abbildungen Extern -> Intern
-- -------------------------------------------------

-- b_MapExt2Int: block
-- begin
-- end block b_MapExt2Int;

-------------------------------------------------
-- Abbildungen Intern <-> Intern
-------------------------------------------------

b_MapInt2Int: block
begin
    lsc_nextReadAddress     <= ls_Address - 1 when (ls_Address > 0)           else to_unsigned(cMaxAddress, ADDRESS_WIDTH+1);
    lsc_nextWriteAddress    <= ls_Address + 1 when (ls_Address < cMaxAddress) else (others => '0');
    
    -- ===  bei einem Lesezugriff die Adresse kombinatorisch erhöhen, damit beim  === --
    -- ===  nahtlosen anschließenend Lesezugriff auch das neue Datum bereitsteht  === --
    lsc_MemAddress <= lsc_nextReadAddress when (read = '1') else ls_Address;
end block b_MapInt2Int;

-- -------------------------------------------------
-- -- Abbildungen Intern -> Extern
-- -------------------------------------------------

-- b_MapInt2Ext: block
-- begin
-- end block b_MapInt2Ext;



i_BRAM: entity work.BRAM
port map (
    Reset       => reset,
    Clock       => clock,
    ClockEn     => '1',
    Address     => std_logic_vector(lsc_MemAddress(12 downto 0)),
    WE          => ls_BRAM_WriteEnable,
    Data        => ls_WriteData,
    Q           => li_BRAM_ls_q
    );


i_DRAM: entity work.DRAM
port map (
    Reset       => reset,
    Clock       =>  clock,
    ClockEn     =>  '1',
    Address     =>  std_logic_vector(lsc_MemAddress(7 downto 0)),
    WE          =>  ls_DRAM_WriteEnable,
    Data        =>  ls_WriteData,
    Q           =>  li_DRAM_ls_q
    );



-- memory address controller
p_LIFO: process(reset, clock)
begin
    if (reset = '1') then 
        ls_WriteReadCnt     <= 0;       
        ls_Address          <= (others => '0');
        ls_WriteData        <= (others => '0');
        ls_BRAM_WriteEnable <= '0';
        ls_DRAM_WriteEnable <= '0';
        output              <= (others => '0');
    elsif rising_edge(clock) then
        ls_BRAM_WriteEnable  <= '0';
        ls_DRAM_WriteEnable  <= '0';
        
        if (write = '1') then                                               --  pre write increment stackpointer
            ls_Address   <= lsc_nextWriteAddress;
            ls_WriteData <= input;
            if (lsc_nextWriteAddress < cBRAM_Size) then 
                ls_BRAM_WriteEnable  <= '1';
            else
                ls_DRAM_WriteEnable  <= '1';
            end if;    
            
            if (ls_WriteReadCnt < cAddresses) then
                ls_WriteReadCnt <= ls_WriteReadCnt + 1;
            end if;
            
        elsif (read = '1') then
            if (ls_Address < cBRAM_Size) then 
                output <= li_BRAM_ls_q;
            else
                output <= li_DRAM_ls_q;
            end if;
            ls_Address <= lsc_nextReadAddress;                          -- post read increment stackpointer

            if (ls_WriteReadCnt > 0) then
                ls_WriteReadCnt <= ls_WriteReadCnt - 1;
            else 
                output <= (others => '0');                              -- Ausgabe ausblenden, wenn max. Speicherbereich gelesen wurde
            end if;
        end if;

    end if;
end process p_LIFO;

--**************************************************************************************************
END rtl;
--**************************************************************************************************