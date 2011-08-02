----------------------------------------------------------------------------------
-- external_SRAM.vhd
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
-- Simple SRAM interface.
--
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity external_SRAM is
generic (
    gAddress_Width  : integer := 17;                                                                -- default: 128KiB static RAM
    gData_Width     : integer :=  8
    );
port (
    Reset_i         : in  std_logic;
    CLK_i           : in  std_logic;

    Write_i         : in  std_logic;
    Data_i          : in  std_logic_vector(gData_Width-1 downto 0);
    Read_i          : in  std_logic;
    Data_o          : out std_logic_vector(gData_Width-1 downto 0);

    -- ===  SRAM  === --
    SRAM_CE_no      : out   std_logic;                                                              -- chip enable (low active)
    SRAM_OE_no      : out   std_logic;                                                              -- data_o enable (low active)
    SRAM_WE_no      : out   std_logic;                                                              -- write enable (low active)
    SRAM_Addr_o     : out   std_logic_vector(gAddress_Width-1 downto 0);                            --
    SRAM_Data_io    : inout std_logic_vector(gData_Width-1 downto 0)                                -- bi-directional data ports
    );
end entity external_SRAM;


--**************************************************************************************************
ARCHITECTURE rtl OF external_SRAM IS
--**************************************************************************************************


constant cAddresses         : integer := 2**gAddress_Width;
constant cMaxAddress        : integer := cAddresses - 1;


signal ls_Address           : unsigned (gAddress_Width-1 downto 0);
signal ls_WriteReadCnt      : integer range 0 to cMaxAddress;            -- Zähler für Ausgabe ausblenden, wenn max. Speicherbereich gelesen wurde
signal ls_WriteData         : std_logic_vector(gData_Width-1 downto 0);

type t_sm_SRAM is (
    s_SaveReset,
    s_Idle,
    s_WriteCycle,
    s_WriteCycle_2,
    s_ReadCycle
    );

signal sm_SRAM : t_sm_SRAM;

signal ls_wait_cnt          : integer range 0 to 7;


--**************************************************************************************************
BEGIN
--**************************************************************************************************

-- -------------------------------------------------
-- -- Abbildungen Extern -> Intern
-- -------------------------------------------------

-- b_MapExt2Int: block
-- begin
-- end block b_MapExt2Int;

-- -------------------------------------------------
-- -- Abbildungen Intern <-> Intern
-- -------------------------------------------------

-- b_MapInt2Int: block
-- begin
-- end block b_MapInt2Int;

-- -------------------------------------------------
-- -- Abbildungen Intern -> Extern
-- -------------------------------------------------

-- b_MapInt2Ext: block
-- begin
-- end block b_MapInt2Ext;


-- ===  memory address controller  === --
p_LIFO: process(Reset_i, CLK_i)
begin
    if (Reset_i = '1') then
        sm_SRAM             <= s_SaveReset;
        Data_o              <= (others => '0');
        SRAM_CE_no          <= not('0');                               -- chip enable
        SRAM_OE_no          <= not('0');                               -- output enable
        SRAM_WE_no          <= not('0');                               -- write enable
        SRAM_Addr_o         <= (others => '0');
        SRAM_Data_io        <= (others => 'Z');                        -- data output tristate
        ls_Address          <= (others => '0');
        ls_WriteData        <= (others => '0');
        ls_WriteReadCnt     <= 0;
        ls_wait_cnt         <= 0;
        
    elsif rising_edge(CLK_i) then
        case sm_SRAM is

            ----------------------------------------------------------------------------------------
            when s_SaveReset =>                                                                     -- sicheres Herauskommen aus dem asynchronen Reset
                sm_SRAM <= s_Idle;

            ----------------------------------------------------------------------------------------
            when s_Idle =>
                SRAM_CE_no   <= not('0');                                                           -- chip enable
                SRAM_OE_no   <= not('0');                                                           -- output enable
                SRAM_WE_no   <= not('0');                                                           -- write enable
                SRAM_Addr_o  <= std_logic_vector(ls_Address);
                SRAM_Data_io <= (others => 'Z');                                                    -- data output tristate

                if (Write_i = '1') then
                    if (ls_WriteReadCnt < cMaxAddress) then
                        ls_WriteReadCnt <= ls_WriteReadCnt + 1;
                    end if;
                    ls_Address   <= ls_Address + 1;
                    SRAM_Addr_o  <= std_logic_vector(ls_Address + 1);
                    SRAM_CE_no   <= not('1');                                                       -- chip enable
                    -- SRAM_WE_no   <= not('1');                                                       -- write enable (ein Takt später setzen)
                    ls_WriteData <= Data_i ;                                                        -- latch write data
                    ls_wait_cnt  <= 2;
                    sm_SRAM      <= s_WriteCycle;
                end if;

                if (Read_i = '1') then
                    if (ls_WriteReadCnt > 0) then
                        ls_WriteReadCnt <= ls_WriteReadCnt - 1;
                        SRAM_CE_no   <= not('1');                                                   -- chip enable
                        SRAM_OE_no   <= not('1');                                                   -- output enable
                        ls_wait_cnt  <= 6;
                        sm_SRAM <= s_ReadCycle;
                    else
                        Data_o <= (others => '0');                                                  -- Ausgabe ausblenden, wenn max. Speicherbereich gelesen wurde
                    end if;
                end if;

            ----------------------------------------------------------------------------------------
            when s_WriteCycle =>
                SRAM_WE_no   <= not('1');                                                           -- write enable (ein Takt später setzen)
                if (ls_wait_cnt > 0) then
                    ls_wait_cnt <= ls_wait_cnt - 1;
                else
                    SRAM_Data_io <= ls_WriteData;                                                   -- output latched write data
                    ls_wait_cnt  <= 2;
                    sm_SRAM      <= s_WriteCycle_2;
                end if;

            ----------------------------------------------------------------------------------------
            when s_WriteCycle_2 =>
                if (ls_wait_cnt > 0) then
                    ls_wait_cnt <= ls_wait_cnt - 1;
                else
                    SRAM_WE_no   <= not('0');                                                       -- write enable
                    -- SRAM_Data_io <= (others => 'Z');                                                -- data output tristate [Daten noch einen Takt länger anstehen lassen]
                    sm_SRAM      <= s_Idle;
                end if;

            ----------------------------------------------------------------------------------------
            when s_ReadCycle =>
                if (ls_wait_cnt > 0) then
                    ls_wait_cnt <= ls_wait_cnt - 1;
                else
                    ls_Address  <= ls_Address - 1;
                    SRAM_CE_no   <= not('0');                                                           -- chip enable
                    SRAM_OE_no   <= not('0');                                                           -- output enable
                    Data_o      <= SRAM_Data_io;                                                    -- input data
                    sm_SRAM     <= s_Idle;
                end if;
        end case;

    end if;
end process p_LIFO;

--**************************************************************************************************
END rtl;
--**************************************************************************************************