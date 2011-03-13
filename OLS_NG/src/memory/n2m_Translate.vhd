----------------------------------------------------------------------------------
-- n2m_translate.vhd
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
-- convert a n-with vector bidirectionally to a m-with vector
--
----------------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity n2m_Translate is
generic (
    n                   : integer := 1;
    m                   : integer := 1
    );
port (
    RESET_i             : in  std_logic;                                    -- Reset (high active)
    CLK_i               : in  std_logic;                                    -- Systemtakt

    -- ===  Daten schreiben  === --
    WriteDataStrobe_i   : in  std_logic;
    WriteData_i         : in  std_logic_vector(31 downto 0);

    WriteDataStrobe_o   : buffer std_logic;
    WriteData_o         : out std_logic_vector(m-1 downto 0);
    
    -- ===  Daten lesen  === --
    ReadDataStrobe_i    : in  std_logic;
    ReadData_o          : out std_logic_vector(31 downto 0);
 
    ReadDataStrobe_o    : out std_logic;
    ReadData_i          : in  std_logic_vector(m-1 downto 0)
    );
end entity n2m_Translate;


--**************************************************************************************************
ARCHITECTURE arch_n2m_Translate OF n2m_Translate IS
--**************************************************************************************************


----------------------------------------------------------------------------------------------------
--                                            Signale                                             --
----------------------------------------------------------------------------------------------------

    signal ti_Decode_ls_ShiftReg        : std_logic_vector(n + m - 2 downto 0);
    signal ti_Decode_ls_ShiftCnt        : integer;

    signal ls_InitEncode                : std_logic;
    
    
--**************************************************************************************************
BEGIN
--**************************************************************************************************


--==================================================================================================
--
--==================================================================================================

i_n_to_m_Decode: entity work.n_to_m_Decode
generic map (
    n                   => n,
    m                   => m
    )
port map (
    RESET_i             => RESET_i,                                         -- Reset (high active)
    CLK_i               => CLK_i,                                           -- Systemtakt

    Data_i              => WriteData_i(n-1 downto 0),
    DataValid_i         => WriteDataStrobe_i,

    Data_o              => WriteData_o(m-1 downto 0),
    DataValid_o         => WriteDataStrobe_o,

    ShiftReg_o          => ti_Decode_ls_ShiftReg,
    ShiftCnt_o          => ti_Decode_ls_ShiftCnt
    );

    
i_n_to_m_Encode: entity work.n_to_m_Encode
generic map (
    n                   => m,
    m                   => n
    )
port map (
    RESET_i             => RESET_i,                                         -- Reset (high active)
    CLK_i               => CLK_i,                                           -- Systemtakt

    ShifInit_i          => ls_InitEncode,                                   -- Initialisierung von ShiftReg und ShiftCnt
    ShiftReg_i          => ti_Decode_ls_ShiftReg,
    ShiftCnt_i          => ti_Decode_ls_ShiftCnt,

    DataRequest_i       => ReadDataStrobe_i,
    Data_o              => ReadData_o(n-1 downto 0),                        -- ('n' von der Entity, lokal 'm')

    DataRequest_o       => ReadDataStrobe_o,
    Data_i              => ReadData_i(m-1 downto 0)                         -- ('m' von der Entity, lokal 'n')
    );


ReadData_o(31 downto n) <= (others => '0');
   
--==================================================================================================
--
--==================================================================================================

p_ReadWrite: process(RESET_i, CLK_i)
begin
    if (RESET_i = '1') then
        ls_InitEncode <= '0';
    elsif rising_edge(CLK_i) then
        if (n <= m) then 
            ls_InitEncode <= WriteDataStrobe_i;
        else 
            ls_InitEncode <= WriteDataStrobe_o;
        end if;
    end if;
end process p_ReadWrite;


--**************************************************************************************************
END arch_n2m_Translate;
--**************************************************************************************************