----------------------------------------------------------------------------------
-- n_to_m_encode.vhd
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
-- encode a m-with vector to a n-with vector
--
----------------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


--synthesis translate_off
--synthesis translate_on


entity n_to_m_Encode is
generic (
    n                   : integer := 1;
    m                   : integer := 1
    );
port (
    RESET_i             : in  std_logic;                                    -- Reset (high active)
    CLK_i               : in  std_logic;                                    -- Systemtakt

    ShifInit_i          : in  std_logic;                                    -- Initialisierung von ShiftReg und ShiftCnt
    ShiftReg_i          : in  std_logic_vector;
    ShiftCnt_i          : in  integer;

    DataRequest_i       : in  std_logic;
    Data_o              : out std_logic_vector(m-1 downto 0);

    DataRequest_o       : out std_logic;
    Data_i              : in  std_logic_vector(n-1 downto 0)
    );
end entity n_to_m_Encode;


--**************************************************************************************************
ARCHITECTURE arch_n_to_m_Encode OF n_to_m_Encode IS
--**************************************************************************************************

----------------------------------------------------------------------------------------------------
--                                           Funktionen                                             --
----------------------------------------------------------------------------------------------------

    function f_max(Val1, Val2 : integer) return integer is
    begin
        if Val1 >= Val2 then
            return Val1;
        else
            return Val2;
        end if;
    end function f_max;

----------------------------------------------------------------------------------------------------
--                                            Signale                                             --
----------------------------------------------------------------------------------------------------

    signal ls_ShiftRegister     : std_logic_vector(n+m-1 -1 downto 0);                                      -- max. benötigter Platz ein Bit weniger als Ausgabebreite
    signal ls_ShiftCnt          : integer range 0 to f_max(n, m) - 1;
    signal LastShiftCnt_in      : integer range 0 to f_max(n, m) - 1;

    signal ls_ShiftInit_Flag    : std_logic;
    signal ls_fetch             : std_logic;
    signal ls_fetch_p0          : std_logic;

--**************************************************************************************************
BEGIN
--**************************************************************************************************


----------------------------------------------------------------------------------------------------
--  n < m
----------------------------------------------------------------------------------------------------

n_less_m: if (n < m) generate

    p_shift: process(RESET_i, CLK_i)
        variable lv_ShiftRegister : std_logic_vector(n+m-1 -1 downto 0);                                -- max. benötigter Platz ein Bit weniger als Ausgabebreite
        variable lv_ShiftCntNext  : integer range 0 to m-1;
        variable lv_Remain        : integer range 0 to n-1;
    begin
        if (RESET_i = '1') then
            ls_ShiftRegister    <= (others => '0');
            ls_ShiftCnt         <= 0;
            Data_o              <= (others => '0');
            DataRequest_o       <= '0';
            ls_fetch            <= '0';  
            ls_fetch_p0         <= '0';  
        elsif rising_edge(CLK_i) then
            DataRequest_o <= '0';
            ls_fetch_p0   <= ls_fetch;

            if (ls_ShiftCnt < m-n) then
                lv_ShiftCntNext := ls_ShiftCnt + n;
            else
                lv_ShiftCntNext := ls_ShiftCnt - (m-n);
            end if;            
         
            if (ls_ShiftInit_Flag = '1') then 
                if (DataRequest_i = '1') then
                    Data_o        <= ls_ShiftRegister(n+m-1 -1 - ls_ShiftCnt downto n-1 - ls_ShiftCnt);                             -- Daten ausgeben
                    ls_ShiftCnt   <= lv_ShiftCntNext;
                    DataRequest_o <= '1';
                    ls_fetch      <= '1';
                end if;
            end if;
            
            if (ls_fetch = '1') then 
                if (ls_ShiftCnt >= n) then                                              -- das gerade ausgegebene Datum auch vom LIFO poppen
                    DataRequest_o <= '1';
                    ls_ShiftCnt   <= lv_ShiftCntNext;
                else 
                    ls_fetch <= '0';  
                end if;
            end if;

            if (ls_fetch_p0 = '1') then 
                ls_ShiftRegister <= Data_i & ls_ShiftRegister(n+m-1 -1 downto n);            -- die Daten linksbündig ins Schiftregister schieben
            end if;
               
            -- ===  Initialisierung  === --
            if (ShifInit_i = '1') then         -- Idee: den Zähler auf einen gültigen Bereich testen und damit das Init-Signal auslösen                                                         -- den Schiftzustand initialisieren
                ls_ShiftInit_Flag <= '1';
                ls_ShiftRegister  <= ShiftReg_i;
                ls_ShiftCnt       <= ShiftCnt_i;
            end if;
             -- if (ShiftCnt_i < m-n) and (ShiftCnt_i /= LastShiftCnt_in) then         -- Idee: den Zähler auf einen gültigen Bereich testen und damit das Init-Signal auslösen                                                         -- den Schiftzustand initialisieren
                -- ls_ShiftInit_Flag <= '1';
                -- ls_ShiftRegister  <= ShiftReg_i;
                -- ls_ShiftCnt       <= ShiftCnt_i;
                -- LastShiftCnt_in   <= ShiftCnt_i;
            -- end if;
        end if;
    end process p_shift;

end generate;


----------------------------------------------------------------------------------------------------
--  n = m
----------------------------------------------------------------------------------------------------

n_equal_m: if (n = m) generate

   -- p_shift: process(RESET_i, CLK_i)
    -- begin
        -- if (RESET_i = '1') then
            -- Data_o        <= (others => '0');
            -- DataRequest_o <= '0';
        -- elsif rising_edge(CLK_i) then
            Data_o        <= Data_i;
            DataRequest_o <= DataRequest_i;
        -- end if;
    -- end process p_shift;

end generate;


----------------------------------------------------------------------------------------------------
--  n > m
----------------------------------------------------------------------------------------------------

n_greater_m: if (n > m) generate

    p_shift: process(RESET_i, CLK_i)
        variable lv_ShiftRegister : std_logic_vector(n+m-1 -1 downto 0);         -- max. benötigter Platz ein Bit weniger als Ausgabebreite
        variable lv_ShiftCntNext  : integer range 0 to n-1;
    begin
        if (RESET_i = '1') then
            lv_ShiftRegister  := (others => '0');
            ls_ShiftRegister  <= (others => '0');
            ls_ShiftCnt       <= 0;
            Data_o            <= (others => '0');
            DataRequest_o     <= '0';
            ls_ShiftInit_Flag <= '0';
        elsif rising_edge(CLK_i) then
            DataRequest_o <= '0';

            if (DataRequest_i = '1') then

                if (ls_ShiftCnt < m) then
                    lv_ShiftCntNext := ls_ShiftCnt + (n-m);
                else
                    lv_ShiftCntNext := ls_ShiftCnt - m;
                end if;

                if (ls_ShiftInit_Flag = '1') then
                    Data_o <= ls_ShiftRegister(m-1 downto 0);                                       -- Daten ausgeben

                    if (lv_ShiftCntNext < m) then
                        DataRequest_o <= '1';                                                       -- Dummypop für gerade gepushtes Element
                    end if;
                    
                elsif (ls_ShiftCnt < m) then
                    lv_ShiftRegister(n-2 downto 0) := ls_ShiftRegister(n+m-1 -1 downto m);
                    lv_ShiftRegister(n-1 + ls_ShiftCnt downto ls_ShiftCnt) := Data_i;               -- die Daten ins Schiftregister einfügen
                    Data_o           <= lv_ShiftRegister(m-1 downto 0);                             -- Daten ausgeben

                    ls_ShiftRegister <= lv_ShiftRegister;
                else
                    Data_o <= ls_ShiftRegister(m-1 + m downto m);                                   -- Daten ausgeben (vor dem Shiften)
                    ls_ShiftRegister(n-2 downto 0) <= ls_ShiftRegister(n+m-1 -1 downto m);

                end if;

                if (lv_ShiftCntNext < m) then
                    DataRequest_o <= '1';
                end if;

                ls_ShiftInit_Flag <= '0';
                ls_ShiftCnt       <= lv_ShiftCntNext;
            end if;


            -- ===  Initialisierung  === --
            if (ShifInit_i = '1') then                                                              -- den Schiftzustand initialisieren
                ls_ShiftInit_Flag <= '1';
                ls_ShiftRegister  <= ShiftReg_i;
                ls_ShiftCnt       <= ShiftCnt_i;
            end if;
        end if;
    end process p_shift;

end generate;


--**************************************************************************************************
END arch_n_to_m_Encode;
--**************************************************************************************************