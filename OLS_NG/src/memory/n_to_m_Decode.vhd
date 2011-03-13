----------------------------------------------------------------------------------
-- n_to_m_decode.vhd
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
-- decode a n-with vector to a m-with vector
--
----------------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity n_to_m_Decode is
generic (
    n                   : integer := 1;
    m                   : integer := 1
    );
port (
    RESET_i             : in  std_logic;                                    -- Reset (high active)
    CLK_i               : in  std_logic;                                    -- Systemtakt

    Data_i              : in  std_logic_vector(n-1 downto 0);
    DataValid_i         : in  std_logic;

    Data_o              : out std_logic_vector(m-1 downto 0);
    DataValid_o         : out std_logic;

    ShiftReg_o          : out std_logic_vector(n+m-2 downto 0);
    ShiftCnt_o          : out integer
    );
end entity n_to_m_Decode;


--**************************************************************************************************
ARCHITECTURE arch_n_to_m_Decode OF n_to_m_Decode IS
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
--                                           Konstanten                                             --
----------------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------------
--                                            Signale                                             --
----------------------------------------------------------------------------------------------------

    signal ls_ShiftRegister : std_logic_vector(n+m-1 -1 downto 0);                                      -- max. benötigter Platz ein Bit weniger als Ausgabebreite
    signal ls_ShiftCnt      : integer range 0 to f_max(n, m) - 1;

    signal ls_Data          : std_logic_vector(f_max(n, m)-1 downto 0);
    signal ls_DataValid     : std_logic;


--**************************************************************************************************
BEGIN
--**************************************************************************************************

b_MapInt2Ext: block
begin
    ShiftReg_o <= ls_ShiftRegister;
    ShiftCnt_o <= ls_ShiftCnt;
end block b_MapInt2Ext;

----------------------------------------------------------------------------------------------------
--  n < m
----------------------------------------------------------------------------------------------------

n_less_m: if (n < m) generate

    p_shift: process(RESET_i, CLK_i)
        variable lv_ShiftRegister : std_logic_vector(n+m-1 -1 downto 0);                                -- max. benötigter Platz ein Bit weniger als Ausgabebreite
        variable lv_Remain        : integer range 0 to n-1;
    begin
        if (RESET_i = '1') then
            ls_ShiftRegister    <= (others => '0');
            ls_ShiftCnt         <= 0;
            ls_Data             <= (others => '0');
            ls_DataValid        <= '0';
            Data_o              <= (others => '0');
            DataValid_o         <= '0';
        elsif rising_edge(CLK_i) then
            DataValid_o <= '0';

            if (DataValid_i = '1') then

                if (ls_DataValid = '1') then
                    Data_o       <= ls_Data;                    -- Daten ausgeben
                    DataValid_o  <= '1';
                    ls_DataValid <= '0';
                end if;

                lv_ShiftRegister := ls_ShiftRegister(m-2 downto 0) & Data_i;                            -- die Daten rechtsbündig ins Schiftregister schieben

                if (ls_ShiftCnt < m-n) then
                    ls_ShiftCnt <= ls_ShiftCnt + n;                                                     -- geschobene Bits aufsummieren
                else
                    lv_Remain   := ls_ShiftCnt - (m-n);                                                 -- 'übrigbleibende' Bits
                    -- Data_o      <= lv_ShiftRegister(m + lv_Remain -1 downto lv_Remain);                    -- Daten ausgeben
                    -- DataValid_o <= '1';
                    ls_Data      <= lv_ShiftRegister(m + lv_Remain -1 downto lv_Remain);                    -- Daten ausgeben
                    ls_DataValid <= '1';
                    ls_ShiftCnt <= lv_Remain;                                                           -- Bitzähler mit der Anzahl der übrigebliebenen Bits setzten
                end if;

                ls_ShiftRegister <= lv_ShiftRegister;                                                   -- die Daten ins Schiftregister schieben
            end if;

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
            -- Data_o      <= (others => '0');
            -- DataValid_o <= '0';
        -- elsif rising_edge(CLK_i) then
            Data_o      <= Data_i;
            DataValid_o <= DataValid_i;
        -- end if;
    -- end process p_shift;

end generate;


----------------------------------------------------------------------------------------------------
--  n > m
----------------------------------------------------------------------------------------------------

n_greater_m: if (n > m) generate

    p_shift: process(RESET_i, CLK_i)
        variable lv_ShiftRegister : std_logic_vector(n+m-1 -1 downto 0) := (others => '0');         -- max. benötigter Platz ein Bit weniger als Ausgabebreite
    begin
        if (RESET_i = '1') then
            ls_ShiftRegister    <= (others => '0');
            ls_ShiftCnt         <= 0;
            ls_Data             <= (others => '0');
            Data_o              <= (others => '0');
            DataValid_o         <= '0';
        elsif rising_edge(CLK_i) then
            DataValid_o <= '0';

            -- -- ===  sofort die Daten pushen  === --
            -- if (ls_ShiftCnt < m) then
                -- if (DataValid_i = '1') then

                    -- lv_ShiftRegister(n+m-1 -1 downto m) := ls_ShiftRegister(n-2 downto 0);
                    -- lv_ShiftRegister(n+m-1 -1 - ls_ShiftCnt downto m-1 - ls_ShiftCnt) := Data_i;    -- die Daten ins Schiftregister einfügen

                    -- Data_o           <= lv_ShiftRegister(n+m-1 -1 downto n-1);                      -- Daten ausgeben
                    -- DataValid_o      <= '1';

                    -- ls_ShiftRegister <= lv_ShiftRegister;
                    -- ls_ShiftCnt      <= ls_ShiftCnt + (n-m);                                        -- geschobene Bits herunterzählen (n-Bits hinzugefügt und m-Bits ausgegeben)
                -- end if;
            -- else
                -- Data_o      <= ls_ShiftRegister(n+m-1 -1 -m downto n-1 -m);                    -- Daten ausgeben (vor dem Shiften)
                -- DataValid_o <= '1';

                -- ls_ShiftRegister(n+m-1 -1 downto m) <= ls_ShiftRegister(n-2 downto 0);
                -- ls_ShiftCnt <= ls_ShiftCnt - m;                                                     -- geschobene Bits herunterzählen
            -- end if;

            -- ===  die Daten erst ein Datum später pushen, damit auf dem Stack nicht die gleichen Daten stehen wie im Schiftregister  === --
            if (DataValid_i = '1') then
                ls_Data     <= Data_i;

                Data_o      <= ls_ShiftRegister(n+m-1 -1 downto n-1);                    -- Daten ausgeben (vor dem Shiften)
                DataValid_o <= '1';

                ls_ShiftRegister(n+m-1 -1 downto m) <= ls_ShiftRegister(n-2 downto 0);
                ls_ShiftCnt <= ls_ShiftCnt + (n-m);                                        -- geschobene Bits herunterzählen (n-Bits hinzugefügt und m-Bits ausgegeben)

            elsif (ls_ShiftCnt >= m) then
                Data_o      <= ls_ShiftRegister(n+m-1 -1 downto n-1);                    -- Daten ausgeben (vor dem Shiften)
                DataValid_o <= '1';

                ls_ShiftRegister(n+m-1 -1 downto m) <= ls_ShiftRegister(n-2 downto 0);
                ls_ShiftCnt <= ls_ShiftCnt - m;                                        -- geschobene Bits herunterzählen (n-Bits hinzugefügt und m-Bits ausgegeben)
            else
                ls_ShiftRegister(n+m-1 -1 - ls_ShiftCnt downto m-1 - ls_ShiftCnt) <= ls_Data;    -- die Daten ins Schiftregister einfügen
            end if;

        end if;
    end process p_shift;

end generate;


--**************************************************************************************************
END arch_n_to_m_Decode;
--**************************************************************************************************