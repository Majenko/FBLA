----------------------------------------------------------------------------------
-- Lattice.vhd
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
-- wrapper to replace Xilinx primitives by Lattice equivalent
--
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

entity BUFGMUX is
port (
    O   : out std_logic;        -- Clock MUX output
    I0  : in  std_logic;        -- Clock0 input
    I1  : in  std_logic;        -- Clock1 input
    S   : in  std_logic         -- Clock select input
    );
end;

--**************************************************************************************************
ARCHITECTURE arch_BUFGMUX OF BUFGMUX IS
--**************************************************************************************************

component DCS
    -- synthesis translate_off
    generic (
        DCSMODE : string := "POS"
        );
    -- synthesis translate_on
    port (
        CLK0    :   in  std_logic;
        CLK1    :   in  std_logic;
        SEL     :   in  std_logic;
        DCSOUT  :   out std_logic
        );
end component;


--**************************************************************************************************
BEGIN
--**************************************************************************************************


i_DCS: DCS
    -- synthesis translate_off
    generic map (
        DCSMODE => "POS"
        )
    -- synthesis translate_on
    port map (
        SEL     => S,
        CLK0    => I0,
        CLK1    => I1,
        DCSOUT  => O
    );

--**************************************************************************************************
end arch_BUFGMUX;
--**************************************************************************************************


-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity clockman is
    port (
        clkin   : in  std_logic;        -- clock input
        clk0    : out std_logic         -- double clock rate output
    );
end clockman;

--**************************************************************************************************
ARCHITECTURE arch_clockman OF clockman IS
--**************************************************************************************************

--**************************************************************************************************
BEGIN
--**************************************************************************************************

i_PLL: entity work.PLL
port map (
    CLK     => clkin, 
    CLKOP   => clk0,
    LOCK    => open
    );


--**************************************************************************************************
end arch_clockman;
--**************************************************************************************************
