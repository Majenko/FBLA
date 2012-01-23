----------------------------------------------------------------------------------
-- eia232.vhd
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
-- EIA232 aka RS232 interface.
--
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity eia232 is
generic (
    FREQ : integer;
    SCALE : integer;
    RATE : integer
    );
port (
    clock       : in  std_logic;
    reset       : in  std_logic;
    speed       : in  std_logic_vector(1 downto 0);
    XOnOff_i    : in  std_logic;
    rx          : in  std_logic;
    tx          : out std_logic;
    cmd         : out std_logic_vector(39 downto 0);
    execute     : out std_logic;
    data        : in  std_logic_vector(31 downto 0);
    send        : in  std_logic;
    busy        : out std_logic;
    LED_Xon_o   : out std_logic;
    LED_Xoff_o  : out std_logic
    );
end eia232;

--**************************************************************************************************
ARCHITECTURE rtl OF eia232 IS
--**************************************************************************************************

component prescaler
generic (
    SCALE   : integer
    );
port (
    clock   : in  std_logic;
    reset   : in  std_logic;
    div     : in  std_logic_vector(1 downto 0);
    scaled  : out std_logic
    );
end component;

component receiver
generic (
    FREQ        : integer;
    RATE        : integer
    );
port (
    rx          : in  std_logic;
    clock       : in  std_logic;
    trxClock    : in  std_logic;
    reset       : in  std_logic;
    op          : out std_logic_vector(7 downto 0);
    data        : out std_logic_vector(31 downto 0);
    execute     : out std_logic
    );
end component;

component transmitter
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
end component;


constant TRXFREQ : integer := FREQ / SCALE;  -- reduced rx & tx clock for receiver and transmitter

signal trxClock             : std_logic;
signal executeReg           : std_logic;
signal executePrev          : std_logic;
signal id                   : std_logic;
signal meta                 : std_logic;
signal xon                  : std_logic;
signal xoff                 : std_logic;
signal wrFlags              : std_logic;
signal disabledGroupsReg    : std_logic_vector(3 downto 0);
signal opcode               : std_logic_vector(7 downto 0);
signal opdata               : std_logic_vector(31 downto 0);


--**************************************************************************************************
BEGIN
--**************************************************************************************************

cmd     <= opdata & opcode;
execute <= executeReg;

-- ===  process special uart commands that do not belong in core decoder  === --
process(clock)
begin
    if rising_edge(clock) then
        id          <= '0';
        meta        <= '0';
        xon         <= '0';
        xoff        <= '0';
        wrFlags     <= '0';
        executePrev <= executeReg;
        if ((executePrev = '0') and (executeReg = '1')) then
            case opcode is
                when x"02" => id <= '1';
                when x"04" => null;  --  meta <= '1';
                when x"11" => xon     <= XOnOff_i;
                when x"13" => xoff    <= XOnOff_i;
                when x"82" => wrFlags <= '1';
                when others =>
            end case;
        end if;
    end if;
end process;

process(reset, clock)
begin
    if (reset = '1') then
        disabledGroupsReg <= (others => '0');
    elsif rising_edge(clock) then
        if wrFlags = '1' then
            disabledGroupsReg <= opdata(5 downto 2);
        end if;
    end if;
end process;


i_prescaler: prescaler
generic map (
    SCALE   => SCALE
    )
port map (
    clock   => clock,
    reset   => reset,
    div     => speed,
    scaled  => trxClock
    );

i_receiver: receiver
generic map (
    FREQ        => TRXFREQ,
    RATE        => RATE
    )
port map (
    rx          => rx,
    clock       => clock,
    trxClock    => trxClock,
    reset       => reset,
    op          => opcode,
    data        => opdata,
    execute     => executeReg
    );

i_transmitter: transmitter
generic map (
    FREQ            => TRXFREQ,
    RATE            => RATE
    )
port map (
    data            => data,
    disabledGroups  => disabledGroupsReg,
    write           => send,
    id              => id,
    meta            => meta,
    xon             => xon,
    xoff            => xoff,
    clock           => clock,
    trxClock        => trxClock,
    reset           => reset,
    tx              => tx,
    busy            => busy
);


p_LED_Xon_Xoff: process(reset, clock)
begin
    if (reset = '1') then
        LED_Xon_o   <= '0';
        LED_Xoff_o  <= '0';
    elsif rising_edge(clock) then
        if (xon = '1') then
            LED_Xon_o   <= '1';
            LED_Xoff_o  <= '0';
        elsif (xoff = '1') then
            LED_Xon_o   <= '0';
            LED_Xoff_o  <= '1';
        end if;
    end if;
end process p_LED_Xon_Xoff;


assert (1 = 2)                                                                                      -- ohne 'assert' wird bei der Synthese (Synplify) kein Report ausgeben
    report "EIA232: " &
        "FREQ  = "  & integer'image(FREQ)  & ", " &
        "RATE  = "  & integer'image(RATE)  & ", " &
        "SCALE = "  & integer'image(SCALE)
    severity NOTE;

--**************************************************************************************************
END rtl;
--**************************************************************************************************