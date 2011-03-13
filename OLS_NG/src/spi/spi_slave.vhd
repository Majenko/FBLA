----------------------------------------------------------------------------------
-- spi_slave.vhd
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
-- spi_slave
--
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;



entity spi_slave is
--	generic (
--		FREQ : integer;
--		SCALE : integer;
--		RATE : integer
--	);
	port (
		clock : in  std_logic;
		reset : in  std_logic;
		miso : out std_logic;
		data : in  std_logic_vector(31 downto 0);
		send : in  std_logic;          
		mosi : in  std_logic;
		--ss : out std_logic;
		sclk : in  std_logic;
		cmd : out std_logic_vector(39 downto 0);
		execute : out std_logic;
		busy : out std_logic;
		cs : in std_logic
	);
end spi_slave;

--**************************************************************************************************
ARCHITECTURE rtl OF spi_slave IS


	
	component spi_receiver
	port (
		rx : in  std_logic;
		clock : in  std_logic;
		sclk : in  std_logic;
	   reset : in  std_logic;
		op : out std_logic_vector(7 downto 0);
		data : out std_logic_vector(31 downto 0);
	   execute : out std_logic;
		transmitting : in  std_logic;
		cs : in std_logic
	   );
	end component;

	component spi_transmitter
	port (
		data : in  std_logic_vector(31 downto 0);
		disabledGroups : in  std_logic_vector(3 downto 0);
		write : in  std_logic;
		id : in  std_logic;
		xon : in  std_logic;
		xoff : in  std_logic;
		clock : in  std_logic;
		sclk : in  std_logic;
		reset : in  std_logic;
		tx : out std_logic;
		cs : in  std_logic;
		busy : out std_logic
		);
	end component;

signal executeReg, executePrev, id, xon, xoff, wrFlags, busyReg : std_logic;
signal disabledGroupsReg : std_logic_vector(3 downto 0);
signal opcode : std_logic_vector(7 downto 0);
signal opdata : std_logic_vector(31 downto 0);

--**************************************************************************************************
BEGIN
--**************************************************************************************************
	cmd <= opdata & opcode;
	execute <= executeReg;
	busy <= busyReg;
	
	-- process special uart commands that do not belong in core decoder
	process(clock)
	begin
		if rising_edge(clock) then
			id <= '0'; xon <= '0'; xoff <= '0'; wrFlags <= '0';
			executePrev <= executeReg;
			if executePrev = '0' and executeReg = '1' then
				case opcode is
					when x"02" => id <= '1';
					when x"11" => xon <= '1';
					when x"13" => xoff <= '1';
					when x"82" => wrFlags <= '1';
					when others =>
				end case;
			end if;
		end if;
	end process;

	process(clock)
	begin
		if rising_edge(clock) then
			if wrFlags = '1' then
				disabledGroupsReg <= opdata(5 downto 2);
			end if;
		end if;
	end process;

	
	i_spi_receiver: spi_receiver
	port map (
		rx => mosi,
		clock => clock,
		sclk => sclk,
		reset => reset,
		op => opcode,
		data => opdata,
		execute => executeReg,
		transmitting => busyReg,
		cs => cs
	);

	i_spi_transmitter: spi_transmitter
	port map (
		data => data,
		disabledGroups => disabledGroupsReg,
		write => send,
		id => id,
		xon => xon,
		xoff => xoff,
		clock => clock,
		sclk => sclk,
		reset => reset,
		tx => miso,
		cs => cs,
		busy => busyReg
	);

end rtl;

