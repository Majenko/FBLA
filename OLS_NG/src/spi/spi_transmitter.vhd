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
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

use ieee.numeric_std.all;

entity spi_transmitter is
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
		busy: out std_logic
	);
end spi_transmitter;

--**************************************************************************************************
ARCHITECTURE rtl OF spi_transmitter IS


	component rsnc_bit 
				generic(
					add_stgs_num : integer := 0;
					inv_f_stgs   : integer := 0
							); 
				port (	                   
					clk : in  std_logic;
					di  : in  std_logic;
					do  : out std_logic
					);
	end component;

	type TX_STATES is (IDLE, SEND, POLL);

	signal dataBuffer : std_logic_vector(31 downto 0);
	signal disabledBuffer : std_logic_vector(3 downto 0);
	signal txBuffer : std_logic_vector(7 downto 0) := "00000000";
	signal byte : std_logic_vector(7 downto 0);
	signal bits : integer range 0 to 7;
	signal bytes : integer range 0 to 4;
	signal state : TX_STATES;
	signal paused, writeByte, byteDone, disabled, rsync_sclk : std_logic;
	signal sclkPrev, busyReg : std_logic;

--**************************************************************************************************
BEGIN
--**************************************************************************************************
--	pause <= paused;
	tx <= txBuffer(7);
	busy <= busyReg;

	i_rsnc_bit: rsnc_bit port map (
		clk => clock,
		di => sclk,
		do => rsync_sclk
	);

	-- sends one byte
	process(clock, rsync_sclk)
	begin
		if rising_edge(clock) then
			sclkPrev <= rsync_sclk;		
			if writeByte = '1' then
				bits <= 0;
				byteDone <= disabled;
				txBuffer <= byte;
			end if;

			if cs = '1' then
				bits <= 0;
			end if;

			if (sclkPrev = '1' and rsync_sclk = '0') then	--detect falling edge of sclk
				if byteDone = '0' then
					txBuffer <=  txBuffer(6 downto 0) & '1';
					if bits = 7 then
						byteDone <= '1';
					else
						bits <= bits + 1;
					end if;
				end if;
			end if;
		end if;
	end process;

	-- control mechanism for sending a 32 bit word
	process(clock, reset)
	begin

		if reset = '1' then
			writeByte <= '0';
			state <= IDLE;
			dataBuffer <= (others => '0');
			disabledBuffer <= (others => '0');

		elsif rising_edge(clock) then
			if (state /= IDLE) or (write = '1') then
				busyReg <= '1';
			else
				busyReg <= '0';
			end if;

			case state is
					
				-- when write is '1', data will be available with next cycle
				when IDLE =>
					if write = '1' then
						dataBuffer <= data;
						disabledBuffer <= disabledGroups;
						state <= SEND;
						bytes <= 0;
					elsif id = '1' then
						dataBuffer <= x"534c4131";
						disabledBuffer <= "0000";
						state <= SEND;
						bytes <= 0;
					end if;
				
				when SEND =>
					if bytes = 4 then
						state <= IDLE;
					else
						bytes <= bytes + 1;
						case bytes is
							when 0 =>
								byte <= dataBuffer(7 downto 0);
								disabled <= disabledBuffer(0);
							when 1 =>
								byte <= dataBuffer(15 downto 8);
								disabled <= disabledBuffer(1);
							when 2 =>
								byte <= dataBuffer(23 downto 16);
								disabled <= disabledBuffer(2);
							when others =>
								byte <= dataBuffer(31 downto 24);
								disabled <= disabledBuffer(3);
						end case;
						writeByte <= '1';
						state <= POLL;
					end if;

				when POLL =>
					if rsync_sclk = '0' then
					writeByte <= '0';
					end if;				
					
					if byteDone = '1' and writeByte = '0' then
						state <= SEND;
					end if;
				
			end case;
		end if;
	end process;
	
	-- set paused mode according to xon/xoff commands
--	process(clock, reset)
--	begin
--		if reset = '1' then
--			paused <= '0';
--		elsif rising_edge(clock) then
--			if xon = '1' then
--				paused <= '0';
--			elsif xoff = '1' then
--				paused <= '1';
--			end if;
--		end if;
--	end process;
	
end rtl;
