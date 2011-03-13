----------------------------------------------------------------------------------
-- receiver.vhd
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
-- Receives commands from the serial port. The first byte is the commands
-- opcode, the following (optional) four byte are the command data.
-- Commands that do not have the highest bit in their opcode set are
-- considered short commands without data (1 byte long). All other commands are
-- long commands which are 5 bytes long.
--
-- After a full command has been received it will be kept available for 10 cycles
-- on the op and data outputs. A valid command can be detected by checking if the
-- execute output is set. After 10 cycles the registers will be cleared
-- automatically and the receiver waits for new data from the serial port.
--
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

use ieee.numeric_std.all;

entity spi_receiver is
   port ( rx : in  std_logic;
           clock : in  std_logic;
			  sclk : in  std_logic;
			  reset : in  std_logic;
           op : out std_logic_vector(7 downto 0);
           data : out std_logic_vector(31 downto 0);
           execute : out std_logic;
			  transmitting : in  std_logic;
			  cs : in std_logic
	);
end spi_receiver;

--**************************************************************************************************
ARCHITECTURE rtl OF spi_receiver IS

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

	type UART_STATES is (INIT, WAITCS, READBYTE, ANALYZE, READY);


	signal counter, ncounter : integer range 0 to 15;	-- clock prescaling counter
	signal bitcount : integer range 0 to 8; 		-- count rxed bits of current byte
	signal bytecount, nbytecount : integer range 0 to 5;		-- count rxed bytes of current command
	signal state, nstate : UART_STATES;								-- receiver state
	signal opcode, nopcode : std_logic_vector(7 downto 0);	-- opcode byte
	signal dataBuf, ndataBuf : std_logic_vector(31 downto 0); -- data dword
	signal sclkPrev, rsync_sclk, rsync_cs, csPrev : std_logic;
	signal spiByte : std_logic_vector(7 downto 0);	


--**************************************************************************************************
BEGIN
--**************************************************************************************************
	op <= opcode;
	data <= dataBuf;

	i_rsnc_bit_sclk: rsnc_bit port map (
		clk => clock,
		di => sclk,
		do => rsync_sclk
	);
	
	i_rsnc_bit_cs: rsnc_bit port map (
		clk => clock,
		di => cs,
		do => rsync_cs
	);	


	process(clock, reset, rsync_sclk, rsync_cs)
	begin
		if reset = '1' then
			bitcount <= 0;
		elsif rising_edge(clock) then
			sclkPrev <= rsync_sclk;
			csPrev <= rsync_cs;
			if ((csPrev = '1' and rsync_cs = '0') or state /= READBYTE) then
				bitcount <= 0;
			end if;
				if sclkPrev = '0' and rsync_sclk = '1' and cs = '0' then	--detect rising edge of sclk
					spiByte <= spiByte(6 downto 0) & rx;
					bitcount <= bitcount + 1;
				end if;
		end if;
	end process;
	
	process(clock, reset)
	begin
		if reset = '1' then
			state <= INIT;
		elsif rising_edge(clock) then
			counter <= ncounter;
			bytecount <= nbytecount;
			dataBuf <= ndataBuf;
			opcode <= nopcode;
			state <= nstate;
		end if;		
	end process;

	process(rsync_sclk, state, counter, bitcount, bytecount, dataBuf, opcode, rx, cs)
	begin
		case state is

			-- reset uart
			when INIT =>
				ncounter <= 0;
				nbytecount <= 0;
				nopcode <= (others => '0');
				ndataBuf <= (others => '0');
				nstate <= WAITCS;

			-- wait for cs
			when WAITCS =>
				ncounter <= 0; 
				nbytecount <= bytecount;
				nopcode <= opcode;
				ndataBuf <= dataBuf;
				if cs = '0' then
					nstate <= READBYTE;
				else
					nstate <= state;
				end if;
				
			-- receive byte
			when READBYTE =>
				ncounter <= counter;
				nbytecount <= bytecount;
				nopcode <= opcode;
				ndataBuf <= dataBuf;
				nstate <= state;
				if (bitcount = 8 and rsync_sclk = '0') then
					nbytecount <= bytecount + 1;
					nstate <= ANALYZE;			
					if bytecount = 0 then
						nopcode <= spiByte;
						ndataBuf <= dataBuf;
					else
						nopcode <= opcode;
						ndataBuf <= spiByte & dataBuf(31 downto 8);
					end if;
				end if;
				
			-- check if long or short command has been fully received
			when ANALYZE =>
				ncounter <= 0; 
				nbytecount <= bytecount;
				nopcode <= opcode;
				ndataBuf <= dataBuf;
				-- long command when 5 bytes have been received
				if bytecount = 5 then
					nstate <= READY;
				-- short command when set flag not set
				elsif opcode(7) = '0' then
					nstate <= READY;
				-- otherwise continue receiving
				else
					nstate <= WAITCS;
				end if;
				
			-- done, give 10 cycles for processing
			when READY =>
				ncounter <= counter + 1;
				nbytecount <= 0;
				nopcode <= opcode;
				ndataBuf <= dataBuf;
				if counter = 10 then
					nstate <= INIT;
				else
					nstate <= state;
				end if;
					
		end case;

	end process;

	-- set execute flag properly
	process(state)
	begin
		if state = READY then
			execute <= '1';
		else
			execute <= '0';
		end if;
	end process;

end rtl;
