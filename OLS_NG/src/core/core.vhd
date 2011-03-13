----------------------------------------------------------------------------------
-- core.vhd
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
-- The core contains all "platform independent" modules and provides a
-- simple interface to those components. The core makes the analyzer
-- memory type and computer interface independent.
--
-- This module also provides a better target for test benches as commands can
-- be sent to the core easily.
--
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity core is
generic (
    MEMORY_DEPTH    : integer := 6
    );
port ( 
    clock           : in  std_logic;
    extReset        : in  std_logic;
    cmd             : in  std_logic_vector(39 downto 0);
    execute         : in  std_logic;
    intReset        : out std_logic;
    input           : in  std_logic_vector(31 downto 0);
    inputClock      : in  std_logic;
    sampleReady50   : out std_logic;
    output          : out std_logic_vector(31 downto 0);
    outputSend      : out std_logic;
    outputBusy      : in  std_logic;
    memoryIn        : in  std_logic_vector(31 downto 0);
    memoryOut       : out std_logic_vector(31 downto 0);
    memoryRead      : out std_logic;
    memoryWrite     : out std_logic;
    extTriggerIn    : in  std_logic;
    extTriggerOut   : out std_logic;           
    extClockOut     : out std_logic;
    armLED          : out std_logic;
    triggerLED      : out std_logic;
    numberScheme    : out std_logic;
    testMode        : out std_logic         
    );
end core;


--**************************************************************************************************
ARCHITECTURE rtl OF core IS
--**************************************************************************************************

component decoder
port ( 
    reset       : in  std_logic;
    clock       : in  std_logic;
    opcode      : in  std_logic_vector(7 downto 0);
    execute     : in  std_logic;
    wrtrigmask  : out std_logic_vector(3 downto 0);
    wrtrigval   : out std_logic_vector(3 downto 0);
    wrtrigcfg   : out std_logic_vector(3 downto 0);
    wrspeed     : out std_logic;
    wrsize      : out std_logic;
    wrFlags     : out std_logic;
    arm         : out std_logic;
    -- testmode    : out std_logic;
    IntReset    : out std_logic
    );
end component;

component flags
port (
    data            : in  std_logic_vector(10 downto 0);
    clock           : in  std_logic;
    write           : in  std_logic;          
    demux           : out std_logic;
    filter          : out std_logic;
    external        : out std_logic;
    inverted        : out std_logic;
    rle             : out std_logic;
    numberScheme    : out std_logic;
    testMode        : out std_logic      
    );
end component;

component sync is
port (
    reset           : in  std_logic;
    clock           : in  std_logic;
    input           : in  std_logic_vector(31 downto 0);
    enableFilter    : in  std_logic;
    enableDemux     : in  std_logic;
    falling         : in  std_logic;
    output          : out std_logic_vector(31 downto 0)
    );
end component;

component sampler
port (
   reset        : in  std_logic;
   clock        : in  std_logic;
   input        : in  std_logic_vector(31 downto 0);
   exClock      : in  std_logic;
   external     : in  std_logic;
   data         : in  std_logic_vector(23 downto 0);
   wrDivider    : in  std_logic;          
   sample       : out std_logic_vector(31 downto 0);
   ready        : out std_logic;
   ready50      : out std_logic
   );
end component;

component trigger
port (
   input            : in  std_logic_vector(31 downto 0);
   inputReady       : in  std_logic;
   data             : in  std_logic_vector(31 downto 0);
   clock            : in  std_logic;
   reset            : in  std_logic;
   wrMask           : in  std_logic_vector(3 downto 0);
   wrValue          : in  std_logic_vector(3 downto 0);
   wrConfig         : in  std_logic_vector(3 downto 0);
   arm              : in  std_logic;
   demuxed          : in  std_logic;
   run              : out std_logic;
   ExtTriggerIn     : in  std_logic      
   );
end component;

component controller
port (
    clock           : in  std_logic;
    reset           : in  std_logic;
    input           : in  std_logic_vector(31 downto 0);    
    inputReady      : in  std_logic;
    data            : in  std_logic_vector(31 downto 0);
    wrFlags         : in  std_logic;    
    wrSize          : in  std_logic;
    run             : in  std_logic;
    busy            : in  std_logic;
    send            : out std_logic;
    output          : out std_logic_vector(31 downto 0);
    memoryIn        : in  std_logic_vector(31 downto 0);
    memoryOut       : out std_logic_vector(31 downto 0);
    memoryRead      : out std_logic;
    memoryWrite     : out std_logic
    );
end component;

component rle_enc
generic (
    MEMORY_DEPTH : integer
    ); 
port (
   clock    : in  std_logic;
   reset    : in  std_logic;
   dataIn   : in  std_logic_vector(31 downto 0);
   validIn  : in  std_logic;
   enable   : in  std_logic;
   dataOut  : out std_logic_vector(31 downto 0);
   validOut : out std_logic
   );
end component;

signal opcode           : std_logic_vector(7 downto 0);
signal data             : std_logic_vector(31 downto 0);
signal rleOut           : std_logic_vector(31 downto 0);
signal sample           : std_logic_vector(31 downto 0);
signal syncedInput      : std_logic_vector(31 downto 0);
signal sampleClock      : std_logic;
signal run              : std_logic;
signal reset            : std_logic;
signal rleValid         : std_logic;
signal rleEnable        : std_logic;
signal wrtrigmask       : std_logic_vector(3 downto 0);
signal wrtrigval        : std_logic_vector(3 downto 0);
signal wrtrigcfg        : std_logic_vector(3 downto 0);
signal wrDivider        : std_logic;
signal wrsize           : std_logic;
signal arm              : std_logic;
signal resetCmd         : std_logic;
signal flagDemux        : std_logic; 
signal flagFilter       : std_logic; 
signal flagExternal     : std_logic; 
signal flagInverted     : std_logic; 
signal wrFlags          : std_logic; 
signal sampleReady      : std_logic;
signal armLEDreg        : std_logic;
signal triggerLEDreg    : std_logic;


signal ls_memoryIn      : std_logic_vector(31 downto 0);

attribute UGROUP: string;
attribute UGROUP of i_sync : label is "Sync";


--**************************************************************************************************
BEGIN
--**************************************************************************************************

   data             <= cmd(39 downto 8);
   opcode           <= cmd(7 downto 0);
   reset            <= extReset or resetCmd;
   intReset         <= reset;
   armLED           <= not armLEDreg; --Logic Sniffers LEDs are connected to 3.3V so a logic 0 turns the LED on.
   triggerLED       <= not triggerLEDreg; 
   extClockOut      <= sampleClock;
   extTriggerOut    <= run;
   
    --Generates observable trigger and arm LED signals
    p_LED: process (extReset, clock)
    begin
        if (extReset = '1') then 
            armLEDreg     <= '0';
            triggerLEDreg <= '0';
        elsif rising_edge(clock) then
            if (arm = '1') then
                armLEDreg     <= '1';
                triggerLEDreg <= '0';
            elsif (run = '1') then
                armLEDreg     <= '0';
                triggerLEDreg <= '1';
            end if;
        end if;
    end process p_LED;
   

   -- -- select between internal and external sampling clock
   -- BUFGMUX_intex: entity work.BUFGMUX
   -- port map (
      -- O => sampleClock,       -- Clock MUX output
      -- I0 => clock,            -- Clock0 input
      -- I1 => inputClock,       -- Clock1 input
      -- S => flagExternal       -- Clock select input
   -- );

   sampleClock <= clock;
   
   
i_decoder: decoder 
port map (
    reset       => extReset,
    clock       => clock,
    opcode      => opcode,
    execute     => execute,
    wrtrigmask  => wrtrigmask,
    wrtrigval   => wrtrigval,
    wrtrigcfg   => wrtrigcfg,
    wrspeed     => wrDivider,
    wrsize      => wrsize,
    wrFlags     => wrFlags,
    arm         => arm,
    -- testmode   => testmode,
    IntReset    => resetCmd
    );

i_flags: flags 
port map (
    data            => data(10 downto 0),
    clock           => clock,
    write           => wrFlags,
    demux           => flagDemux,
    filter          => flagFilter,
    external        => flagExternal,
    inverted        => flagInverted,
    rle             => rleEnable,
    numberScheme    => numberScheme,
    testMode        => testMode
    );
   
i_sync: sync
port map (
    reset           => reset,
    clock           => sampleClock,
    input           => input,
    enableFilter    => flagFilter,
    enableDemux     => flagDemux,
    falling         => flagInverted,
    output          => syncedInput
    );

i_sampler: sampler
port map (
    reset       => reset,
    clock       => clock,
    input       => syncedInput,
    exClock     => inputClock,  -- use sampleClock?
    external    => flagExternal,
    data        => data(23 downto 0),
    wrDivider   => wrDivider,
    sample      => sample,
    ready       => sampleReady,
    ready50     => sampleReady50
    );
   
i_trigger: trigger
port map (
    input           => sample,
    inputReady      => sampleReady,
    data            => data,
    clock           => clock,
    reset           => reset,
    wrMask          => wrtrigmask,
    wrValue         => wrtrigval,
    wrConfig        => wrtrigcfg,
    arm             => arm,
    demuxed         => flagDemux,
    run             => run,
    extTriggerIn    => extTriggerIn
    );

i_rle_enc: rle_enc   
generic map (
    MEMORY_DEPTH => MEMORY_DEPTH
    )
port map (
    clock       => clock,
    reset       => reset,
    dataIn      => sample,
    validIn     => sampleReady,
    enable      => rleEnable,
    dataOut     => rleOut,
    validOut    => rleValid
    );

i_controller: controller 
port map (
    clock       => clock,
    reset       => reset,
    input       => rleOut,
    inputReady  => rleValid,
    data        => data,
    wrFlags     => wrflags,     
    wrSize      => wrsize,
    run         => run,
    busy        => outputBusy,
    send        => outputSend,
    output      => output,
    memoryRead  => memoryRead,         -- vom Speicher lesen
    memoryIn    => ls_memoryIn,
    memoryWrite => memoryWrite,       -- in den Speicher schreiben
    memoryOut   => MemoryOut
   );
   
p_RLE_comb: process(rleEnable, memoryIn)
begin
    ls_memoryIn <= memoryIn;
        
    -- ===  RLE-Decoding Bit beim Lesen aus dem Speicher zur höchsten Position für die Übertragung verschieben  === --
    if (rleEnable = '1') then
    
        ls_memoryIn(MEMORY_DEPTH-1) <= '0';
        
        case MEMORY_DEPTH is
            when 0 to 7 =>
                ls_memoryIn( 7) <= memoryIn(MEMORY_DEPTH-1);
            when 8 to 15 =>
                ls_memoryIn(15) <= memoryIn(MEMORY_DEPTH-1);
            when 16 to 23 =>
                ls_memoryIn(23) <= memoryIn(MEMORY_DEPTH-1);
            when 24 to 31 =>
                ls_memoryIn(31) <= memoryIn(MEMORY_DEPTH-1);
            when others =>
                null;
        end case;
     end if;
end process p_RLE_comb;   
   
   
--**************************************************************************************************
END rtl;
--**************************************************************************************************