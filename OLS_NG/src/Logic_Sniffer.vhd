----------------------------------------------------------------------------------
-- Logic_Sniffer.vhd
--
-- Copyright (C) 2006 Michael Poppitz
-- Copyright (C) 2011-2012 Mario Schrenk
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
-- Logic Analyzer top level module. It connects the core with the hardware
-- dependend IO modules and defines all inputs and outputs that represent
-- phyisical pins of the fpga.
--
-- It defines two constants FREQ and RATE. The first is the clock frequency
-- used for receiver and transmitter for generating the proper baud rate.
-- The second defines the speed at which to operate the serial port.
--
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity Logic_Sniffer is
generic (
    MEMORY_DEPTH            : integer :=  8;               
    Physical_MEMORY_DEPTH   : integer := 36;               
    CLOCK_SPEED             : integer := 50
   );
port (
    RESET_i         : in  std_logic;
    CLK_i           : in  std_logic;

    ExtClk_i        : in  std_logic;
    ExtTrigger_i    : in  std_logic;
    ExtInside_i     : in  std_logic_vector(31 downto 0);
    ExtOutside_i    : in  std_logic_vector(31 downto 0);

    RS232_Speed_i   : in  std_logic_vector(1 downto 0);
    RS232_XonXoff_i : in  std_logic; 
    RS232_Rx_i      : in  std_logic;
    RS232_Tx_o      : out std_logic;

    LED_no          : out std_logic_vector(7 downto 0);

    -- ===  SRAM  === --
    SRAM_CE_no      : out   std_logic;                                                              -- Chip Enable (low active)
    SRAM_OE_no      : out   std_logic;                                                              -- Data_o Enable (low active)
    SRAM_WE_no      : out   std_logic;                                                              -- Write Enable (low active)
    SRAM_Addr_o     : out   std_logic_vector(17-1 downto 0);                                        --
    SRAM_Data_io    : inout std_logic_vector(8-1 downto 0)                                          -- bi-directional data ports
   );
end Logic_Sniffer;

--**************************************************************************************************
ARCHITECTURE rtl OF Logic_Sniffer IS
--**************************************************************************************************


component eia232
generic (
    FREQ : integer;
    SCALE : integer;
    RATE : integer
    );
port (
    clock : in  std_logic;
    reset : in  std_logic;
    speed : in  std_logic_vector(1 downto 0);
    XOnOff_i : in  std_logic;                         -- enable Xon/Xoff
    rx : in  std_logic;
    data : in  std_logic_vector(31 downto 0);
    send : in  std_logic;
    tx : out std_logic;
    cmd : out std_logic_vector(39 downto 0);
    execute : out std_logic;
    busy : out  std_logic;
    LED_Xon_o  : out std_logic;
    LED_Xoff_o : out std_logic
    );
end component;

component core
generic (
    MEMORY_DEPTH    : integer
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
    ExtTriggerIn    : in  std_logic;
    extTriggerOut   : out std_logic;           
    extClockOut     : out std_logic;
    armLED          : out std_logic;
    triggerLED      : out std_logic;
    numberScheme    : out std_logic;
    testMode        : out std_logic         
    );
end component;

component sram_bram
generic (
    ADDRESS_WIDTH : integer;
    MEMORY_DEPTH : integer
);
port (
    reset     : in  std_logic;
    clock     : in  std_logic;
    input     : in  std_logic_vector(35 downto 0);
    output    : out std_logic_vector(35 downto 0);
    read      : in  std_logic;
    write     : in  std_logic
    );
end component;
    
signal cmd                      : std_logic_vector(39 downto 0);
signal memoryIn                 : std_logic_vector(31 downto 0);
signal memoryOut                : std_logic_vector(31 downto 0);
signal ls_input                 : std_logic_vector(31 downto 0);
signal output                   : std_logic_vector(31 downto 0);
-- signal clock                    : std_logic;
-- signal clk                      : std_logic;
signal read                     : std_logic; 
signal write                    : std_logic; 
signal execute                  : std_logic; 
signal send                     : std_logic; 
signal busyReg                  : std_logic; 
signal testModeReg              : std_logic := '0'; 
signal numberSchemeReg          : std_logic := '1'; 
-- signal testmodeState            : std_logic := '0';  --Puts the OLS into a test mode. The Wing connector outputs a test pattern that can be captured by the Buffered connector with a ribbon cable.
-- signal numbering                : std_logic := '0';      --Selects the number scheme on the OLS. 0 is Inside scheme 1 is Outside scheme
signal ti_eia232_ts_LED_Xon     : std_logic;
signal ti_eia232_ts_LED_Xoff    : std_logic;
signal test_counter             : unsigned(31 downto 0) := (others => '0');



-- signal ExtClk_i                 : std_logic;
-- signal ExtTrigger_i             : std_logic;
signal extClockOut              : std_logic;
signal extTriggerOut            : std_logic;
signal armLED                   : std_logic;
signal triggerLED               : std_logic;
signal li_Core_ls_intReset      : std_logic;

-- signal ls_J24_input             : std_logic_vector(3 downto 0);
signal ti_Status_LED_ts_Status_LED  : std_logic;


-- signal ti_n2m_Translate_ls_WriteDataStrobe  : std_logic;
-- signal ti_n2m_Translate_ls_WriteData        : std_logic_vector(35 downto 0);
-- signal ti_n2m_Translate_ls_ReadDataStrobe   : std_logic;
-- signal ti_SRAM_ls_output                    : std_logic_vector(35 downto 0);



constant FREQ       : integer := 100000000;         -- limited to 100M by onboard SRAM
-- constant TRXSCALE   : integer := 32;                -- 100M / 28 / 115200 = 31 (5bit)  --If serial communications are not working then try adjusting this number.
-- constant RATE       : integer := 115200;            -- maximum & base rate
-- constant TRXSCALE   : integer := 15;                -- 100M / 28 / 230400 = 15 (4bit)  --If serial communications are not working then try adjusting this number.
constant TRXSCALE   : integer := 31;                -- 100M / 14 / 230400 = 31 (5bit)  --If serial communications are not working then try adjusting this number.
constant RATE       : integer := 230400;            -- maximum & base rate


--**************************************************************************************************
BEGIN
--**************************************************************************************************

    -- -----------------------------------------------
    -- Abbildungen Extern -> Intern
    -- -----------------------------------------------

    -- b_MapExt2Int: block
    -- begin
    -- end block b_MapExt2Int;


    -- -------------------------------------------------
    -- -- Abbildungen Intern <-> Intern
    -- -------------------------------------------------
    
    -- b_MapInt2Int: block
    -- begin
    -- end block b_MapInt2Int;


    -------------------------------------------------
    -- Abbildungen Intern -> Extern
    -------------------------------------------------

    b_MapInt2Ext: block
    begin
        LED_no(7) <= armLED;
        LED_no(6) <= triggerLED;
        LED_no(5) <= not('0');
        LED_no(3) <= not(ti_eia232_ts_LED_Xoff);
        LED_no(4) <= not(ti_eia232_ts_LED_Xon); 
        LED_no(2) <= not('0');
        LED_no(1) <= not('0');
        LED_no(0) <= not(ti_Status_LED_ts_Status_LED);
    end block b_MapInt2Ext;

    
   -- ===  Multiplexer to connect test mode and number schemes  === --
    process(RESET_i, CLK_i)
    begin
        if (RESET_i = '1') then
            ls_input        <= (others => '0');
            test_counter    <= (others => '0');
        elsif rising_edge(CLK_i) then
            if (testModeReg = '1') then 
                -- ===  Test Mode  === --
               ls_input     <= "0000000" & std_logic_vector(test_counter(31 downto 7));
               -- ls_input     <= std_logic_vector(test_counter);
               test_counter <= test_counter + 1;
            else 
                if (numberSchemeReg = '1') then
                    -- ===  Outside Number Scheme  === --
                    ls_input <= ExtOutside_i;
                else
                    -- ===  Inside Number Scheme  === --
                    ls_input <= ExtInside_i;
                end if;
            end if;
        end if;
    end process;


    i_Status_LED: entity work.Status_LED
    port map (
        RESET_i              => RESET_i,                                                            -- Reset      (high active)
        CLK_i                => CLK_i,                                                              -- Systemtakt (132,7104 MHz)            -- blinkt viel langsamer ;-) (sp�ter noch anpassen)

        slow_i               => '0',                                                                -- Blinken im Halbsekundentakt
        fast_i               => '1',                                                                -- sehr schnelles Blinken

        Status_LED_o         => ti_Status_LED_ts_Status_LED                                         -- blinkende Status-LED
        );

    i_eia232: eia232
    generic map (
        FREQ  => FREQ,
        SCALE => TRXSCALE,
        RATE  => RATE
        )
    port map (
        clock       => CLK_i, 
        reset       => RESET_i,
        speed       => RS232_Speed_i,                       -- SPEED
        XOnOff_i    => RS232_XonXoff_i,                     -- enable Xon/Xoff
        rx          => RS232_Rx_i,
        tx          => RS232_Tx_o,
        cmd         => cmd,
        execute     => execute,
        data        => output,
        send        => send,
        busy        => busyReg,
        LED_Xon_o   => ti_eia232_ts_LED_Xon,
        LED_Xoff_o  => ti_eia232_ts_LED_Xoff
        );

    i_core: core
    generic map (
        MEMORY_DEPTH => MEMORY_DEPTH
    )
    port map (
        clock           => CLK_i,
        extReset        => RESET_i,
        cmd             => cmd,
        execute         => execute,
        intReset        => li_Core_ls_intReset,
        input           => ls_input,
        inputClock      => ExtClk_i,
        -- sampleReady50   => ready50,
        output          => output,
        outputSend      => send,
        outputBusy      => busyReg,
        memoryWrite     => write,
        memoryOut       => memoryOut,
        memoryRead      => read,
        memoryIn        => memoryIn,
        ExtTriggerIn    => ExtTrigger_i,
        extTriggerOut   => extTriggerOut,
        extClockOut     => extClockOut,
        armLED          => armLED,
        triggerLED      => triggerLED,
        numberScheme    => numberSchemeReg,
        testMode        => testModeReg
        );

    -- process(CLK_i)
    -- begin
        -- if rising_edge(CLK_i) then
            -- write_p0 <= write;
        -- end if;
    -- end process;

    -- i_n2m_Translate: entity work.n2m_Translate
    -- generic map (
        -- n                   => MEMORY_DEPTH,
        -- m                   => Physical_MEMORY_DEPTH
        -- )
    -- port map (
        -- RESET_i             => li_Core_ls_intReset,                         -- Reset (high active)
        -- CLK_i               => CLK_i,                                       -- Systemtakt

        -- -- ===  Core-Interface  === --
        -- WriteDataStrobe_i   => write,
        -- WriteData_i         => memoryOut,

        -- ReadDataStrobe_i    => read,
        -- ReadData_o          => memoryIn,

        -- -- ===  Memory-Interface  === --
        -- WriteDataStrobe_o   => ti_n2m_Translate_ls_WriteDataStrobe,
        -- WriteData_o         => ti_n2m_Translate_ls_WriteData(Physical_MEMORY_DEPTH-1 downto 0),

        -- ReadDataStrobe_o    => ti_n2m_Translate_ls_ReadDataStrobe,
        -- ReadData_i          => ti_SRAM_ls_output(Physical_MEMORY_DEPTH-1 downto 0)
        -- );


    -- i_sram: sram_bram
    -- generic map (
        -- ADDRESS_WIDTH => 12,
        -- MEMORY_DEPTH => Physical_MEMORY_DEPTH
        -- )
    -- port map (
        -- reset   => RESET_i,
        -- -- reset   => li_Core_ls_intReset,
        -- clock   => CLK_i,
        -- write   => ti_n2m_Translate_ls_WriteDataStrobe,
        -- input   => ti_n2m_Translate_ls_WriteData,
        -- read    => ti_n2m_Translate_ls_ReadDataStrobe,
        -- output  => ti_SRAM_ls_output
        -- );
    
    
    i_external_SRAM: entity work.external_SRAM
    generic map (
        gAddress_Width  => 17,                                                                      -- 17 = 128KiB static RAM
        gData_Width     =>  8
        )
    port map (
        Reset_i         => RESET_i,
        CLK_i           => CLK_i,
    
        Write_i         => write,
        Data_i          => memoryOut(7 downto 0),
        Read_i          => read,
        Data_o          => memoryIn(7 downto 0),
    
        -- ===  SRAM  === --
        SRAM_CE_no      => SRAM_CE_no,                                                              -- chip enable (low active)
        SRAM_OE_no      => SRAM_OE_no,                                                              -- data_o enable (low active)
        SRAM_WE_no      => SRAM_WE_no,                                                              -- write enable (low active)
        SRAM_Addr_o     => SRAM_Addr_o,                                                             --
        SRAM_Data_io    => SRAM_Data_io                                                             -- bi-directional data ports
        );

    memoryOut(31 downto 8) <= (others => '0');
    memoryIn(31 downto 8)  <= (others => '0');
   
--**************************************************************************************************
END rtl;
--**************************************************************************************************