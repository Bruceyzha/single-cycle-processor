library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.common.all;
entity pipeline is
    port (reset : in std_logic;
        clk : in std_logic;
        pc_in : in word;
        rd1_in : in word;
        rd2_in : in word;
        ir_in : in word;
        pc_out : out word;
        rd1_out : out word;
        rd2_out : out word;
        ir_out : out word;
        we : in std_logic);
end entity pipeline;
--
-- Note: Because this core is FPGAtargeted,the idea is that these registers
-- will get implemented as dualportDistributed RAM. Because there is no
-- such thing as tripleportmemory in an FPGA (that I know of), and we
--  need 3 ports to support 2 reads and 1 write per cycle, the easiestway
-- to implement that is to have two identical banks of registers that contain
-- the same data. Each uses 2 ports and everybody's happy.
--
architecture rtl of pipeline is
    type regbank_t is array (0 to 3) of word;
    signal regbank0 : regbank_t := (others => (others => '0'));

begin -- architecture Behavioral
   -- purpose: create registers
-- type : sequential
-- inputs : clk
-- outputs:
    registers_proc : process (clk) is
    begin -- process registers_proc
        if rising_edge(clk) then
            if (we = '1') then
                regbank0(0) <= pc_in;
                regbank0(1) <= rd1_in;
                regbank0(2) <= rd2_in;
                regbank0(3) <= ir_in;
        end if;
    end if;
    end process registers_proc;
    pc_out <= regbank0(0);
    rd1_out <= regbank0(1);
    rd2_out <= regbank0(2);
    ir_out <= regbank0(3);
-- asynchronous read
end architecture rtl;