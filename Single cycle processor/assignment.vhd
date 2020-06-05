library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
library work;
use work.common.all;
entity assignment is
    port (reset : in std_logic;
        clk : in std_logic;
        y : out word);
end assignment;

architecture behavioral of assignment is

signal alu_func : alu_func_t := ALU_NONE;
signal load_type : load_type_t :=LOAD_NONE;
signal alu_A : word := x"00000000"; -- one input of alu
signal alu_B : word := x"00000000"; -- one input of alu
signal alu_out : word := x"00000000"; -- output of alu
signal reg_B : word := x"00000000"; -- the data of register rs2
signal regb_out : word := x"00000000"; -
-- the output of pipeline for the data of rs2
signal reg_A : word := x"00000000"; --the data of register rs1
signal fwd_A : word := x"00000000"; --the output of mux for forward
signal fwd_B : word := x"00000000"; --the output of mux for forward
signal imm : word := x"00000000"; -- the imm for I type
signal imm_rd : word := x"00000000"; -- the imm for R type
signal ir : word := x"00000000"; -- the instruction
signal ir2 : word := x"00000000"; -- the instruction put into pipeline
signal dmem_out : word := x"00000000"; -- the output of dmem
signal rf_wdata : word := x"00000000"; -- the data of write back
signal lu_imm : word := x"00000000";
signal fwd_rs1 : word := x"00000000";
signal fwd_rs2 : word := x"00000000";
signal fwd_rd : word := x"00000000";
signal branch_imm : unsigned(word'range) := x"00000000";
signal jalr_imm : unsigned(word'range) :=x"00000000";
signal lhu_imm : unsigned(word'range) :=x"00000000";
signal pc2 : unsigned(word'range) := x"00000000";
signal pc3 : unsigned(word'range) := x"00000000";
-- instruction fields
signal opcode : opcode_t;
--signal opcode_out1 : opcode_t;
signal funct3 : std_logic_vector(2 downto 0);
--signal func3 : std_logic_vector(2 downto 0);
signal funct7 : std_logic_vector(6 downto 0);
signal rs1 : std_logic_vector(4 downto 0);
signal rs2 : std_logic_vector(4 downto 0);
signal rd : std_logic_vector(4 downto 0);
--signal fwd_rd : std_logic_vector(4 downto 0);
signal pc : unsigned(word'range) := x"00000000";


-- control signals
signal regwrite : std_logic;
signal pipewrite : std_logic;
signal fwd1 : std_logic;
signal fwd2 : std_logic;
signal wbsel : std_logic_vector(2 downto 0);
signal memwrite : std_logic;
signal op2sel : std_logic_vector(2 downto 0);
signal PCSel : std_logic_vector(1 downto 0);


component alu is
port (alu_func : in alu_func_t;
    op1 : in word;
    op2 : in word;
    result : out word);
end component alu;

component pipeline is
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
    end component pipeline;

component imem is
port(
    addr : in std_logic_vector(5 downto 0);
    dout : out word);
end component imem;

component dmem is
port (reset : in std_logic;
    clk : in std_logic;
    raddr : in std_logic_vector(5 downto 0);
    dout : out word;
    waddr : in std_logic_vector(5 downto 0);
    din : in word;
    we : in std_logic);
end component dmem;

component regfile is
port (reset : in std_logic;
    clk : in std_logic;
    addra : in std_logic_vector(4 downto 0);
    addrb : in std_logic_vector(4 downto 0);
    rega : out word;
    regb : out word;
    addrw : in std_logic_vector(4 downto 0);
    dataw : in word;
    we : in std_logic);
end component regfile;

begin
-- datapath
    alu0: alu port map(
        alu_func => alu_func,
        op1 => alu_A,
        op2 => alu_B,
        result => alu_out);

    imem0: imem port map(
        addr => std_logic_vector(pc(7 downto 2)),
        dout => ir);

    dmem0: dmem port map(
        reset => reset,
        clk => clk,
        raddr => alu_out(7 downto 2),
        dout => dmem_out,
        waddr => alu_out(7 downto 2),
        din => regb_out,
        we => memwrite);

    rf0: regfile port map(
        reset => reset,
        clk => clk,
        addra => rs1,
        addrb => rs2,
        rega => reg_A,
        regb => reg_B,
        addrw => rd,
        dataw => rf_wdata,
        we => regwrite);

    pipeline0: pipeline port map(
        reset=> reset,
        clk => clk,
        pc_in=>word(pc2),
        rd1_in=>fwd_A,
        rd2_in=>fwd_B,
        ir_in=> ir,
        unsigned(pc_out) =>pc3,
        rd1_out=>alu_A,
        rd2_out=>regb_out,
        ir_out=> ir2,
        we => pipewrite);

alu_B <= regb_out when op2sel = "000" else
    imm when op2sel = "001" else
    imm_rd when op2sel ="010" else
    word(lhu_imm) when op2sel ="011";
-- the mux for alu_b
rf_wdata <= alu_out when wbsel = "000" else
    word(lu_imm) when wbsel ="100" else
    regb_out when wbsel = "001" else
    dmem_out when wbsel ="010" else
    word(pc3+4) when wbsel="011";
-- the mux for write data back
pc2 <=pc+4;-- that is the input for pipeline
fwd_A <= alu_out when fwd1 ='0' else
    reg_A;
--The mux for the forward
fwd_B <= alu_out when fwd2 ='0' else
    reg_B;
--The mux for the forward
-- instruction fields
imm(31 downto 12) <= (others => ir2(31)); --I type sign extend
imm(11 downto 0) <= ir2(31 downto 20);
imm_rd(31 downto 12) <= (others => funct7(6));-- R type
imm_rd(11 downto 5) <= funct7;
imm_rd(4 downto 0) <= rd;
rs1 <= ir(19 downto 15); -- the address of rs1
rs2 <= ir(24 downto 20); -- the address of rs2
rd <= ir2(11 downto 7); -- the address of rd
funct3 <= ir2(14 downto 12);
funct7 <= ir2(31 downto 25);
opcode <= ir2(6 downto 0);
--the sign extend and zero extend
branch_imm(31 downto 13) <= (others => ir2(31));
branch_imm(12 downto 0) <= unsigned(ir2(31) & ir2(7) & ir2(30 downt
o 25) & ir2(11 downto 8) & '0');
--The B type for branch eq
jalr_imm(31 downto 12) <= (others => ir2(31));
jalr_imm(11 downto 0) <= unsigned(ir2(31 downto 20));
--The I type for jalr
lu_imm(11 downto 0) <= (others => '0');
lu_imm(31 downto 12) <= ir2(31 downto 12);
--The U type for lui
lhu_imm(15 downto 0)<= unsigned(imm(15 downto 0));
lhu_imm(31 downto 16)<=(others =>'0');
--the lhu imm convert to unsigned number
fwd_rs1(4 downto 0)<= ir(19 downto 15);
fwd_rs1(31 downto 5)<=(others =>'0');
fwd_rs2(4 downto 0)<= ir(24 downto 20);
fwd_rs2(31 downto 5)<=(others =>'0');
fwd_rd(4 downto 0)<= ir2(11 downto 7);
fwd_rd(31 downto 5)<=(others =>'0');
decode_proc : process (ir2, funct7, funct3, opcode) is
begin
-- -- put control for forward instruction here
    if(fwd_rs1/=fwd_rd) then
    fwd1<='1';
    else
    fwd1<='0';
    end if;
    if(fwd_rs2/=fwd_rd) then
    fwd2<='1';
    else
    fwd2<='0';
    end if;
    pipewrite<='1';
    regwrite <= '0';
    op2sel <= "000";
    memwrite <= '0';
    wbsel <= "000";
    alu_func <= ALU_NONE;
    PCSel <="00";
    case opcode is
    when OP_ITYPE =>
-- put control for Rtype instruction here
        regwrite <= '1';
        op2sel <= "001";
        case (funct3) is
            when "000" => alu_func <= ALU_ADD;
            when "001" => alu_func <= ALU_SLL;
            when "010" => alu_func <= ALU_SLT;
            when "011" => alu_func <= ALU_SLTU;
            when "100" => alu_func <= ALU_XOR;
            when "110" => alu_func <= ALU_OR;
            when "111" => alu_func <= ALU_AND;
            when "101" =>
                if (ir2(30) = '1') then
                alu_func <= ALU_SRA;
                else
                alu_func <= ALU_SRL;
                end if;
            when others => null;
        end case;

    when OP_RTYPE =>
-- put control for Rtype instruction here
        regwrite <= '1';
        op2sel<="000";
        case (funct3) is
            when "000" =>
            if (ir2(30) = '1') then
            alu_func <= ALU_SUB;
            else
            alu_func <= ALU_ADD;
            end if;
        when "001" => alu_func <= ALU_SLL;
        when "010" => alu_func <= ALU_SLT;
        when "011" => alu_func <= ALU_SLTU;
        when "100" => alu_func <= ALU_XOR;
        when "101" =>
            if (ir2(30) = '1') then
            alu_func <= ALU_SRA;
            else
            alu_func <= ALU_SRL;
            end if;
        when "110" => alu_func <= ALU_OR;
        when "111" => alu_func <= ALU_AND;
        when others => null;
    end case;

    when op_LUI =>
-- put control for LUi instruction here
        op2sel <="100";
        regwrite <='1';
        wbsel <="100";
        memwrite <='0';
        PCSel <= "00";
        alu_func <= ALU_ADD;

-- put control for AUIPC instruction here
    when op_AUIPC =>
        op2sel <="001";
        regwrite <='1';
        wbsel <="100";
        memwrite <='0';
        PCSel <= "00";
        alu_func <= ALU_ADD;

    when OP_JALR =>
-- put control for jalr instruction here
        op2sel <="000";
        regwrite <='1';
        wbsel <="011";
        memwrite <='0';
        PCSel <= "10";
        alu_func <= ALU_ADD;

    when OP_LOAD =>
-- put control for load instruction here
        regwrite <= '1';
        memwrite <= '1';
        wbsel <= "010";
        PCSel <="00";
        alu_func <= ALU_ADD;
    case (funct3) is
        when "010" => load_type <= LW;
            op2sel <= "001";
        when "101" => load_type <= LHU;
            op2sel <= "011";
        when others => null;
    end case;

    when OP_STORE =>
-- put control for store instruction here
        regwrite <= '1';
        op2sel <= "010";
        memwrite <= '1';
        wbsel <= "001";
        PCSel <="00";
        alu_func <= ALU_ADD;

    when OP_BRANCH =>
-- put control for branch instruction here
        regwrite <= '0';
        wbsel<="000";
        memwrite <= '0';
        op2sel <="000";
        alu_func<= ALU_NONE;
        if (alu_A /=regb_out) then
            PCSel <= "01";
        else
            PCSel <="00";
        end if;
    when others => null;
end case;
end process;

y <= alu_out;
acc: process(reset, clk)
begin
    if (reset = '1') then
        pc <= x"00000054";

    elsif rising_edge(clk) then

        if (PCSel ="01") then
            pc<= pc3 + branch_imm;
        elsif(PCSel ="00") then
            pc <= pc + 4;
        elsif(PCSel="10") then
            pc<=(unsigned(alu_A)+jalr_imm);
        end if;
    end if;
end process;
end architecture;
