library IEEE;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library UNISIM;
use UNISIM.VComponents.all;

use work.Firmware_pkg.all;

entity i2c_testbench is
  PORT ( 
    -- 300 MHz clk_in
    CLK_IN_P : in std_logic;
    CLK_IN_N : in std_logic
  );      
end i2c_testbench;

architecture Behavioral of i2c_testbench is

  signal clk_in_buf : std_logic := '0';
  signal clk_300 : std_logic := '0';
  signal inputCounter: unsigned(31 downto 0) := (others=> '0');
  signal i2c_master_reset_n : std_logic := '1';
  signal i2c_master_ena : std_logic := '0';
  signal i2c_addr : std_logic_vector(6 downto 0) := (others=>'0');
  signal i2c_master_rw : std_logic := '0';
  signal i2c_master_data_wr : std_logic_vector(7 downto 0) := (others=>'0');
  signal i2c_master_busy : std_logic := '0';
  signal i2c_master_data_rd : std_logic_vector(7 downto 0) := (others=>'0');
  signal i2c_master_ack_error : std_logic := '0';
  signal i2c_master_sda : std_logic := 'Z';
  signal i2c_master_scl : std_logic := 'Z';
  signal i2c_master_scl_buf : std_logic := '0';
  signal i2c_slave_ioout : std_logic_vector(7 downto 0) := (others=>'0');
  signal i2c_slave_reset_n : std_logic := '1';

  signal injectCounter: unsigned(31 downto 0) := (others=> '0');
  type state_type is (idle_state, reset_state, set_register);
  signal state : state_type := idle_state;
  signal resetCounter: unsigned(31 downto 0) := (others=> '0');
  signal busy_prev : std_logic := '0';

  signal innerWriteState: unsigned(1 downto 0) := (others=>'0');
  signal innerReadState: unsigned(1 downto 0) := (others=>'0');
  signal system_reset : std_logic := '0';

  component i2c_master is
  generic(
    input_clk : INTEGER := 300_000_000; --input clock speed from user logic in Hz
    bus_clk   : INTEGER := 100_000);   --speed the i2c bus (scl) will run at in Hz
  port (
    clk       : IN     STD_LOGIC;                    --system clock
    reset_n   : IN     STD_LOGIC;                    --active low reset
    ena       : IN     STD_LOGIC;                    --latch in command
    addr      : IN     STD_LOGIC_VECTOR(6 DOWNTO 0); --address of target slave
    rw        : IN     STD_LOGIC;                    --'0' is write, '1' is read
    data_wr   : IN     STD_LOGIC_VECTOR(7 DOWNTO 0); --data to write to slave
    busy      : OUT    STD_LOGIC;                    --indicates transaction in progress
    data_rd   : OUT    STD_LOGIC_VECTOR(7 DOWNTO 0); --data read from slave
    ack_error : BUFFER STD_LOGIC;                    --flag if improper acknowledge from slave
    sda       : INOUT  STD_LOGIC;                    --serial data output of i2c bus
    scl       : INOUT  STD_LOGIC                     --serial clock output of i2c bus
  );
  end component;

  --component I2CslaveWith8bitsIO is
  --port (
  --  SDA : inout std_logic;
  --  SCL : inout std_logic;
  --  IOout : out std_logic_vector(7 downto 0)
  --);
  --end component;

  component i2cs_rx is
  generic(
		WR    : std_logic:='0';
		DADDR : std_logic_vector(6 downto 0); --:= "0010001";		   -- 11h (22h) device address
		ADDR  : std_logic_vector(7 downto 0)  --:= "00000000"		   -- 00h	    sub address		
  );
  port (
    RST  : in std_logic;
    SDA : inout std_logic;
    SCL : inout std_logic;
    DOUT : out std_logic_vector(7 downto 0)
  );
  end component;

begin

  -- Generate clock for simulation
  input_clk_simulation_i : if in_simulation generate
    process
      constant clk_period_by_2 : time := 1.666 ns;
      begin
      while 1=1 loop
        clk_in_buf <= '0';
        wait for clk_period_by_2;
        clk_in_buf <= '1';
        wait for clk_period_by_2;
      end loop;
    end process;
  end generate input_clk_simulation_i;
  -- Get clock from pin for synthesis
  input_clk_synthesize_i : if in_synthesis generate
    ibufg_i : IBUFGDS
    port map (
               I => CLK_IN_P,
               IB => CLK_IN_N,
               O => clk_in_buf
    );
  end generate input_clk_synthesize_i;

  bufg_i : BUFG
  port map (
            I => clk_in_buf,
            O => clk_300
  );

  i2c_master_i : i2c_master
  port map (
    clk       => clk_300,
    reset_n   => i2c_master_reset_n,
    ena       => i2c_master_ena, 
    addr      => i2c_addr, 
    rw        => i2c_master_rw,
    data_wr   => i2c_master_data_wr,
    busy      => i2c_master_busy,
    data_rd   => i2c_master_data_rd,
    ack_error => i2c_master_ack_error,
    sda       => i2c_master_sda,
    scl       => i2c_master_scl
  );

  --i2c_slave_i : I2CslaveWith8bitsIO
  --port map(
  --  SDA => i2c_master_sda,
  --  SCL => i2c_master_scl,
  --  IOout => i2c_slave_ioout
  --);

  i2c_master_scl_buf <= '1' when i2c_master_scl='Z' else '0';
  i2c_slave_i : i2cs_rx
  generic map(
    DADDR => "0100111",
    ADDR =>  "11011101"
  )
  port map(
    SDA => i2c_master_sda,
    SCL => i2c_master_scl_buf,
    DOUT => i2c_slave_ioout,
    RST => i2c_slave_reset_n
  );

  resetInjector_i: process (clk_300) is
  begin
    if rising_edge(clk_300) then
      injectCounter <= injectCounter + 1;
      -- Set reset to 0
      if injectCounter=5 then
        system_reset <= '1';
      elsif injectCounter=7 then
        system_reset <= '0';
      end if;
    end if;
  end process;

  -- Logic to control master (FSM)
  -- This code has a bug. The next state should be done once busy goes up
  inputGenerator_i: process (clk_300) is
    VARIABLE busy_cnt : INTEGER RANGE 0 TO 2 := 0;               --counts the busy signal transistions during one transaction
  begin
    if system_reset = '1' then
      state <= reset_state;
    elsif rising_edge(clk_300) then
      case state is
        when idle_state =>
          state <= idle_state;
        when reset_state =>
          resetCounter <= resetCounter +1;
          i2c_master_ena <= '0';                      --clear i2c enable
          -- apply reset
          if resetCounter=3000 then
            i2c_slave_reset_n <='0';
            i2c_master_reset_n <='0';
            state <= reset_state;
          -- release reset for slave
          elsif resetCounter=6000 then
            i2c_slave_reset_n <='1';
            state <= reset_state;
          elsif resetCounter=9000 then
          -- release reset for master
            i2c_master_reset_n <='1';
            state <= reset_state;
          else
            state <= reset_state;
            -- wait for busy to clear
            if resetCounter > 9000 and i2c_master_busy = '0' then
              state <= set_register;
              resetCounter <= (others=>'0');
            end if;
          end if;
        when set_register =>
          busy_prev <= i2c_master_busy;                       --capture the value of the previous i2c busy signal
          IF(busy_prev = '0' AND i2c_master_busy = '1') THEN  --i2c busy just went high
            busy_cnt := busy_cnt + 1;                    --counts the times busy has gone from low to high during transaction
          END IF;
          CASE busy_cnt IS                             --busy_cnt keeps track of which command we are on
            WHEN 0 =>                                    --no command latched in yet
              i2c_master_ena <= '1';                              --initiate the transaction
              i2c_addr <= "0100111";                --set the address of the temp sensor
              i2c_master_rw <= '0';                               --command 1 is a write
              i2c_master_data_wr <= x"dd";                   --set the Register Pointer to the Configuration Register
            WHEN 1 =>                                    --1st busy high: command 1 latched, okay to issue command 2
              i2c_master_data_wr <= x"aa";                       --write the new configuration value to the Configuration Register
            WHEN 2 =>                                    --2nd busy high: command 2 latched
              i2c_master_ena <= '0';                              --deassert enable to stop transaction after command 2
              IF(i2c_master_busy = '0') THEN                      --transaction complete
                busy_cnt := 0;                               --reset busy_cnt for next transaction
                state <= idle_state;                    --advance to setting the Register Pointer for data reads
              END IF;
            WHEN OTHERS => NULL;
          END CASE;
        when others => Null;
      end case;
    end if;
  end process;

  ---- Logic to control master (FSM)
  ---- This code has a bug. The next state should be done once busy goes up
  --inputGenerator_i: process (clk_300) is
  --begin
  --  if system_reset = '1' then
  --    state <= reset_state;
  --  elsif rising_edge(clk_300) then
  --    case state is
  --      when idle_state =>
  --        state <= idle_state;
  --      when reset_state =>
  --        resetCounter <= resetCounter +1;
  --        -- apply reset
  --        if resetCounter=3000 then
  --          i2c_slave_reset_n <='0';
  --          i2c_master_reset_n <='0';
  --          state <= reset_state;
  --        -- release reset for slave
  --        elsif resetCounter=6000 then
  --          i2c_slave_reset_n <='1';
  --          state <= reset_state;
  --        elsif resetCounter=9000 then
  --        -- release reset for master
  --          i2c_master_reset_n <='1';
  --          state <= reset_state;
  --        else
  --          state <= reset_state;
  --          -- wait for busy to clear
  --          if resetCounter > 9000 and i2c_master_busy = '0' then
  --            state <= write_state;
  --            resetCounter <= (others=>'0');
  --          end if;
  --        end if;
  --      when write_state =>
  --        if i2c_master_busy = '0' and innerWriteState = 0 then
  --          i2c_master_ena <= '1';
  --          i2c_master_rw <= '0';
  --          i2c_master_data_wr <= x"dd";
  --          state <= write_state;
  --          innerWriteState <= "01";
  --        elsif i2c_master_busy = '1' and innerWriteState = 1 then
  --        -- once busy rises
  --          state <= write_state;
  --          innerWriteState <= "10";
  --        -- wait for busy to clear
  --        elsif i2c_master_busy = '0' and innerWriteState =2 then
  --          state <= write2_state;
  --          innerWriteState <= "00";
  --          i2c_master_data_wr <= x"00";
  --        end if;
  --      when write2_state =>
  --        if i2c_master_busy = '0' and innerWriteState = 0 then
  --          i2c_master_ena <= '1';
  --          i2c_master_rw <= '0';
  --          i2c_master_data_wr <= x"11";
  --          state <= write2_state;
  --          innerWriteState <= "01";
  --        elsif i2c_master_busy = '1' and innerWriteState = 1 then
  --        -- once busy rises
  --          state <= write2_state;
  --          innerWriteState <= "10";
  --          -- End transaction
  --          i2c_master_ena <= '0';
  --        -- wait for busy to clear
  --        elsif i2c_master_busy = '0' and innerWriteState =2 then
  --          state <= read_state;
  --          innerWriteState <= "00";
  --          i2c_master_data_wr <= x"00";
  --        end if;
  --      when read_state =>
  --        if i2c_master_busy = '0' and innerReadState = 0 then
  --          i2c_master_ena <= '1';
  --          i2c_master_rw <= '1';
  --          state <= read_state;
  --          innerReadState <= "01";
  --        elsif i2c_master_busy = '1' and innerReadState = 1 then
  --        -- once busy rises
  --          state <= read_state;
  --          innerReadState <= "10";
  --          -- End transaction
  --          i2c_master_ena <= '0';
  --        -- wait for busy to clear
  --        elsif i2c_master_busy = '0' and innerReadState =2 then
  --          state <= idle_state;
  --          innerReadState <= "00";
  --        end if;
  --    end case;
  --  end if;
  --end process;

  --inputGenerator_i: process (clk_300) is
  --begin
  --  if rising_edge(clk_300) then

  --    inputCounter <= inputCounter + 1;
  --    -- Set reset to 0
  --    if inputCounter=5 then
  --      i2c_master_reset_n <= '0';
  --      i2c_slave_reset_n <= '0';
  --      i2c_master_ena <= '0';
  --      i2c_master_rw <= '0';
  --      i2c_master_data_wr <= x"00";
  --    -- Release reset
  --    elsif inputCounter=6 then
  --      i2c_slave_reset_n <= '1';
  --    elsif inputCounter=2000 then
  --      i2c_master_reset_n <= '1';
  --    else
  --      -- Wait for busy to be 0
  --      if i2c_master_busy /= '1' then
  --        -- Set ena to 1, rw to 0, data_wr to ff
  --        i2c_master_ena <= '1';
  --        i2c_master_rw <= '0';
  --        i2c_master_data_wr <= x"dd";
  --      end if;
  --    end if;
  --  end if;
  --end process;


end Behavioral;
