library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tx is
  generic (
    DATA_WIDTH      : natural := 8;
    DIVISOR         : natural := 217      -- DIVISOR = 100,000,000 / BAUD_RATE
    );
  port (
    tx_byte  : in  std_logic_vector (DATA_WIDTH-1 downto 0);
    send     : in  std_logic;
    main_clk : in  std_logic;
    rst      : in  std_logic;
    tx_out   : out std_logic := '1';
    tx_valid : out std_logic := '0');

end entity tx;

architecture behavioral of tx is

  type tx_state_type is (tx_idle, tx_start, tx_send, tx_stop_check, tx_done);
  signal tx_state : tx_state_type := tx_idle;

  signal tx_bit_count : natural range 0 to DATA_WIDTH-1 := 0;
  signal sample_count : natural range 0 to DIVISOR-1:= 0;

begin  -- architecture behaviroral

  tx_state_transitions : process (main_clk) is
  begin  -- process state_logic
    if main_clk'event and main_clk = '1' then  -- rising clock edge
      if rst = '1' then
        tx_state <= tx_idle;
      else

        case tx_state is

          when tx_idle =>
            if send = '1' then
              tx_state <= tx_start;
            end if;

          when tx_start =>
            if sample_count = DIVISOR-1 then
              tx_state <= tx_send;
            else
              tx_state <= tx_start;
            end if;

          when tx_send =>
            if tx_bit_count = (tx_byte'length - 1) and sample_count = DIVISOR-1 then
              tx_state <= tx_stop_check;
            else
              tx_state <= tx_send;
            end if;

          when tx_stop_check =>
            if sample_count = DIVISOR-1 then
              tx_state <= tx_done;
            else
              tx_state <= tx_stop_check;
            end if;

          when tx_done =>
            tx_state <= tx_idle;

        end case;
      end if;
    end if;
  end process tx_state_transitions;

  tx_state_assignments : process (main_clk) is
  begin  -- process tx_state_transitions
  if main_clk'event and main_clk = '1' then
    case tx_state is
      when tx_idle =>
        tx_valid <= '0';
      when tx_start =>
        tx_valid <= '0';
      when tx_send =>
        tx_valid <= '0';
      when tx_stop_check =>
        tx_valid <= '0';
      when tx_done =>
        tx_valid <= '1';
    end case;
  end if;
  end process tx_state_assignments;

  tx_bit_counter : process (main_clk) is
  begin  -- process rx_bit_counter
    if main_clk'event and main_clk = '1' then
      if rst = '1' then                 -- asynchronous reset (active high)
        tx_bit_count <= 0;
      else

        if tx_state = tx_idle then
          --tx_out       <= '1';
          tx_bit_count <= 0;
          sample_count <= 0;

        elsif tx_state = tx_start then
          --tx_out <= '0';
          if sample_count = DIVISOR-1 then
            sample_count <= 0;
          else
            sample_count <= sample_count + 1;
          end if;

        elsif tx_state = tx_send then
          --tx_out <= tx_byte(tx_bit_count);
          if sample_count = DIVISOR-1 then
            sample_count <= 0;
            if tx_bit_count = (tx_byte'length - 1) then
              tx_bit_count <= 0;
            else
              tx_bit_count <= tx_bit_count + 1;
            end if;
          else
            sample_count <= sample_count + 1;
          end if;

        elsif tx_state = tx_stop_check then
          --tx_out <= '1';
          if sample_count = DIVISOR-1 then
            sample_count <= 0;
          else
            sample_count <= sample_count + 1;
          end if;

        elsif tx_state = tx_done then
          --tx_out <= '1';
        end if;
      end if;
    end if;
  end process tx_bit_counter;

  tx_out_process : process(main_clk) is
  begin
    if main_clk'event and main_clk = '1' then
      if rst = '1' then                 -- asynchronous reset (active high)
        tx_out <= '1';
      else
        case tx_state is
          when tx_idle =>
            tx_out <= '1';
          when tx_start =>
            tx_out <= '0';
          when tx_send =>
            tx_out <= tx_byte(tx_bit_count);
          when tx_stop_check =>
            tx_out <= '1';
          when tx_done =>
            tx_out <= '1';
        end case;
      end if;
    end if;
  end process;
    
end architecture behavioral;
