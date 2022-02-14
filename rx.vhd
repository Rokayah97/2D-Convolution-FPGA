library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity rx is
  generic (
    DATA_WIDTH : natural := 8);

  port (
    rx_in     : in  std_logic;
    main_clk  : in  std_logic;
    rst       : in  std_logic;
    rx_clk    : in  std_logic;
    rx_clk_en : out std_logic;
    rx_byte   : out std_logic_vector (DATA_WIDTH-1 downto 0) := (others => '0');
    rx_valid  : out std_logic                                := '0';
    rx_fail   : out std_logic                                := '0'
    );

end entity rx;

architecture behavioral of rx is

  type rx_state_type is (rx_idle, rx_start, rx_receive, rx_stop_check, rx_failed);
  signal rx_state : rx_state_type := rx_idle;

  signal rx_bit_count : natural range 0 to DATA_WIDTH-1 := 0;

begin  -- architecture behavioral

  rx_state_transitions : process (main_clk) is
  begin  -- process state_logic

    if main_clk'event and main_clk = '1' then  -- rising clock edge
      if rst = '1' then                        -- synchronous reset (active high)
        rx_state <= rx_idle;
      else
        case rx_state is

          when rx_idle =>
            if rx_in = '0' then
              rx_state <= rx_start;
            else
              rx_state <= rx_idle;
            end if;

          when rx_start =>
            if rx_clk = '1' then
              rx_state <= rx_receive;
            else
              rx_state <= rx_start;
            end if;

          when rx_receive =>
            if rx_clk = '1' then
              if rx_bit_count = (DATA_WIDTH-1) then
                rx_state <= rx_stop_check;
              else
                rx_state <= rx_receive;
              end if;
            end if;

          when rx_stop_check =>
            if rx_clk = '1' then
              if rx_in = '1' then
                rx_state <= rx_idle;
              else
                rx_state <= rx_failed;
              end if;
            end if;

          when rx_failed =>
            rx_state <= rx_failed;
        end case;
      end if;
    end if;

  end process rx_state_transitions;

  rx_state_assignments : process (main_clk) is
  begin  -- process state_assignments
  if main_clk'event and main_clk = '1' then
    case rx_state is
      when rx_idle =>
        rx_clk_en <= '0';
        rx_fail   <= '0';
      when rx_start =>
        rx_clk_en <= '1';
        rx_fail   <= '0';
      when rx_receive =>
        rx_clk_en <= '1';
        rx_fail   <= '0';
      when rx_stop_check =>
        rx_clk_en <= '1';
        rx_fail   <= '0';
      when rx_failed =>
        rx_clk_en <= '0';
        rx_fail   <= '1';
    end case;
  end if;
  end process rx_state_assignments;

  rx_valid_generator : process (main_clk) is
  begin
    if main_clk'event and main_clk = '1' then
      if rst = '1' then
        rx_valid <= '0';
      else
        if rx_state = rx_stop_check then
          --if rx_bit_count = (DATA_WIDTH - 1) and rx_clk = '1' then
          if rx_clk = '1' then
            rx_valid <= '1';
          else
            rx_valid <= '0';
          end if;
        else
          rx_valid <= '0';
        end if;
      end if;
    end if;
  end process rx_valid_generator;

  rx_bit_counter : process (main_clk) is
  begin  -- process rx_bit_counter
    if main_clk'event and main_clk = '1' then
      if rst = '1' then                 -- asynchronous reset (active high)
        rx_bit_count <= 0;
      else
        if rx_clk = '1' then
          if rx_state = rx_receive then
            rx_byte(rx_bit_count) <= rx_in;
            if rx_bit_count = (DATA_WIDTH-1) then
              rx_bit_count <= 0;
            else
              rx_bit_count <= rx_bit_count + 1;
            end if;
          else
            rx_bit_count <= 0;
          end if;
        end if;
      end if;
    end if;
  end process rx_bit_counter;

end architecture behavioral;
