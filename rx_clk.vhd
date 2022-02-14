library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity rx_clk is
  generic (
    DIVISOR         : natural := 27);      -- DIVISOR = 100,000,000 / BAUD_RATE
  port (
    main_clk  : in  std_logic;          -- clock
    rst       : in  std_logic;          -- reset(stratix button is normally low)
    rx_clk_en : in  std_logic;          -- enable input
    rx_clk    : out std_logic           -- 1 clk spike at the middle of bit
    );
end rx_clk;

architecture Behavioral of rx_clk is

  constant COUNTER_BITS : integer := integer(ceil(log2(real(DIVISOR))));
  signal sample_counter : natural range 0 to DIVISOR-1 := 0;

  type rx_clk_states is (rx_clk_off, rx_clk_initial, rx_clk_regular);
  signal rx_clk_state : rx_clk_states := rx_clk_off;

begin

  rx_clk_state_transitions : process (main_clk) is
  begin  -- process SM_state_transitions
    if main_clk'event and main_clk = '1' then
      if rst = '1' then                 -- synchronous reset (active high)
        rx_clk_state <= rx_clk_off;
      else
        if rx_clk_en = '1' then
          case rx_clk_state is
            when rx_clk_off =>
              rx_clk_state <= rx_clk_initial;

            when rx_clk_initial =>
              if sample_counter = (DIVISOR/2)-1 then
                rx_clk_state <= rx_clk_regular;
              else
                rx_clk_state <= rx_clk_initial;
              end if;

            when rx_clk_regular =>
              rx_clk_state <= rx_clk_regular;

          end case;
        elsif rx_clk_en = '0' then
          rx_clk_state <= rx_clk_off;
        end if;

      end if;
    end if;
  end process rx_clk_state_transitions;

-- sample each bit 8 times, take the sample at the middle
  rx_sampling_process : process (main_clk) is
  begin
    if main_clk'event and main_clk = '1' then
      if rst = '1' then
        sample_counter <= 0;
        rx_clk         <= '0';
      else
        if rx_clk_state = rx_clk_off then
          sample_counter <= 0;
          rx_clk         <= '0';

        elsif rx_clk_state = rx_clk_initial then
          if sample_counter = (DIVISOR/2)-1 then
            sample_counter <= 0;
            rx_clk         <= '1';
          else
            sample_counter <= sample_counter + 1;
            rx_clk         <= '0';
          end if;

        elsif rx_clk_state = rx_clk_regular then
          if sample_counter = (DIVISOR-1) then
            sample_counter <= 0;
            rx_clk         <= '1';
          else
            sample_counter <= sample_counter + 1;
            rx_clk         <= '0';
          end if;
        end if;
      end if;
    end if;
  end process;

end Behavioral;
