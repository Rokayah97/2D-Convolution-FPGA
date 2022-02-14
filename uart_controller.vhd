library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.uart_pck.all;

entity uart_controller is
  generic (
    XRES            : natural := 60;  -- x_resolution
    YRES            : natural := 60;  -- y_resolution
    DATA_WIDTH      : natural := 8;
    DIVISOR         : natural := 108);   -- DIVISOR = 100,000,000 / BAUD_RATE
  port (
    clk       : in  std_logic;
    rst       : in  std_logic;
    rx_in     : in  std_logic;
--    tx_byte : in  std_logic_vector (DATA_WIDTH-1 downto 0);
--    tx_send : in  std_logic;
    tx_out    : out std_logic := '1';
    rx_fail   : out std_logic := '0';
    tx_test   : out std_logic := '0');
--    rx_in_dbg : out std_logic);

end entity uart_controller;

architecture behavior of uart_controller is

--  component ila_0 is
--    port (
--      clk : in std_logic;

--      trig_in     : in  std_logic;
--      trig_in_ack : out std_logic;
--      probe0      : in  std_logic_vector(0 downto 0);
--      probe1      : in  std_logic_vector(0 downto 0);
--      probe2      : in  std_logic_vector(0 downto 0);
--      probe3      : in  std_logic_vector(0 downto 0);
--      probe4      : in  std_logic_vector(2 downto 0)
--      );
--  end component ila_0;

  type uart_sm is (uart_idle, uart_conv_receive, uart_rx_receive, uart_rx_stop,
                   uart_tx_send);

  constant FRAME_SIZE     : natural := XRES*YRES;
  constant CONV_MAT_SIZE  : natural := 9; -- Convolution kernel size (M x N)
  constant CONV_DIM       : natural := 3;
  constant CONV_SIZE      : natural := (XRES-(CONV_DIM-1))*(YRES-(CONV_DIM-1)); -- Convolved image size
  
  signal uart_state       : uart_sm                                  := uart_idle;
  signal rx_package_count : natural range 0 to FRAME_SIZE-1          := 0;
  signal rx_conv_count    : natural range 0 to CONV_MAT_SIZE-1       := 0;
  signal rx_byte_im       : std_logic_vector (DATA_WIDTH-1 downto 0) := (others => '0');
  signal rx_valid_im      : std_logic                                := '0';
  signal tx_byte_im       : std_logic_vector (DATA_WIDTH-1 downto 0) := (others => '0');
  signal tx_send_im       : std_logic                                := '0';
  signal tx_send_delay    : std_logic                                := '0';
  signal tx_valid_im      : std_logic                                := '0';
  signal tx_package_count : integer range 0 to CONV_SIZE-1           := 0;
  
  signal tx_out_im    : std_logic;
  signal rx_clk_im    : std_logic;
  signal rx_clk_en_im : std_logic;
  signal conv_ram_en  : std_logic := '0';
  signal conv_done_im : std_logic;

--  signal rx_in_dbg_im         : std_logic;
--  signal rx_package_count_vec : std_logic_vector(2 downto 0);
  signal tx_test_im           : std_logic := '0';


begin  -- architecture behavior

  uart_state_transitions : process (clk) is
  begin  -- process uart_state_transitions
    if clk'event and clk = '1' then     -- rising clock edge
      if rst = '1' then                 -- synchronous reset (active high)
        uart_state <= uart_idle;
      else

        case uart_state is
          when uart_idle =>
            if rx_in = '0' then
              uart_state <= uart_conv_receive;
            else
              uart_state <= uart_idle;
            end if;
            
          when uart_conv_receive =>
            if rx_valid_im = '1' and rx_conv_count = CONV_MAT_SIZE-1 then
              uart_state <= uart_rx_receive;
            else
              uart_state <= uart_conv_receive;
            end if;

          when uart_rx_receive =>
            if rx_valid_im = '1' and rx_package_count = FRAME_SIZE-1 then
              uart_state <= uart_rx_stop;
            else
              uart_state <= uart_rx_receive;
            end if;

          when uart_rx_stop =>
            if tx_send_im = '1' then
              uart_state <= uart_tx_send;
            else
              uart_state <= uart_rx_stop;
            end if;

          when uart_tx_send =>
            if tx_valid_im = '1' and tx_package_count = CONV_SIZE-1 then
              uart_state <= uart_idle;
            else
              uart_state <= uart_tx_send;
            end if;
        end case;

      end if;
    end if;
  end process uart_state_transitions;

  rx_package_counter : process (clk) is
  begin  -- process rx_package_counter
    if clk'event and clk = '1' then     -- rising clock edge
      if rst = '1' then                 -- synchronous reset (active high)
        rx_package_count <= 0;
      else
        if rx_valid_im = '1' and uart_state = uart_rx_receive then
          if rx_package_count = FRAME_SIZE-1 then
            rx_package_count <= 0;
          else
            rx_package_count <= rx_package_count + 1;
          end if;
        end if;
      end if;
    end if;
  end process rx_package_counter;
  
  rx_conv_counter : process (clk) is
  begin
    if clk'event and clk = '1' then
      if rst = '1' then
        rx_conv_count <= 0;
      else
        if rx_valid_im = '1' and uart_state = uart_conv_receive then
          if rx_conv_count = CONV_MAT_SIZE-1 then
            rx_conv_count <= 0;
          else
            rx_conv_count <= rx_conv_count + 1;
          end if;
        end if;
      end if;
    end if;
  end process;

  tx_package_counter : process (clk) is
  begin  -- process rx_package_counter
    if clk'event and clk = '1' then     -- rising clock edge
      if rst = '1' then                 -- synchronous reset (active high)
        tx_package_count <= 0;
      else
        if tx_valid_im = '1' then
          if tx_package_count = CONV_SIZE-1 then
            tx_package_count <= 0;
          else
            tx_package_count <= tx_package_count + 1;
          end if;
        end if;
      end if;
    end if;
  end process tx_package_counter;

  tx_send_trigger : process (clk) is
  begin  -- process tx_send_trigger
    if clk'event and clk = '1' then     -- rising clock edge
      if rst = '1' then                 -- synchronous reset (active high)
        tx_send_im <= '0';
      else
        if uart_state = uart_idle then
          tx_send_im <= '0';
        elsif uart_state = uart_conv_receive then
          tx_send_im <= '0';
        elsif uart_state = uart_rx_stop then
          if conv_done_im = '1' then
            tx_send_im <= '1';
          else
            tx_send_im <= '0';
          end if;
        elsif uart_state = uart_tx_send then
          if tx_valid_im = '1' then
            if tx_package_count = CONV_SIZE-1 then
              tx_send_im <= '0';
            else
              tx_send_im <= '1';
            end if;
          else
            tx_send_im <= '0';
          end if;
        else
          tx_send_im <= '0';
        end if;
      end if;
    end if;
  end process tx_send_trigger;

  tx_send_delay_process : process (clk) is
  begin
    if clk'event and clk = '1' then
      if rst = '1' then
        tx_send_delay <= '0';
      else
        tx_send_delay <= tx_send_im;
      end if;
    end if;
  end process tx_send_delay_process;
  
  conv_ram_en_process : process (clk) is
  begin
    if clk'event and clk = '1' then
      if rst = '1' then
        conv_ram_en <= '0';
      elsif uart_state = uart_idle and rx_in = '0' then
        conv_ram_en <= '1';
      elsif uart_state = uart_conv_receive then
        conv_ram_en <= '1';
      else
        conv_ram_en <= '0';
      end if;
    end if;
  end process;
  
--  conv_ram_en <= '1' when uart_state = uart_idle else
--                 '1' when uart_state = uart_conv_receive else
--                 '0';
  ---------------------------------------------------------------------------------------------------------------------
  -- MAPPING
  ---------------------------------------------------------------------------------------------------------------------

  block_ram_map : block_ram
    generic map (
      DATA_WIDTH => DATA_WIDTH,
      XRES       => XRES,
      YRES       => YRES)
    port map (
      clk              => clk,
      rst              => rst,
      rx_byte          => rx_byte_im,
      rx_valid         => rx_valid_im,
      conv_ram_en      => conv_ram_en,
      rx_conv_count    => rx_conv_count,
      rx_package_count => rx_package_count,
      tx_byte          => tx_byte_im,
      conv_done        => conv_done_im,
      tx_send          => tx_send_im,
      tx_package_count => tx_package_count,
      tx_test          => tx_test_im);

--  debouncer_map : debouncer
--    generic map (
--      N => 18)
--    port map (
--      button_in     => tx_send,
--      clk           => clk,
--      rst           => rst,
--      debounced_out => tx_send_delay);

  rx_map : rx
    generic map (
      DATA_WIDTH => DATA_WIDTH)
    port map (
      rx_in     => rx_in,
      main_clk  => clk,
      rst       => rst,
      rx_clk    => rx_clk_im,
      rx_clk_en => rx_clk_en_im,
      rx_byte   => rx_byte_im,
      rx_valid  => rx_valid_im,
      rx_fail   => rx_fail);

  rx_clk_map : rx_clk
    generic map (
      DIVISOR         => DIVISOR)
    port map (
      main_clk  => clk,
      rst       => rst,
      rx_clk_en => rx_clk_en_im,
      rx_clk    => rx_clk_im);

  tx_map : tx
    generic map (
      DATA_WIDTH      => DATA_WIDTH,
      DIVISOR         => DIVISOR)
    port map (
      tx_byte  => tx_byte_im,
      send     => tx_send_delay,
      main_clk => clk,
      rst      => rst,
      tx_out   => tx_out_im,
      tx_valid => tx_valid_im);
      
  tx_out <= tx_out_im;

--  tx_dly_process : process (clk) is
--  begin
--    if clk'event and clk = '1' then
--      tx_out <= tx_out_im;
--    end if;
--  end process
  
  tx_test <= tx_test_im;
---------------------------------------------------------------------------------------------------------------------
-- DEBUGGER MAPPING
---------------------------------------------------------------------------------------------------------------------
--  rx_dbg_process : process (clk) is
--  begin
--    if clk'event and clk = '1' then
--      rx_in_dbg_im <= rx_in;
--    end if;
--  end process;
--  rx_in_dbg <= rx_in_dbg_im;

--  rx_package_count_vec <= std_logic_vector(to_unsigned(rx_package_count, 3));

--  ila_0_map : ila_0
--    port map (
--      clk         => clk,
--      trig_in     => rx_in,
--      trig_in_ack => open,
--      probe0(0)   => rx_in,
--      probe1(0)   => rx_clk_im,
--      probe2(0)   => tx_test_im,
--      probe3(0)   => tx_out_im,
--      probe4      => rx_package_count_vec);

end architecture behavior;
