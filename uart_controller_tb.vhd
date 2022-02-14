library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.uart_pck.all;

entity uart_controller_tb is
  generic (
    FRAME_SIZE      : natural := 3600;      -- x_resolution * y_resolution (e.g., 128x128) + kernel size
    DATA_WIDTH      : natural := 8;
    DIVISOR         : natural := 217);  -- DIVISOR = 100,000,000 / BAUD_RATE
end entity uart_controller_tb;

architecture behavior of uart_controller_tb is

  signal clk_tb    : std_logic := '0';
  signal rst_tb    : std_logic := '0';
  signal rx_in_tb  : std_logic := '1';
  signal tx_out_tb : std_logic;
  signal rx_fail   : std_logic;
  
  signal count_tb  : std_logic_vector(DATA_WIDTH-1 downto 0) := "11111100";
  signal count_package : std_logic_vector(DATA_WIDTH+1 downto 0) := "1000000000";
  type kernel_array is array (8 downto 0) of std_logic_vector(DATA_WIDTH+1 downto 0);
  signal kernel_data : kernel_array;
  signal process_en : std_logic := '0';
  signal kernel_en  : std_logic := '0';
begin  -- architecture behavior
  
  kernel_data(0) <= "1000000000" after 0 ns;
  kernel_data(1) <= "1000000000" after 0 ns;
  kernel_data(2) <= "1000000000" after 0 ns;
  kernel_data(3) <= "1000000000" after 0 ns;
  kernel_data(4) <= "1010000000" after 0 ns;
  kernel_data(5) <= "1000000000" after 0 ns;
  kernel_data(6) <= "1000000000" after 0 ns; 
  kernel_data(7) <= "1000000000" after 0 ns; 
  kernel_data(8) <= "1110000000" after 0 ns;

  tb_clk_gen : process is
  begin  -- process tb_clk_gen
    clk_tb <= not clk_tb;
    wait for 5 ns;
  end process tb_clk_gen;

  rst_tb <= '0' after 0 us;

  kernel_en  <= '1' after 15 us,
                '0' after 345.3 us;
  process_en <= '0' after 0 us,
                '1' after 350 us;
                
  count_package <= '1' & count_tb & '0';
  
  rx_in_gen : process is
    variable i            : natural range 0 to FRAME_SIZE*10 := 0;
    variable bit_duration : time                   := 2.17 us;
    variable bit_count    : natural range 0 to (count_package'length)-1   := 0;
    variable pack_count   : natural range 0 to 8 := 0;
  begin
    if process_en = '1' then
      if i <= (FRAME_SIZE*10)-2 then
        rx_in_tb  <= count_package(bit_count);
        wait for bit_duration;
        if bit_count = (count_package'length)-1 then
          bit_count := 0;
          count_tb  <= std_logic_vector(signed(count_tb) + 1);
        else
          bit_count := bit_count + 1;
        end if;
        i         := i + 1;
      elsif i > (FRAME_SIZE*10)-2 then
        rx_in_tb <= '1';
        wait for bit_duration;
      end if;
    elsif kernel_en = '1' then
      if i <= (9*10)-2 then
        rx_in_tb  <= kernel_data(pack_count)(bit_count);
        wait for bit_duration;
        if bit_count = (count_package'length)-1 then
          bit_count := 0;
          pack_count := pack_count + 1;
          count_tb  <= std_logic_vector(signed(count_tb) + 1);
        else
          bit_count := bit_count + 1;
        end if;
        i:= i + 1;
      elsif i > (9*10)-2 then
        rx_in_tb <= '1';
        wait for bit_duration;
      end if;
    else
      count_tb <= (others => '0');
      pack_count := 0;
      i := 0;
      bit_count := 0;
      wait for 1 ns;
    end if;
  end process rx_in_gen;

  tb_uart_map : uart_controller
    generic map (
      XRES            => 60,
      YRES            => 60,
      DATA_WIDTH      => DATA_WIDTH,
      DIVISOR         => DIVISOR)
    port map (
      clk    => clk_tb,
      rst    => rst_tb,
      rx_in  => rx_in_tb,
      tx_out => tx_out_tb,
      rx_fail=> rx_fail);

end architecture behavior;
