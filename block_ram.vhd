library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity block_ram is

  generic (
    DATA_WIDTH : natural := 8;
    XRES       : natural := 240;
    YRES       : natural := 240);

  port (
    clk              : in  std_logic;
    rst              : in  std_logic;
    rx_byte          : in  std_logic_vector (DATA_WIDTH-1 downto 0);
    rx_valid         : in  std_logic;
    conv_ram_en      : in  std_logic;
    rx_conv_count    : in  natural range 0 to 8;
    rx_package_count : in  natural range 0 to (XRES*YRES)-1;
    tx_byte          : out std_logic_vector (DATA_WIDTH-1 downto 0);
    conv_done        : out std_logic := '0';
    tx_send          : in  std_logic;
    tx_package_count : in  integer;
    tx_test          : out std_logic);

end entity block_ram;

architecture behavior of block_ram is

  constant FRAME_SIZE   : natural := XRES*YRES;
  constant KERNEL_SIZE  : natural := 3;  -- Convolution kernel size of one dimension
  constant KERNEL_TOTAL : natural := 9;
  constant KERNEL_BOUND : natural := ((KERNEL_SIZE+1)/2);
  constant CONV_SIZE    : natural := (XRES-KERNEL_BOUND)*(YRES-KERNEL_BOUND);  -- Convolved image size
  constant CONV_YLIMIT  : natural := FRAME_SIZE-(KERNEL_BOUND*XRES)-KERNEL_BOUND-1;
  constant CONV_XLIMIT  : natural := XRES-KERNEL_BOUND-1;
  constant MULT_WIDTH   : natural := 2*DATA_WIDTH+1;

  subtype data_type is std_logic_vector (DATA_WIDTH-1 downto 0);
  type ram_type is array (FRAME_SIZE-1 downto 0) of data_type;
  type conv_kernel is array (KERNEL_TOTAL-1 downto 0) of data_type;
  type mult_type is array (KERNEL_TOTAL-1 downto 0) of std_logic_vector(MULT_WIDTH-1 downto 0);
  type sum_type is array (KERNEL_TOTAL-2 downto 0) of std_logic_vector(MULT_WIDTH + integer(ceil(log2(real(KERNEL_TOTAL))))-1 downto 0);
  type conv_memory is array (CONV_SIZE-1 downto 0) of data_type;
  type ram_state_type is (ram_idle, ram_kernel_write, ram_data_write,
                          ram_conv_out);

  signal ram_state          : ram_state_type                    := ram_idle;
  signal ram_state_delay    : ram_state_type                    := ram_idle;
  signal ram_array          : ram_type;
  signal conv_array         : conv_kernel;
  signal mult_array         : mult_type;
  signal sum_out            : sum_type;
  signal result_array       : conv_memory;
  signal conv_count         : natural range 0 to CONV_YLIMIT    := 0;
  signal conv_line_count    : natural range 0 to CONV_XLIMIT    := 0;
  signal sum_count          : natural range 0 to KERNEL_TOTAL-2 := 0;
  signal sum_count_delay    : natural range 0 to KERNEL_TOTAL-2 := 0;
  signal sum_count_delay2   : natural range 0 to KERNEL_TOTAL-2 := 0;
  signal result_count       : natural range 0 to CONV_SIZE-1    := 0;
  signal result_count_delay : natural range 0 to CONV_SIZE-1    := 0;

  attribute use_dsp               : string;  -- To enable DSP modules inside FPGA
  attribute use_dsp of mult_array : signal is "yes";

begin  -- architecture behavior

  state_transitions : process (clk) is
  begin
    if clk'event and clk = '1' then
      if rst = '1' then
        ram_state <= ram_idle;
      else

        case ram_state is
          when ram_idle =>
            if conv_ram_en = '1' then
              ram_state <= ram_kernel_write;
            else
              ram_state <= ram_idle;
            end if;

          when ram_kernel_write =>
            if conv_ram_en = '0' then
              ram_state <= ram_data_write;
            else
              ram_state <= ram_kernel_write;
            end if;

          when ram_data_write =>
            if rx_package_count = FRAME_SIZE-1 and rx_valid = '1' then
              ram_state <= ram_conv_out;
            else
              ram_state <= ram_data_write;
            end if;

          when ram_conv_out =>
            if conv_count = CONV_YLIMIT and sum_count = KERNEL_TOTAL-2 then
              ram_state <= ram_idle;
            else
              ram_state <= ram_conv_out;
            end if;
        end case;

      end if;
    end if;
  end process;

  ram_state_delay_process : process (clk) is
  begin                                 -- This process was required for timing improvement
    if clk'event and clk = '1' then
      if rst = '1' then
        ram_state_delay <= ram_idle;
      else
        ram_state_delay <= ram_state;
      end if;
    end if;
  end process;

  conv_kernel_write : process (clk) is
  begin
    if clk'event and clk = '1' then
      if conv_ram_en = '1' then
        if rx_valid = '1' then
          conv_array(rx_conv_count) <= rx_byte;
        end if;
      end if;
    end if;
  end process;

  ram_process : process (clk) is
  begin  -- process read_write
    if clk'event and clk = '1' then     -- rising clock edge
      if conv_ram_en = '0' then
        if rx_valid = '1' then
          ram_array(rx_package_count) <= rx_byte;
        end if;
      end if;
    end if;
  end process ram_process;

  conv_counter_process : process (clk) is
  begin
    if clk'event and clk = '1' then
      if rst = '1' then
        conv_count      <= 0;
        conv_line_count <= 0;
        sum_count       <= 0;
        result_count    <= 0;
      else
        if ram_state = ram_conv_out then
          if sum_count = KERNEL_TOTAL-2 then
            sum_count <= 0;
            if conv_count = CONV_YLIMIT then
              conv_count      <= 0;
              conv_line_count <= 0;
              result_count    <= 0;
            else
              if conv_line_count = CONV_XLIMIT then
                conv_count      <= conv_count + KERNEL_SIZE;
                conv_line_count <= 0;
                result_count    <= result_count + 1;
              else
                conv_line_count <= conv_line_count + 1;
                conv_count      <= conv_count + 1;
                result_count    <= result_count + 1;
              end if;
            end if;
          else
            sum_count <= sum_count + 1;
          end if;
        else
          conv_count      <= 0;
          conv_line_count <= 0;
          sum_count       <= 0;
          result_count    <= 0;
        end if;
      end if;
    end if;
  end process;

  sum_count_delay_process : process (clk) is
  begin
    if clk'event and clk = '1' then
      if rst = '1' then
        sum_count_delay  <= 0;
        sum_count_delay2 <= 0;
      else
        sum_count_delay  <= sum_count;
        sum_count_delay2 <= sum_count_delay;
      end if;
    end if;
  end process;

  result_count_delay_process : process (clk) is
  begin
    if clk'event and clk = '1' then
      if rst = '1' then
        result_count_delay <= 0;
      elsif ram_state = ram_conv_out then
        if sum_count = KERNEL_TOTAL-2 then
          result_count_delay <= result_count;
        end if;
      elsif ram_state = ram_kernel_write then
        result_count_delay <= 0;
      elsif ram_state = ram_data_write then
        result_count_delay <= 0;
      end if;
    end if;
  end process;

  conv_done_process : process (clk) is
  begin
    if clk'event and clk = '1' then
      if rst = '1' then
        conv_done <= '0';
      else
        if sum_count = KERNEL_TOTAL-2 then
          if conv_count = CONV_YLIMIT then
            conv_done <= '1';
          else
            conv_done <= '0';
          end if;
        else
          conv_done <= '0';
        end if;
      end if;
    end if;
  end process;

  multiplier_process : process (clk) is
  begin
    if clk'event and clk = '1' then
      if ram_state = ram_conv_out and sum_count = 0 then
        for i in 0 to (KERNEL_SIZE-1) loop
          for k in 0 to (KERNEL_SIZE-1) loop
            mult_array(i*(KERNEL_SIZE)+k) <= std_logic_vector(signed('0' & ram_array(i*XRES + conv_count + k))
                                                              * signed(conv_array(i*(KERNEL_SIZE)+k)));
          end loop;
        end loop;
      end if;
    end if;
  end process;

  accumulator_process : process (clk) is
  begin
    if clk'event and clk = '1' then
      if (ram_state_delay = ram_conv_out) then
        for j in 0 to (KERNEL_TOTAL-2) loop
          if j = 0 then
            sum_out(j) <= std_logic_vector(resize(signed(mult_array(j)), sum_out(0)'length) +
                                           resize(signed(mult_array(j+1)), sum_out(0)'length));
          else
            sum_out(j) <= std_logic_vector(resize(signed(sum_out(j-1)), sum_out(0)'length) +
                                           resize(signed(mult_array(j+1)), sum_out(0)'length));
          end if;
        end loop;
      end if;
    end if;
  end process;

  conv_out_process : process (clk) is
  begin
    if clk'event and clk = '1' then
      if sum_count_delay2 = KERNEL_TOTAL-2 then
        result_array(result_count_delay) <= sum_out(KERNEL_TOTAL-2)(13 downto 6);
      end if;
      if tx_send = '1' then
        tx_byte <= result_array(tx_package_count);
      end if;
    end if;
  end process;

  tx_test <= '1' when ram_state = ram_conv_out else
             '0';

end architecture behavior;
