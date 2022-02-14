library ieee;
use ieee.std_logic_1164.all;

package uart_pck is

  component block_ram is
    generic (
      DATA_WIDTH : natural := 8;
      XRES       : natural := 320;
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
      conv_done        : out std_logic;
      tx_send          : in  std_logic;
      tx_package_count : in  natural range 0 to (XRES*YRES)-1;
      tx_test          : out std_logic);
  end component block_ram;

  component rx is
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
      rx_fail   : out std_logic                                := '0');
  end component rx;

  component rx_clk is
    generic (
      DIVISOR         : natural := 217);
    port (
      main_clk  : in  std_logic;        -- clock
      rst       : in  std_logic;        -- reset(board buttons are normally low)
      rx_clk_en : in  std_logic;        -- enable input
      rx_clk    : out std_logic         -- 1 clk spike at the middle of bit
      );
  end component rx_clk;

  component tx is
    generic (
      DATA_WIDTH      : natural := 8;
      DIVISOR         : natural := 217);
    port (
      tx_byte  : in  std_logic_vector (DATA_WIDTH-1 downto 0);
      send     : in  std_logic;
      main_clk : in  std_logic;
      rst      : in  std_logic;
      tx_out   : out std_logic := '1';
      tx_valid : out std_logic := '0');
  end component tx;

  component uart_controller is
    generic (
      XRES            : natural := 320;  -- x_resolution
      YRES            : natural := 240;  -- y_resolution
      DATA_WIDTH      : natural := 8;
      DIVISOR         : natural := 217);     -- DIVISOR = 100,000,000 / (SAMPLES PER BIT x BAUD_RATE)
    port (
      clk       : in  std_logic;
      rst       : in  std_logic;
      rx_in     : in  std_logic;
--    tx_byte : in  std_logic_vector (DATA_WIDTH-1 downto 0);
--    tx_send : in  std_logic;
      tx_out    : out std_logic := '1';
      rx_fail   : out std_logic := '0';
      tx_test   : out std_logic := '0');
--      rx_in_dbg : out std_logic);
  end component uart_controller;

end package uart_pck;
