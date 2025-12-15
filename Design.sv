//============================================================
// Async FIFO RTL (Gray Code, 2-FF CDC)
//============================================================
module async_fifo #(
  parameter DATA_WIDTH = 8,
  parameter DEPTH      = 16,
  parameter ADDR_WIDTH = $clog2(DEPTH)
)(
  // Write domain
  input  logic                  w_clk,
  input  logic                  w_rst_n,
  input  logic                  w_en,
  input  logic [DATA_WIDTH-1:0] w_data,
  output logic                  full,

  // Read domain
  input  logic                  r_clk,
  input  logic                  r_rst_n,
  input  logic                  r_en,
  output logic [DATA_WIDTH-1:0] r_data,
  output logic                  empty
);

  logic [DATA_WIDTH-1:0] mem [0:DEPTH-1];

  logic [ADDR_WIDTH:0] w_bin, w_gray, w_bin_next, w_gray_next;
  logic [ADDR_WIDTH:0] r_bin, r_gray, r_bin_next, r_gray_next;

  logic [ADDR_WIDTH:0] r_gray_sync1, r_gray_sync2;
  logic [ADDR_WIDTH:0] w_gray_sync1, w_gray_sync2;

  // Binary → Gray
  assign w_bin_next  = w_bin + (w_en && !full);
  assign w_gray_next = (w_bin_next >> 1) ^ w_bin_next;

  assign r_bin_next  = r_bin + (r_en && !empty);
  assign r_gray_next = (r_bin_next >> 1) ^ r_bin_next;

  // Write logic
  always_ff @(posedge w_clk or negedge w_rst_n) begin
    if (!w_rst_n) begin
      w_bin <= '0; w_gray <= '0;
    end else begin
      w_bin <= w_bin_next;
      w_gray <= w_gray_next;
      if (w_en && !full)
        mem[w_bin[ADDR_WIDTH-1:0]] <= w_data;
    end
  end

  // Read logic
  always_ff @(posedge r_clk or negedge r_rst_n) begin
    if (!r_rst_n) begin
      r_bin <= '0; r_gray <= '0; r_data <= '0;
    end else begin
      r_bin <= r_bin_next;
      r_gray <= r_gray_next;
      if (r_en && !empty)
        r_data <= mem[r_bin[ADDR_WIDTH-1:0]];
    end
  end

  // CDC sync
  always_ff @(posedge w_clk or negedge w_rst_n)
    if (!w_rst_n) {r_gray_sync2,r_gray_sync1} <= 0;
    else begin
      r_gray_sync1 <= r_gray;
      r_gray_sync2 <= r_gray_sync1;
    end

  always_ff @(posedge r_clk or negedge r_rst_n)
    if (!r_rst_n) {w_gray_sync2,w_gray_sync1} <= 0;
    else begin
      w_gray_sync1 <= w_gray;
      w_gray_sync2 <= w_gray_sync1;
    end

  assign full  = (w_gray_next ==
                 {~r_gray_sync2[ADDR_WIDTH:ADDR_WIDTH-1],
                   r_gray_sync2[ADDR_WIDTH-2:0]});

  assign empty = (r_gray_next == w_gray_sync2);

endmodule
