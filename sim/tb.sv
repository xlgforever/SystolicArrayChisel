module tb();

    wire rst;
    wire clk;
    localparam WIDTH = 16;
/*autowire*/
// Beginning of automatic wires (for undeclared instantiated-module outputs)
wire [15:0]		d;			// From u_fp_ma of fp_ma.v
wire			valid_out;		// From u_fp_ma of fp_ma.v
// End of automatics
/*autoreginput*/
// Beginning of automatic reg inputs (for undeclared instantiated-module inputs)
reg [15:0]		a;			// To u_fp_ma of fp_ma.v
reg [15:0]		b;			// To u_fp_ma of fp_ma.v
reg [15:0]		c;			// To u_fp_ma of fp_ma.v
reg			valid_in;		// To u_fp_ma of fp_ma.v
// End of automatics

//LZD_Wrapper u_lzd(
//  .clock(clk),
//  .reset(rst),
//  .data(input_data),
//  .zcnt(zcnt),
//  .allZero(allZero)
//);

/* fp_ma AUTO_TEMPLATE (
   .clock(clk),
   .reset(rst),
);
*/

fp_ma u_fp_ma(/*autoinst*/
	      // Outputs
	      .d			(d[15:0]),
	      .valid_out		(valid_out),
	      // Inputs
	      .clock			(clk),			 // Templated
	      .reset			(rst),			 // Templated
	      .a			(a[15:0]),
	      .b			(b[15:0]),
	      .c			(c[15:0]),
	      .valid_in			(valid_in));


  clk_gen u_clk_gen (
		     // Outputs
		     .clk		(clk));

  rst_gen u_rst_gen (
		     // Outputs
		     .rst_n		(),
		     .rst_p		(rst));
  /* tx_sys AUTO_TEMPLATE(
  .clk(clk),
  .reset(rst),
  .fp16_a(a[]),
  .fp16_b(b[]),
  .fp16_c(c[]),
  .in_valid(valid_in),
  .fp16_d(d[]),
  .out_valid(valid_out),
  );
  */
// tx_sys u_tx_sys(/*autoinst*/
//		 // Outputs
//		 .fp16_a		(a[WIDTH-1:0]),
//		 .fp16_b		(b[WIDTH-1:0]),
//		 .fp16_c		(c[WIDTH-1:0]),
//		 .in_valid		(valid_in),
//		 // Inputs
//		 .clk			(clk),
//		 .reset			(rst),
//		 .fp16_d		(d[WIDTH-1:0]),
//		 .out_valid		(valid_out));

 
 
initial begin
    a = 'd0;
    b = 'd0;
    c = 'd0;
    valid_in = 'd0;
    wait(~rst);
    @(posedge clk);
    a = 16'h3909;
    b = 16'h9e13;
    c = 16'h3546;
    valid_in = 'd1;
    wait(valid_out);
    @(posedge clk);
    valid_in = 'd0;
    $finish;
end

dump u_dump();


endmodule



// Local Variables:
// verilog-auto-inst-param-value:t
// verilog-library-directories:("." "../"  )
// End:
