module tb();

    wire rst;
    wire clk;
/*autowire*/
// Beginning of automatic wires (for undeclared instantiated-module outputs)
wire [15:0]		io_out;			// From u_FPMultiply of FPMultiply.v
wire			io_valid_out;		// From u_FPMultiply of FPMultiply.v
// End of automatics
/*autoreginput*/
// Beginning of automatic reg inputs (for undeclared instantiated-module inputs)
reg [15:0]		io_a;			// To u_FPMultiply of FPMultiply.v
reg [15:0]		io_b;			// To u_FPMultiply of FPMultiply.v
reg			io_valid_in;		// To u_FPMultiply of FPMultiply.v
// End of automatics

//LZD_Wrapper u_lzd(
//  .clock(clk),
//  .reset(rst),
//  .data(input_data),
//  .zcnt(zcnt),
//  .allZero(allZero)
//);

/* FPMultiply AUTO_TEMPLATE (
   .clock(clk),
   .reset(rst),
);
*/

FPMultiply u_FPMultiply(/*autoinst*/
			// Outputs
			.io_out		(io_out[15:0]),
			.io_valid_out	(io_valid_out),
			// Inputs
			.clock		(clk),			 // Templated
			.reset		(rst),			 // Templated
			.io_a		(io_a[15:0]),
			.io_b		(io_b[15:0]),
			.io_valid_in	(io_valid_in));


  clk_gen u_clk_gen (
		     // Outputs
		     .clk		(clk));

  rst_gen u_rst_gen (
		     // Outputs
		     .rst_n		(),
		     .rst_p		(rst));

initial begin
    io_a = 'd0;
    io_b = 'd0;
    io_valid_in = 'd0;
    wait(~rst);
    @(posedge clk);
    io_a = 16'h3e00;
    io_b = 16'h3e00;
    io_valid_in = 'd1;
    wait(io_valid_out);
    @(posedge clk);
    io_valid_in = 'd0;
    $finish;
end
dump u_dump();
endmodule



// Local Variables:
// verilog-auto-inst-param-value:t
// verilog-library-directories:("." "../"  )
// End:
