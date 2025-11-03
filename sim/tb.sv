module tb();
    reg [3:0] input_data;
    wire [1:0] zcnt;
    wire allZero;


LZD_Wrapper u_lzd(
  .clock(clk),
  .reset(rst),
  .data(input_data),
  .zcnt(zcnt),
  .allZero(allZero)
);


  clk_gen u_clk_gen (
             /*AUTOINST*/
		     // Outputs
		     .clk		(clk));

  rst_gen u_rst_gen (
            /*AUTOINST*/
		     // Outputs
		     .rst_n		(),			 // Templated
		     .rst_p		(rst));			 // Templated

initial begin
    input_data =4'd0;
    wait(~rst);
    @(posedge clk);
    input_data = 4'b0010;
    #100;
    @(posedge clk);
    input_data = 4'b0101;
    $finish;
end
dump u_dump();
endmodule
