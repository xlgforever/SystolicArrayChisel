module clk_gen (/*AUTOARG*/
   // Outputs
   clk
   );

output    clk      ;
// output    clk_160  ;
// output    clk_320  ;

reg       clk      ;
// reg       clk_160  ;
// reg       clk_320  ;

initial begin
  clk =0; 
  forever  #10  clk =~clk ;
end

// initial begin
//   clk_160 = 0 ;
//   forever  #3.125  clk_160 =~clk_160 ;  
// end

// initial begin
//   clk_320 = 0 ;
//   forever  #1.5625  clk_320 =~clk_320 ;  
// end




endmodule



