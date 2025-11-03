module rst_gen(/*AUTOARG*/
   // Outputs
   rst_n, rst_p
   );

output   rst_n, rst_p  ;

reg      rst_n, rst_p  ;

initial begin
  rst_n = 1'b0 ;
  #1000
  rst_n = 1'b1 ;  
end

initial begin
  rst_p = 1'b1;
  #100
  rst_p = 1'b1;
  #1000
  rst_p = 1'b1;
  #100
  rst_p = 1'b0;
  
end


endmodule


