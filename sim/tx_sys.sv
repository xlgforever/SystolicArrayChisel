module  tx_sys (

clk               ,
reset             ,
fp16_a            ,
fp16_b            ,
fp16_c            ,
in_valid          ,

fp16_d            ,
out_valid         

);


localparam int unsigned EXP_BITS = 5;
localparam int unsigned MAN_BITS = 10;
localparam int unsigned WIDTH    = EXP_BITS+MAN_BITS+1;
localparam int unsigned BIAS     = 15;

input                clk               ;
input                reset             ;

output    [ WIDTH-1:0]    fp16_a            ;
output    [ WIDTH-1:0]    fp16_b            ;
output    [ WIDTH-1:0]    fp16_c            ;
output               in_valid          ;

input     [ WIDTH-1:0]    fp16_d            ;
input                out_valid         ;

////////////////////////////////////////////////////////////////////////////////////

reg       [ WIDTH-1:0]    fp16_a            ;
reg       [ WIDTH-1:0]    fp16_b            ;
reg       [ WIDTH-1:0]    fp16_c            ;
reg                  in_valid          ;


bit [WIDTH-1:0] c_queue[$];

reg  [WIDTH-1:0]     fp16_a_read  ;
reg  [WIDTH-1:0]     fp16_b_read  ;
reg  [WIDTH-1:0]     fp16_c_read  ;
reg  [WIDTH-1:0]     fp16_d_read  ;

integer fp;


initial begin 
	fp = $fopen("abcd.txt","r");
	$fscanf(fp,"%h %h %h %h ", fp16_a_read,fp16_b_read,fp16_c_read,fp16_d_read   );
	c_queue.push_front(  fp16_d_read  );	
end
////////////////////////////////////////////////////////////////////////////////////
always @ (posedge clk   or posedge    reset  )
begin
	if ( reset ) begin 
		in_valid <= 'd0 ;
	end
	else begin 
		in_valid <= 'd1 ;			
	end
end


always @ (posedge clk   or posedge    reset  )
begin
	if ( reset ) begin 
		fp16_a   <= 'd0 ;
		fp16_b   <= 'd0 ;
		fp16_c   <= 'd0 ;
	end
	else  begin 
    		//fp16_a <=  16'h00 ;			
		//fp16_b <=  16'h7e00 ;
		//fp16_c <=  0 ;

		fp16_a <=  fp16_a_read ;			
		fp16_b <=  fp16_b_read ;
		fp16_c <=  fp16_c_read ;

		#1  	$fscanf(fp,"%h %h %h %h ", fp16_a_read,fp16_b_read,fp16_c_read,fp16_d_read   );
			c_queue.push_front(  fp16_d_read  );	

     		 if ($feof(fp)) begin
			$display("compare done !!!!");
			#1
			$finish ;
       	         end
							
	end
end


reg    [WIDTH-1:0]   reference_d  ,  rtl_d ;
reg             compare_flag          ;

always @ (posedge clk   or posedge    reset  )
begin
	if ( reset ) begin 
		compare_flag <= 'd0 ;
	end
	else if (out_valid)  begin 
		reference_d   <=  c_queue.pop_back;
	  	rtl_d         <=  fp16_d          ;	
		compare_flag  <= 'd1 ;							
	end
end

reg   [31:0]  icnt  ;
reg   [31:0]  ocnt  ;

wire  [WIDTH:0]   result_sub     =   ((reference_d[WIDTH-2:0]=='d0)&&(rtl_d[WIDTH-2:0]=='d0))  ?   'd0  :    {1'b0,reference_d}   -  {1'b0,rtl_d} ;
wire  [WIDTH:0]   result_sub_abs = result_sub[WIDTH]  ?   ~result_sub+1 :   result_sub ;

reg      error_flag   ;
always @ (posedge clk   or posedge    reset  )
begin
	if ( reset ) begin 
		ocnt <= 'd0 ;
		error_flag <= 'd0 ;
	end
	else if (  compare_flag  )  begin
		ocnt <= ocnt + 'd1 ;		
		if (  result_sub_abs !=0 )  begin 
			$display("%m:the result:%0d  compare fail!!!! reference and rtl  is %h --  %h \n", ocnt, reference_d , rtl_d );
			error_flag <= 'd1 ;		
		end
		else error_flag <= 'd0 ;	
	end
end


always @ (posedge clk   or posedge    reset  )
begin
	if ( reset ) begin 
		icnt <= 'd0 ;
	end
	else if (  in_valid  )  begin
		icnt <= icnt + 'd1 ;		
	end
end

initial begin
    wait(icnt=='d1);
    repeat (9) @(posedge clk);
    $finish;
end

endmodule

