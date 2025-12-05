module dump();





`ifdef  DUMP
initial begin

//#44_000_000 

//wait (tb.finance_v2_top.u_own_top.u_sub_top.u_mirp_out_top.u_mdqp_mem_ctrl.mirp_sn_enable);

$vcdpluson;
  $vcdplusmemon;
  $vcdplusglitchon;
  $vcdplusflush;
//#DUMP_ED ;
//$finish ;
//$vcdplusoff;


$fsdbAutoSwitchDumpfile(1000,"mac.fsdb",100);
$fsdbDumpoff;

$fsdbDumpSVA;
$fsdbDumpMDA;
$fsdbDumpMem;

//$fsdbDumpvars(0,tb.tx_mon);
//$fsdbDumpvars(1,tb.wrapper_filter_rx_top.filter_rx_top);
//$fsdbDumpvars(1,tb.wrapper_filter_rx_top.filter_rx_top.filter_rx_fsm);
//$fsdbDumpvars(0,tb_Vortex);
$fsdbDumpvars(0,tb,"+all");
$fsdbDumpon;
//wait ( tb.tx_sys.tcp_frame_cnt[31:0]==32'd15000 );
//wait ( tb_uart_axi.u_uart_driver.read_finish==1);
//#1000_000_000 ;
//#1000_00000;


//$finish ;

end

`endif






endmodule






