MODE=LZD
DUMP=DUMP

VERDI_PATH = $(VERDI_HOME)

run:
	echo "\`define  $(DUMP)       "  >   define.sv
	echo "\`define  $(MODE)       "  >>  define.sv
	rm -rf all.vc
	find .. -type f -name "*.sv" > all.vc
	echo "./tb.sv"  >> all.vc
	echo "./dump.v" >> all.vc
	echo "./clk_gen.v" >> all.vc
	echo "./rst_gen.v" >> all.vc

	vcs -full64   -LDFLAGS -Wl,--no-as-needed -lca  -kdb   -timescale=1ns/1ps  -P  $(VERDI_PATH)/share/PLI/VCS/LINUX64/novas.tab  $(VERDI_PATH)/share/PLI/VCS/LINUX64/pli.a +vcs+lic+wait   -sverilog +verilog2001ext+.v    -debug_all   -f all.vc -top tb    -cm line+cond+fsm+tgl -cm_dir cov_result/my_cov_info_$(MODE)   -assert svaext    -l vcs_com.log
	./simv -debug_all    -cm line+cond+fsm+tgl -cm_dir cov_result/my_cov_info_$(MODE)     -l vcs_sim_$(MODE).log 
	

clean:
	rm -rf simv*
	rm -rf csrc*
	rm -rf verdiLog  cov_result novas* mac*  *.log *.h    ucli.key  

emacs:
	emacs   --batch   tb.sv             -f verilog-auto     -f save-buffer 	

verdi_vc:
	##source /home/haorui/.bashrc_2015
	verdi -sv -f tb.vc -top tb&
verdi:
	verdi -elab simv.daidir/kdb &
	







