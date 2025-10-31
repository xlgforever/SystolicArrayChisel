package sa

import chisel3._
import chisel3.util._
// _root_ disambiguates from package chisel3.util.circt if user imports chisel3.util._
import _root_.circt.stage.ChiselStage


class LZD(lzdWidth:Int) extends Module {
	val lzdOutputWidth = log2Ceil(lzdWidth)

	val io = FlatIO(new Bundle {
		val data = Input(UInt(lzdWidth.W))

		val zcnt = Output(UInt(lzdOutputWidth.W))
		val allZero = Output(Bool())
	})
////////////////////////////////////////////////
	def lzd_node_btm(left_i:Bool, right_i:Bool) :(Bool, Bool) = {
		val data_o = left_i || right_i // is there any one?
		val cnt_o = left_i ^ data_o // the one is the left or right
		(data_o, cnt_o)
	}

	if(lzdOutputWidth==1){
		val (data_o, cnt_o) = lzd_node_btm(io.data(1), io.data(0))
		io.allZero := ~data_o
		io.zcnt := cnt_o.asUInt
	} else {
		
	}
}
