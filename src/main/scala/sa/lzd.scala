package sa

import chisel3._
import chisel3.util._
// _root_ disambiguates from package chisel3.util.circt if user imports chisel3.util._
import _root_.circt.stage.ChiselStage


class LZD(lzdWidth:Int) extends Module {
	val lzdOutputWidth = log2Ceil(lzdWidth)
	val io = FlatIO(new Bundle {
		// input
		val data = Input(UInt(lzdWidth.W))
		// output
		val zcnt = Output(UInt(lzdOutputWidth.W))
		val data_o = Output(Bool())
	})
////////////////////////////////////////////////
	def lzd_node_btm(left_i:Bool, right_i:Bool) :(Bool, Bool) = {
		val data_o = left_i || right_i // is there any one?
		val cnt_o = left_i ^ data_o // the one is the left or right
		(data_o, cnt_o)
	}
	
	

	if(lzdOutputWidth==1){
		val (data_o, cnt_o) = lzd_node_btm(io.data(1), io.data(0))
		io.data_o := data_o
		io.zcnt := cnt_o.asUInt
	} else {
		val left = Wire(Bool())
		val right = Wire(Bool())
		val cnts = Seq.fill(2)(Wire(UInt((lzdOutputWidth-1).W)))

		val subNodes = Seq.fill(2)(Module(new LZD(lzdWidth/2)))
		subNodes(1).io.data := io.data(lzdWidth-1, lzdWidth/2)
		subNodes(0).io.data := io.data(lzdWidth/2-1, 0)
		left := subNodes(1).io.data_o // 高位半部分是否包含1
		right := subNodes(0).io.data_o // 低位半部分是否包含1
		cnts(1) := subNodes(1).io.zcnt // 高位半部分的leading zero count
		cnts(0) := subNodes(0).io.zcnt // 低位半部分的leading zero count

		val (data_o, cnt_o) = lzd_node_btm(left, right) // 根据低一层level的left和right计算本层level的输出
		io.data_o := data_o
		val zcnt_tmp = Mux(cnt_o, cnts(0), cnts(1)) 
		// cnt_o 代表了本level所对应的bit位是否为1,例如当输入为4'b0010时, left为0，因为高2bit是全0, right为1，因为不全为0; cnts(1)为1‘b0, cnts(0)也为1'b0;
		// left和right组成2'b01，此时 lzd_node_btm 的输出 cnt_o 为1, 代表当前层级所对应的结果的bit位的值应该为1；即结果的[1]位为1；
		// cnt_o 为1时, zcnt_tmp为cnts(0)；所以此时所对应的level的bit位应为0，即结果的[0]位为0；
		// 总的结果即为2'b10，即输入4'b0010的leading zero count为2；
		io.zcnt := Cat(cnt_o.asUInt, zcnt_tmp)
	}
}

class LZD_Wrapper(inputWidth:Int) extends Module {
	val exp2Width = 1 << log2Ceil(inputWidth)
	val io = FlatIO(new Bundle {
		// input
		val data = Input(UInt(inputWidth.W))
		// output
		val zcnt = Output(UInt(log2Ceil(exp2Width).W))
		val allZero = Output(Bool())
	})	
	val lzdModule = Module(new LZD(exp2Width))
	lzdModule.io.data := Cat(0.U((exp2Width - inputWidth).W), io.data)
	io.allZero := ~lzdModule.io.data_o
	io.zcnt := lzdModule.io.zcnt
}

object LZD extends App {
  ChiselStage.emitSystemVerilogFile(
    new LZD_Wrapper(4),
    firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info", "-default-layer-specialization=enable")
  )
}