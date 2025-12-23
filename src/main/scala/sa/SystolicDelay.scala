package sa

import chisel3._
import chisel3.util._
// _root_ disambiguates from package chisel3.util.circt if user imports chisel3.util._
import _root_.circt.stage.ChiselStage
import svsim.CommonCompilationSettings.Timescale.Unit.us

class SystolicDelay(dataNum:Int, dataWidth: Int, delayCycles:Int, reverse:Boolean) extends Module {
  val io = FlatIO(new Bundle {
	val dataIn = Input(Vec(dataNum, UInt(dataWidth.W)))
	val dataOut = Output(Vec(dataNum, UInt(dataWidth.W)))
  })

  // 如果 reverse 为 false，则dataIn的第0个元素延迟delayCycles*0拍，第1个元素延迟delayCycles*1拍，依此类推；并且dataWidth为1时，启用复位值为0的ShiftRegister
  for(i <- 0 until dataNum){
	if(!reverse){
	  if(dataWidth == 1){
		io.dataOut(i) := ShiftRegister(io.dataIn(i), delayCycles*i, 0.U, true.B)
	  } else {
		io.dataOut(i) := ShiftRegister(io.dataIn(i), delayCycles*i)
	  }
	} else {
	  // 如果 reverse 为 true，则dataIn的第0个元素延迟delayCycles*(dataNum-1-0)拍，第1个元素延迟delayCycles*(dataNum-1-1)拍，依此类推；并且dataWidth为1时，启用复位值为0的ShiftRegister
	  if(dataWidth == 1){
		io.dataOut(i) := ShiftRegister(io.dataIn(i), delayCycles*(dataNum-1-i), 0.U, true.B)
	  } else {
		io.dataOut(i) := ShiftRegister(io.dataIn(i), delayCycles*(dataNum-1-i))
	  }
	}
  }
}

class Transpose(rows:Int, dataWidth: Int) extends Module {
  val columns = rows
  val io = FlatIO(new Bundle {
	val dataIn = Input(Vec(columns, UInt(dataWidth.W)))
	val dataOut = Output(Vec(rows, UInt(dataWidth.W)))
	
	val validIn = Input(Vec(columns, Bool()))
	val validOut = Output(Bool())
  })

	val a_tmp = Reg(Vec(rows, Vec(columns, UInt(dataWidth.W)))) //寄存器阵列
	val wcnt = RegInit(VecInit(Seq.fill(rows)(0.U(log2Ceil(columns).W)))) //写计数器
	val a_tmp_vld = RegInit(VecInit(Seq.fill(columns)(false.B))) //寄存器阵列有效标志

	for(i <- 0 until rows){
	  wcnt(i) := Mux(io.validIn(i), wcnt(i) + 1.U, wcnt(i)) // 第i列来一个数据，就计数1次
	  a_tmp_vld(i) := io.validIn(i) && wcnt(i).andR // 当计数器全为1时，说明该列数据已经写满，可以输出

	  a_tmp(i)(wcnt(i)) := Mux(io.validIn(i), io.dataIn(i), a_tmp(i)(wcnt(i))) // 写数据到寄存器阵列
	}

	io.validOut := RegNext(a_tmp_vld.reduce(_ || _), false.B)  // 只要有一列数据写满，就可以输出
	
	// a_tmp_vld中哪个有效，就输出对应的a_tmp(i)
	io.dataOut := RegNext(Mux1H(a_tmp_vld, a_tmp))
}

class SystolicDelayForInput(cfg: PEConfig, rows:Int=32) extends Module {
	val columns = rows
  val io = FlatIO(new Bundle {
	val sdtIn = Input(Vec(rows, new PESubBundle(cfg)))
	val sdtOut = Output(Vec(rows, new PESubBundle(cfg)))
  })
	val a_delay = Module(new SystolicDelay(rows, cfg.PEInWidth, cfg.PEMACDelay+1, false))
	val b_delay = Module(new SystolicDelay(rows, cfg.PEOutWidth, 1, false))
	val d_delay = Module(new SystolicDelay(rows, cfg.PEInWidth, 1, false))

	val last_delay = Module (new SystolicDelay(rows, 1, 1, false))
	val valid_delay = Module (new SystolicDelay(rows, 1, 1, false))
	val propagate_delay = Module (new SystolicDelay(rows, 1, 1, false))

	a_delay.io.dataIn := VecInit(io.sdtIn.map(_.PE_a))
	b_delay.io.dataIn := VecInit(io.sdtIn.map(_.PE_b))
	d_delay.io.dataIn := VecInit(io.sdtIn.map(_.PE_cd))
	last_delay.io.dataIn := VecInit(io.sdtIn.map(_.PE_last.asTypeOf(UInt(1.W))))
	valid_delay.io.dataIn := VecInit(io.sdtIn.map(_.PE_valid.asTypeOf(UInt(1.W))))
	propagate_delay.io.dataIn := VecInit(io.sdtIn.map(_.PE_propagate.asTypeOf(UInt(1.W))))

	//输出
	for (i <- 0 until rows) {
		io.sdtOut(i).PE_a := a_delay.io.dataOut(i)
		io.sdtOut(i).PE_b := b_delay.io.dataOut(i)
		io.sdtOut(i).PE_cd := d_delay.io.dataOut(i)
		io.sdtOut(i).PE_last := last_delay.io.dataOut(i).asTypeOf(new PESubBundle(cfg).PE_last)
		io.sdtOut(i).PE_valid := valid_delay.io.dataOut(i).asTypeOf(new PESubBundle(cfg).PE_valid)
		io.sdtOut(i).PE_propagate := propagate_delay.io.dataOut(i).asTypeOf(new PESubBundle(cfg).PE_propagate)
	}
}

class SystolicDelayForOutput(cfg: PEConfig, rows:Int=32) extends Module {
	val columns = rows
  val io = FlatIO(new Bundle {
	val dataIn = Input(Vec(rows, UInt(cfg.PEOutWidth.W)))
	val validIn = Input(Vec(rows, Bool()))

	val dataOut = Output(Vec(rows, UInt(cfg.PEOutWidth.W)))
	val validOut = Output(Bool())
  })

  val data_delay = Module(new SystolicDelay(rows, cfg.PEOutWidth, 1, true))
  val valid_delay = Module(new SystolicDelay(rows, 1, 1, true))

  data_delay.io.dataIn := io.dataIn
  valid_delay.io.dataIn := VecInit(io.validIn.map(_.asUInt))

  io.dataOut := data_delay.io.dataOut
  io.validOut := valid_delay.io.dataOut.map(_.asBool).reduce(_ || _)
}


object Transpose extends App {
  //val cfg =  PEConfig(5, 10)
  ChiselStage.emitSystemVerilogFile(
    new Transpose(32, 16),
    firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info", "-default-layer-specialization=enable")
  )
}
