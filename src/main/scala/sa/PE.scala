package sa

import chisel3._
import chisel3.util._
// _root_ disambiguates from package chisel3.util.circt if user imports chisel3.util._
import _root_.circt.stage.ChiselStage
import svsim.CommonCompilationSettings.Timescale.Unit.us


case class PEConfig(expWidth: Int, fracWidth: Int) {
  require(expWidth >= 2, "expWidth >= 2")
  require(fracWidth >= 1, "fracWidth >= 1")
  val PEInWidth: Int = 1+expWidth + fracWidth // 1 bit for sign, expWidth bits for exponent, fracWidth bits for fraction
  val PEOutWidth: Int = 1+expWidth + fracWidth // 1 bit for sign, expWidth bits for exponent, fracWidth bits for fraction
  val PEAccWidth: Int = 1+expWidth + fracWidth // 1 bit for sign, expWidth bits for exponent, fracWidth bits for fraction
  val PEIDWidth: Int = 6 // implicit leading 1 for normalized numbers
  val PETagWidth: Int = PEIDWidth*2
  val PEMACDelay: Int = 9 // MAC delay in cycles
}

class PESubBundle(cfg: PEConfig) extends Bundle {
  val PE_a = UInt(cfg.PEInWidth.W) // PE input A， 对应于矩阵A
  val PE_b = UInt(cfg.PEOutWidth.W)// PE input B, B输入实际上是计算过程中的累加和输入，如果是第一行的PE，则该输入为C矩阵，即偏置矩阵
  val PE_cd = UInt(cfg.PEInWidth.W) // PE input D，D输入实际上是预加载的B矩阵输入

  // 控制信号
  val PE_propagate = Input(Bool()) // 用于选择当前 PE_d_in 预加载到哪个寄存器
  val PE_last = Input(Bool())
  val PE_valid = Input(Bool())
}
// class PEOutBundle(cfg: PEConfig) extends Bundle {
//   val PE_a_out = UInt(cfg.PEInWidth.W) //  向右传播A矩阵
//   val PE_b_out = UInt(cfg.PEOutWidth.W) //　向下传播MAC单元的乘加和计算结果
//   val PE_c_out = UInt(cfg.PEInWidth.W) // 向下传播预加载的B矩阵输入

//   val PE_propagate_out = Output(Bool())
//   val PE_last_out = Output(Bool())
//   val PE_valid_out = Output(Bool())
// }

class PEBundle(cfg: PEConfig) extends Bundle {
	val PEIn = Input(new PESubBundle(cfg))
	val PEOut = Output(new PESubBundle(cfg))
}

class PE(cfg:PEConfig, preloadDelay:Int, rows:Int=32) extends Module{
	val io = FlatIO(new PEBundle(cfg))

	// 例化一个MAC单元
	val fp_cfg = FPConfig(cfg.expWidth, cfg.fracWidth)
	val mac = Module(new fp_ma(fp_cfg))
	// MAC的三个输入，一个输出
	val mac_a_in = Reg(UInt(cfg.PEInWidth.W))
	val mac_b_in = Reg(UInt(cfg.PEInWidth.W))
	val mac_c_in = Reg(UInt(cfg.PEOutWidth.W))
	val mac_d_out = Wire(UInt(cfg.PEOutWidth.W))
	
	
	
	val use_c1_as_buffer = io.PEIn.PE_propagate // 控制信号，选择当前 PE_d_in 预加载到哪个寄存器
	dontTouch(use_c1_as_buffer)
	val use_c1_as_buffer_delay = RegNext(use_c1_as_buffer)

	val mac_valid_in = RegNext(io.PEIn.PE_valid, 0.U)
	val mac_valid_out = Wire(Bool())

	// 连接MAC单元的输入输出
	mac.io.a := mac_a_in
	mac.io.b := mac_b_in
	mac.io.c := mac_c_in
	mac.io.valid_in := mac_valid_in
	mac_d_out := mac.io.d // 乘加和计算结果
	mac_valid_out := mac.io.valid_out // MAC单元的输出有效信号

	io.PEOut.PE_propagate := ShiftRegister(io.PEIn.PE_propagate, cfg.PEMACDelay+1, false.B, true.B) // 后两个参数为复位值和使能信号
	io.PEOut.PE_last := ShiftRegister(io.PEIn.PE_last, cfg.PEMACDelay+1, false.B, true.B) // 后两个参数为复位值和使能信号
	io.PEOut.PE_valid := ShiftRegister(mac_valid_in, cfg.PEMACDelay,false.B, true.B) // 后两个参数为复位值和使能信号

	val valid_cnt = Reg(UInt((log2Ceil(rows)).W))
	valid_cnt := RegNext(Mux(io.PEIn.PE_valid, valid_cnt+1.U, 0.U))

	// 两个预加载寄存器
	val c1 = Reg(UInt(cfg.PEInWidth.W))
	val c2 = Reg(UInt(cfg.PEInWidth.W))

	//一个传播B矩阵的寄存器
	val c_tmp = Reg(UInt(cfg.PEInWidth.W))
	c_tmp := Mux(io.PEIn.PE_valid, io.PEIn.PE_cd, c_tmp) // 如果当前有效输入，则更新c_tmp寄存器
	io.PEOut.PE_cd := ShiftRegister(c_tmp, cfg.PEMACDelay) // 向下传播预加载的B矩阵输入

	

	// 双重缓冲
	val preload_coming = valid_cnt === preloadDelay.U && io.PEIn.PE_valid
	mac_a_in := io.PEIn.PE_a // A矩阵输入一直固定
	mac_c_in := Mux(io.PEIn.PE_valid, io.PEIn.PE_b, 0.U) // 对应C矩阵
	when(use_c1_as_buffer) {
		c1 := Mux(preload_coming, io.PEIn.PE_cd, c1) // 预加载C1
		mac_b_in := Mux(io.PEIn.PE_valid, c2, 0.U)  // 计算使用C2, 对应B矩阵
	} .otherwise {
		c2 := Mux(preload_coming, io.PEIn.PE_cd, c2) // 预加载C2
		mac_b_in := Mux(io.PEIn.PE_valid, c1, 0.U)  // 计算使用C1
	}
	io.PEOut.PE_a := mac_a_in // 向右传播的只有A，所以只延迟了1拍
	io.PEOut.PE_b := Mux(mac_valid_out, mac_d_out, 0.U) // 向下传播MAC单元的乘加和计算结果 ； b_out和c_out都延迟了PEMACDelay+1拍
}
object PE extends App {
  val cfg =  PEConfig(5, 10)
  ChiselStage.emitSystemVerilogFile(
    new PE(cfg, 0, 32),
    firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info", "-default-layer-specialization=enable")
  )
}
