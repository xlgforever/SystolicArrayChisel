package sa

import chisel3._
import chisel3.util._
// _root_ disambiguates from package chisel3.util.circt if user imports chisel3.util._
import _root_.circt.stage.ChiselStage


class Tile(cfg:PEConfig, rows:Int=32) extends Module{
	val cols = rows

	val io = FlatIO(new Bundle {
		val tileIn = Input(Vec(cols, new PESubBundle(cfg)))
		val tileOut = Output(Vec(cols, new PESubBundle(cfg)))
	})

	// rows x cols 个 PE, 每个PE都使用相同的cfg和rows，但是preloadDelay不同，第i行的PE的preloadDelay为i-1
	val peArray = Seq.tabulate(rows, cols){ (i,j) =>
		Module(new PE(cfg, preloadDelay = i, rows))
	}

	val a_tmp = Seq.fill(cols-1, rows)(Wire(UInt(cfg.PEInWidth.W))) // 用于连接PE之间A矩阵
	val b_tmp = Seq.fill(rows-1, cols)(Wire(UInt(cfg.PEOutWidth.W))) // 用于传播PE之间累加和
	val cd_tmp = Seq.fill(rows-1, cols)(Wire(UInt(cfg.PEInWidth.W))) // 用于传播预加载的B矩阵
	val propagate_tmp = Seq.fill(rows-1, cols)(Wire(Bool())) // 用于传播控制信号
	val last_tmp = Seq.fill(rows-1, cols)(Wire(Bool())) // 用于传播控制信号
	val valid_tmp = Seq.fill(rows-1, cols)(Wire(Bool())) // 用于传播控制信号

	val bypass = WireDefault(false.B)

	// 连接 PE 单元
	for(i <- 0 until rows){
		for(j <- 0 until cols){
			if(i==0 && j==0){
				peArray(i)(j).io.PEIn := io.tileIn(i)
				a_tmp(j)(i) := peArray(i)(j).io.PEOut.PE_a
				b_tmp(i)(j) := peArray(i)(j).io.PEOut.PE_b
				cd_tmp(i)(j) := peArray(i)(j).io.PEOut.PE_cd
				propagate_tmp(i)(j) := peArray(i)(j).io.PEOut.PE_propagate
				last_tmp(i)(j) := peArray(i)(j).io.PEOut.PE_last
				valid_tmp(i)(j) := peArray(i)(j).io.PEOut.PE_valid
			} else if(i==0 && j==cols-1){
				peArray(i)(j).io.PEIn.PE_a := a_tmp(j-1)(i)
				peArray(i)(j).io.PEIn.PE_b := io.tileIn(j).PE_b
				peArray(i)(j).io.PEIn.PE_cd := io.tileIn(j).PE_cd
				peArray(i)(j).io.PEIn.PE_propagate := io.tileIn(j).PE_propagate
				peArray(i)(j).io.PEIn.PE_last := io.tileIn(j).PE_last
				peArray(i)(j).io.PEIn.PE_valid := io.tileIn(j).PE_valid
				io.tileOut(i).PE_a := peArray(i)(j).io.PEOut.PE_a // DonCare
				b_tmp(i)(j) := peArray(i)(j).io.PEOut.PE_b
				cd_tmp(i)(j) := peArray(i)(j).io.PEOut.PE_cd
				propagate_tmp(i)(j) := peArray(i)(j).io.PEOut.PE_propagate
				last_tmp(i)(j) := peArray(i)(j).io.PEOut.PE_last
				valid_tmp(i)(j) := peArray(i)(j).io.PEOut.PE_valid
			} else if(i==rows-1 && j==0) {
				peArray(i)(j).io.PEIn.PE_a := io.tileIn(i).PE_a
				peArray(i)(j).io.PEIn.PE_b := b_tmp(i-1)(j)
				peArray(i)(j).io.PEIn.PE_cd := cd_tmp(i-1)(j)
				peArray(i)(j).io.PEIn.PE_propagate := propagate_tmp(i-1)(j)
				peArray(i)(j).io.PEIn.PE_last := last_tmp(i-1)(j)
				peArray(i)(j).io.PEIn.PE_valid := valid_tmp(i-1)(j)
				a_tmp(j)(i) := peArray(i)(j).io.PEOut.PE_a
				io.tileOut(j).PE_b := peArray(i)(j).io.PEOut.PE_b
				io.tileOut(j).PE_cd := peArray(i)(j).io.PEOut.PE_cd
				io.tileOut(j).PE_propagate := peArray(i)(j).io.PEOut.PE_propagate
				io.tileOut(j).PE_last := peArray(i)(j).io.PEOut.PE_last
				io.tileOut(j).PE_valid := peArray(i)(j).io.PEOut.PE_valid
			} else if(i==rows-1 && j==cols-1){
				peArray(i)(j).io.PEIn.PE_a := a_tmp(j-1)(i)
				peArray(i)(j).io.PEIn.PE_b := b_tmp(i-1)(j)
				peArray(i)(j).io.PEIn.PE_cd := cd_tmp(i-1)(j)
				peArray(i)(j).io.PEIn.PE_propagate := propagate_tmp(i-1)(j)
				peArray(i)(j).io.PEIn.PE_last := last_tmp(i-1)(j)
				peArray(i)(j).io.PEIn.PE_valid := valid_tmp(i-1)(j)
				io.tileOut(i) := peArray(i)(j).io.PEOut
			} else if(i==0 && j>=1 && j<=cols-2){
				peArray(i)(j).io.PEIn.PE_a := a_tmp(j-1)(i)
				peArray(i)(j).io.PEIn.PE_b := io.tileIn(j).PE_b
				peArray(i)(j).io.PEIn.PE_cd := io.tileIn(j).PE_cd
				peArray(i)(j).io.PEIn.PE_propagate := io.tileIn(j).PE_propagate
				peArray(i)(j).io.PEIn.PE_last := io.tileIn(j).PE_last
				peArray(i)(j).io.PEIn.PE_valid := io.tileIn(j).PE_valid
				a_tmp(j)(i) := peArray(i)(j).io.PEOut.PE_a
				b_tmp(i)(j) := peArray(i)(j).io.PEOut.PE_b
				cd_tmp(i)(j) := peArray(i)(j).io.PEOut.PE_cd
				propagate_tmp(i)(j) := peArray(i)(j).io.PEOut.PE_propagate
				last_tmp(i)(j) := peArray(i)(j).io.PEOut.PE_last
				valid_tmp(i)(j) := peArray(i)(j).io.PEOut.PE_valid
			} else if(j==0 && i>=1 && i<=rows-2){
				peArray(i)(j).io.PEIn.PE_a := io.tileIn(i).PE_a
				peArray(i)(j).io.PEIn.PE_b := b_tmp(i-1)(j)
				peArray(i)(j).io.PEIn.PE_cd := cd_tmp(i-1)(j)
				peArray(i)(j).io.PEIn.PE_propagate := propagate_tmp(i-1)(j)
				peArray(i)(j).io.PEIn.PE_last := last_tmp(i-1)(j)
				peArray(i)(j).io.PEIn.PE_valid := valid_tmp(i-1)(j)
				a_tmp(j)(i) := peArray(i)(j).io.PEOut.PE_a
				b_tmp(i)(j) := peArray(i)(j).io.PEOut.PE_b
				cd_tmp(i)(j) := peArray(i)(j).io.PEOut.PE_cd
				propagate_tmp(i)(j) := peArray(i)(j).io.PEOut.PE_propagate
				last_tmp(i)(j) := peArray(i)(j).io.PEOut.PE_last
				valid_tmp(i)(j) := peArray(i)(j).io.PEOut.PE_valid
			} else if(i==rows-1 && j>=1 && j<=cols-2){
				peArray(i)(j).io.PEIn.PE_a := a_tmp(j-1)(i)
				peArray(i)(j).io.PEIn.PE_b := b_tmp(i-1)(j)
				peArray(i)(j).io.PEIn.PE_cd := cd_tmp(i-1)(j)
				peArray(i)(j).io.PEIn.PE_propagate := propagate_tmp(i-1)(j)
				peArray(i)(j).io.PEIn.PE_last := last_tmp(i-1)(j)
				peArray(i)(j).io.PEIn.PE_valid := valid_tmp(i-1)(j)
				a_tmp(j)(i) := peArray(i)(j).io.PEOut.PE_a // DonCare
				io.tileOut(j).PE_b := peArray(i)(j).io.PEOut.PE_b
				io.tileOut(j).PE_cd := peArray(i)(j).io.PEOut.PE_cd
				io.tileOut(j).PE_propagate := peArray(i)(j).io.PEOut.PE_propagate
				io.tileOut(j).PE_last := peArray(i)(j).io.PEOut.PE_last
				io.tileOut(j).PE_valid := peArray(i)(j).io.PEOut.PE_valid
			} else if(j==cols-1 && i>=1 && i<=rows-2){
				peArray(i)(j).io.PEIn.PE_a := a_tmp(j-1)(i)
				peArray(i)(j).io.PEIn.PE_b := b_tmp(i-1)(j)
				peArray(i)(j).io.PEIn.PE_cd := cd_tmp(i-1)(j)
				peArray(i)(j).io.PEIn.PE_propagate := propagate_tmp(i-1)(j)
				peArray(i)(j).io.PEIn.PE_last := last_tmp(i-1)(j)
				peArray(i)(j).io.PEIn.PE_valid := valid_tmp(i-1)(j)
				io.tileOut(i).PE_a := peArray(i)(j).io.PEOut.PE_a // DonCare
				b_tmp(i)(j) := peArray(i)(j).io.PEOut.PE_b
				cd_tmp(i)(j) := peArray(i)(j).io.PEOut.PE_cd
				propagate_tmp(i)(j) := peArray(i)(j).io.PEOut.PE_propagate
				last_tmp(i)(j) := peArray(i)(j).io.PEOut.PE_last
				valid_tmp(i)(j) := peArray(i)(j).io.PEOut.PE_valid
			} else {
				peArray(i)(j).io.PEIn.PE_a := a_tmp(j-1)(i)
				peArray(i)(j).io.PEIn.PE_b := b_tmp(i-1)(j)
				peArray(i)(j).io.PEIn.PE_cd := cd_tmp(i-1)(j)
				peArray(i)(j).io.PEIn.PE_propagate := propagate_tmp(i-1)(j)
				peArray(i)(j).io.PEIn.PE_last := last_tmp(i-1)(j)
				peArray(i)(j).io.PEIn.PE_valid := valid_tmp(i-1)(j)
				a_tmp(j)(i) := peArray(i)(j).io.PEOut.PE_a
				b_tmp(i)(j) := peArray(i)(j).io.PEOut.PE_b
				cd_tmp(i)(j) := peArray(i)(j).io.PEOut.PE_cd
				propagate_tmp(i)(j) := peArray(i)(j).io.PEOut.PE_propagate
				last_tmp(i)(j) := peArray(i)(j).io.PEOut.PE_last
				valid_tmp(i)(j) := peArray(i)(j).io.PEOut.PE_valid
			}
		}
	}
	
}

object Tile extends App {
  val cfg =  PEConfig(5, 10)
  ChiselStage.emitSystemVerilogFile(
    new Tile(cfg, 32),
    firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info", "-default-layer-specialization=enable")
  )
}
