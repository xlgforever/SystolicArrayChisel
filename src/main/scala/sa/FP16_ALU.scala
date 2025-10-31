package sa

import chisel3._
import chisel3.util._
// _root_ disambiguates from package chisel3.util.circt if user imports chisel3.util._
import _root_.circt.stage.ChiselStage


case class FPConfig(expWidth: Int, fracWidth: Int) {
  require(expWidth >= 2, "expWidth >= 2")
  require(fracWidth >= 1, "fracWidth >= 1")
  val signWidth: Int = 1
  val expBits: Int = expWidth
  val fracBits: Int = fracWidth
  val sigBits: Int = fracBits + 1 // implicit leading 1 for normalized numbers
  val width: Int = signWidth + expBits + fracBits
  val bias: Int = (1 << (expWidth - 1)) - 1
}

object FPHelpers {
  // helper to extract fields
  def unpack(x: UInt, cfg: FPConfig): (UInt, UInt, UInt) = {
    val sign = x(cfg.width - 1, cfg.width - 1)
    val exp = x(cfg.width - 2, cfg.fracBits)
    val frac = x(cfg.fracBits - 1, 0)
    (sign, exp, frac)
  }
  // helper to check special encodings: zero, inf, subnormal, NaN 
  def specialEncodingChecks(cfg: FPConfig, exp: UInt, frac: UInt): (Bool, Bool, Bool, Bool) = {
	require(exp.getWidth == cfg.expBits, "exp width mismatch")
	require(frac.getWidth == cfg.fracBits, "frac width mismatch")
	val expIsZero = exp === 0.U
	val expIsAllOnes = exp === ((1 << cfg.expBits) - 1).U
	val fracIsZero = frac === 0.U
	val isZero = expIsZero && fracIsZero
	val isInf = expIsAllOnes && fracIsZero
	val isSubnormal = expIsZero && !fracIsZero
	val isNaN = expIsAllOnes && !fracIsZero
	(isZero, isInf, isSubnormal, isNaN)
  }
  //def isZero(exp: UInt, frac: UInt): Bool = exp === 0.U && frac === 0.U
  //def isInf(exp: UInt, frac: UInt): Bool = exp === ((1 << cfg.expBits) - 1).U && frac === 0.U
  //def isSubnormal(exp: UInt, frac: UInt): Bool = exp === 0.U && frac =/= 0.U
  //def isNaN(exp: UInt, frac: UInt): Bool = exp === ((1 << cfg.expBits) - 1).U && frac =/= 0.U

  // We keep wider arithmetic helpers in the module for clarity
  private def cfg = ??? // unused placeholder to satisfy doc comments
}

class FPMultiply(cfg: FPConfig) extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(cfg.width.W))
    val b = Input(UInt(cfg.width.W))
    val out = Output(UInt(cfg.width.W))
    // status flags
    val invalid = Output(Bool()) // e.g., 0 * inf => invalid (NaN)
    val overflow = Output(Bool())
    val underflow = Output(Bool())
  })

  // --- Unpack inputs ---
  val (aSign, aExp, aFrac) = FPHelpers.unpack(io.a, cfg)
  val (bSign, bExp, bFrac) = FPHelpers.unpack(io.b, cfg)

  // special encodings
  val expAllOnes = ((1 << cfg.expBits) - 1).U(cfg.expBits.W)	
  val (aIsZero, aIsInf, aIsSubnormal, aIsNaN) = FPHelpers.specialEncodingChecks(cfg, aExp, aFrac)
  val (bIsZero, bIsInf, bIsSubnormal, bIsNaN) = FPHelpers.specialEncodingChecks(cfg, bExp, bFrac)

  // stage 1
  val sign_s1 = RegNext(aSign ^ bSign)
  val aExp_s1 = RegNext(aExp +& aIsSubnormal.asUInt) // if a is subnormal number, the actual bias is 14, so we add 1 here, later to subtract 15
  val bExp_s1 = RegNext(bExp +& bIsSubnormal.asUInt)
  val aFrac_s1 = RegNext(Mux(aIsSubnormal, Cat(0.U(1.W), aFrac), Cat(1.U(1.W), aFrac))) // sigBits wide
  val bFrac_s1 = RegNext(Mux(bIsSubnormal, Cat(0.U(1.W), bFrac), Cat(1.U(1.W), bFrac)))	 // sigBits wide
  
  // stage 2
  val expAdd_s2 = RegNext(aExp_s1 +& bExp_s1) 
  val fracMul_s2 = RegNext(aFrac_s1 * bFrac_s1)
  // --- Handle special cases up front ---
  // NaN propagation
  val anyNaN = aIsNaN || bIsNaN
  val anyInf = aIsInf || bIsInf

  // invalid: 0 * inf -> NaN
  val invalid = (aIsZero && bIsInf) || (bIsZero && aIsInf)

  // widen to UInt for multiply
  val aSigU = aSig.asUInt
  val bSigU = bSig.asUInt

  // exponent calculation: if denormal, effective exponent = 1 - bias (we will represent it as Int algebraically)
  val aExpInt = Wire(SInt((cfg.expBits + 2).W))
  val bExpInt = Wire(SInt((cfg.expBits + 2).W))
  aExpInt := Mux(aIsNormal, (aExp.asSInt - cfg.bias.S), (1.S - cfg.bias.S))
  bExpInt := Mux(bIsNormal, (bExp.asSInt - cfg.bias.S), (1.S - cfg.bias.S))

  // multiply mantissas -> product width = 2 * sigBits
  val prodWidth = cfg.sigBits * 2
  val rawProd = (aSigU * bSigU).asUInt // width = sigBits + sigBits

  // exponent add
  val rawExp = aExpInt +& bExpInt

  // normalization: product highest bit may be at position 2*sigBits-1 or 2*sigBits-2
  // If the top bit is 1 -> shift right by 1 and increment exponent
  val topBit = rawProd(prodWidth - 1)
  val normalizedSigShifted = Wire(UInt((cfg.sigBits + 1).W)) // keep one extra guard bit for rounding
  val normalizedExp = Wire(SInt(rawExp.getWidth.W))

  // Create an extended product with extra low bits for rounding sticky
  val guardBits = 3 // guard, round, sticky
  val extendedProd = Cat(rawProd, 0.U(guardBits.W)) // simple; we'll compute sticky properly later if we implement shifts

  when(topBit === 1.U) {
    // shift right by (sigBits) ? Actually we want to align to have sigBits+? We'll produce sigBits+guard bits
    // Raw product is (sigBits*2) bits representing (1.f * 1.f) => range [1,4). We want normalized mantissa in [1,2) -> if topBit==1, MSB at position prodWidth-1 implies >=2, so shift right by 1
    normalizedSigShifted := Cat(rawProd(prodWidth - 1), rawProd(prodWidth - 2, prodWidth - cfg.sigBits - 1)) // approximate
    normalizedExp := rawExp + 1.S
  } .otherwise {
    normalizedSigShifted := rawProd(prodWidth - 2, prodWidth - cfg.sigBits - 1)
    normalizedExp := rawExp
  }

  // For rounding we need guard, round, sticky bits; compute them from rawProd low bits
  // Define positions: after selecting top sigBits bits, the next bit is guard, then round, then rest -> sticky
  val shiftAmount = Mux(topBit === 1.U, 1.U, 0.U)
  // index in rawProd where MSB of normalized significand starts
  val msbIndex = (prodWidth - 1)..U - shiftAmount

  // For simplicity compute by shifting rawProd appropriately
  val aligned = (rawProd >> shiftAmount.asUInt)
  val mantissaWithExtras = aligned // width prodWidth-shiftAmount

  val mantissaHigh = mantissaWithExtras((cfg.sigBits), 1) // top sigBits bits excluding implicit 1? adjust indexes carefully
  val guard = mantissaWithExtras(0)
  // set round and sticky (we took a simplified approach)
  val round = 0.U
  val sticky = 0.U

  // Pack mantissa: keep fracBits bits (drop implicit 1)
  val resultFrac = mantissaHigh(cfg.fracBits - 1, 0)

  // Assemble result exponent back to biased form
  val resultExpBiasedS = normalizedExp + cfg.bias.S
  val resultExpBiased = Wire(UInt(cfg.expBits.W))
  // Underflow/overflow checks
  val overflow = Wire(Bool())
  val underflow = Wire(Bool())
  when(resultExpBiasedS > ((1 << cfg.expBits) - 1).S - 1.S) {
    // overflow -> set Inf
    resultExpBiased := expAllOnes
    overflow := true.B
  } .elsewhen(resultExpBiasedS <= 0.S) {
    // underflow -> denormal or zero (very simplified)
    resultExpBiased := 0.U
    underflow := true.B
  } .otherwise {
    resultExpBiased := resultExpBiasedS.asUInt
    overflow := false.B
    underflow := false.B
  }

  // Final packing & special cases
  val outNaN = Cat(1.U(1.W), expAllOnes, 1.U((cfg.fracBits - 1).W)) // quiet NaN pattern (simple)
  val outInf = Cat(rSign, expAllOnes, 0.U(cfg.fracBits.W))
  val outZero = Cat(rSign, 0.U(cfg.expBits.W), 0.U(cfg.fracBits.W))

  io.invalid := invalid || anyNaN
  io.overflow := overflow
  io.underflow := underflow

  io.out := Mux(anyNaN, outNaN,
               Mux(invalid, outNaN,
                 Mux(anyInf && (aIsZero || bIsZero), outNaN,
                   Mux(anyInf, outInf,
                     Mux(aIsZero || bIsZero, outZero,
                       Cat(rSign, resultExpBiased, resultFrac))))))
}

// ---------------------- Example instantiations ----------------------
object FPExamples extends App {
  val fp16 = FPConfig(expWidth = 5, fracWidth = 10) // Note: this is actually IEEE half: exp=5, frac=10 -> total 16
  val fp32 = FPConfig(expWidth = 8, fracWidth = 23)

  println(s"FP16 width: ${fp16.width}, bias: ${fp16.bias}")
  println(s"FP32 width: ${fp32.width}, bias: ${fp32.bias}")

  // In practice: elaborate the module using chisel3.Driver or chisel3.stage.ChiselStage
  // (Example: (new chisel3.stage.ChiselStage).emitVerilog(new FPMultiply(fp16)))
}