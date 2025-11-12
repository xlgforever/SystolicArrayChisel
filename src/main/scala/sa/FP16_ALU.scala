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

class specialSignals() extends Bundle {
  val isNormal = Bool()
  val isZero = Bool()
  val isInf = Bool()
  val isSubnormal = Bool()
  val isNaN = Bool()
  val isSignalingNaN = Bool()
  val isQuietNaN = Bool()
}

class fpNumber(cfg:FPConfig) extends Bundle {
  val sign = UInt(1.W)
  val exp = UInt(cfg.expBits.W)
  val frac = UInt(cfg.fracBits.W)
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
  def specialEncodingChecks2(cfg: FPConfig, exp: UInt, frac: UInt, isBoxed:Bool): specialSignals = {
    require(exp.getWidth == cfg.expBits, "exp width mismatch")
    require(frac.getWidth == cfg.fracBits, "frac width mismatch")
    val specialSignals = new specialSignals()
    val expIsZero = exp === 0.U
    val expIsAllOnes = exp === ((1 << cfg.expBits) - 1).U
    val fracIsZero = frac === 0.U

    specialSignals.isNormal := isBoxed && !expIsZero && !expIsAllOnes
    specialSignals.isZero := isBoxed && expIsZero && fracIsZero
    specialSignals.isInf := isBoxed && expIsAllOnes && fracIsZero
    specialSignals.isSubnormal := isBoxed && expIsZero && !fracIsZero
    specialSignals.isNaN := !isBoxed || (expIsAllOnes && !fracIsZero)
    specialSignals.isSignalingNaN := isBoxed && specialSignals.isNaN && (frac(cfg.fracBits - 1) === 0.U)
    specialSignals.isQuietNaN := specialSignals.isNaN && !specialSignals.isSignalingNaN
    (specialSignals)
  }

  def specialResultGen(cfg: FPConfig, cSign: UInt, aIsZero:Bool, aIsInf:Bool, aIsNaN:Bool,
                    bIsZero:Bool, bIsInf:Bool, bIsNaN:Bool): (Bool, UInt) = {
    val expWidth = cfg.expBits
    val fracBits = cfg.fracBits
    val width = cfg.width
    val numNaN: UInt = Cat(0.U(1.W), ((1 << expWidth) - 1).U(expWidth.W), ((1 << fracBits) - 1).U(fracBits.W))
    val numPosInf: UInt = Cat(0.U(1.W), ((1 << expWidth) - 1).U(expWidth.W), 0.U(fracBits.W))
    val numNegInf: UInt = Cat(1.U(1.W), ((1 << expWidth) - 1).U(expWidth.W), 0.U(fracBits.W))
    val numPosZero: UInt = 0.U(width.W)
    val numNegZero: UInt = Cat(1.U(1.W), 0.U((expWidth + fracBits).W))
    val sepcialResult = MuxCase(numNaN, Array(
      (aIsNaN || bIsNaN) -> numNaN,
      ((aIsInf && bIsZero) || (bIsInf && aIsZero)) -> numNaN,
      (aIsInf || bIsInf) -> Mux(cSign.asBool, numNegInf, numPosInf),
      (aIsZero  || bIsZero) -> Mux(cSign.asBool, numNegZero, numPosZero)
    ))
    val isSpecial = aIsNaN || bIsNaN || aIsInf || bIsInf || aIsZero || bIsZero
    (isSpecial, sepcialResult)
  }

  // op: 0代表乘法， 1代表加法，  2代表乘加
  def specialResultGen2(cfg:FPConfig, op:Int, fpNumberArray:Seq[fpNumber], specialSignalsArray:Seq[specialSignals]) : (Bool, UInt) = {
    val numOperands = specialSignalsArray.length
    require(numOperands >= 2, "at least 2 operand is required")
    
    val info_a = specialSignalsArray(0)
    val info_b = specialSignalsArray(1)
    val info_c = if (numOperands == 3) specialSignalsArray(2) else 0.U.asTypeOf(new specialSignals())
    val fp_a = fpNumberArray(0)
    val fp_b = fpNumberArray(1)
    val fp_c = if (numOperands == 3) fpNumberArray(2) else 0.U.asTypeOf(new fpNumber(cfg))

    val ab_operand_zero = specialSignalsArray.take(2).map(s => s.isZero).reduce(_ || _) // 只判断前两个操作数是否包含0
    val any_operand_inf = specialSignalsArray.map(s => s.isInf).reduce(_ || _)
    val any_operand_nan = specialSignalsArray.map(s => s.isNaN).reduce(_ || _)

    
    val expWidth = cfg.expBits
    val fracBits = cfg.fracBits
    val width = cfg.width
    val numNaN: UInt = Cat(0.U(1.W), ((1 << expWidth) - 1).U(expWidth.W), ((1 << fracBits) - 1).U(fracBits.W))
    val numPosInf: UInt = Cat(0.U(1.W), ((1 << expWidth) - 1).U(expWidth.W), 0.U(fracBits.W))
    val numNegInf: UInt = Cat(1.U(1.W), ((1 << expWidth) - 1).U(expWidth.W), 0.U(fracBits.W))
    val numPosZero: UInt = 0.U(width.W)
    val numNegZero: UInt = Cat(1.U(1.W), 0.U((expWidth + fracBits).W))

    val isSpecial = WireDefault(false.B)
    val specialResult = WireDefault(numNaN)

    when((info_a.isInf && info_b.isZero) || (info_b.isInf && info_a.isZero)){ // 乘法产生了NaN
      isSpecial := true.B
      specialResult := numNaN
    } .elsewhen(any_operand_nan){ // 包含NaN
      isSpecial := true.B
      specialResult := numNaN
    } .elsewhen(any_operand_inf){ // 包含无穷大
      isSpecial := true.B
      if (op == 1) { // 加法操作时
        specialResult := Mux(info_a.isInf, Cat(fp_a.sign, (1<<cfg.expBits -1).U(cfg.expBits.W), 0.U(cfg.fracBits.W)),
                                             Cat(fp_b.sign, (1<<cfg.expBits -1).U(cfg.expBits.W), 0.U(cfg.fracBits.W)))
      } else if(op==2){ // 乘加操作时
        specialResult := Mux(info_c.isInf, Cat(fp_c.sign, (1<<cfg.expBits -1).U(cfg.expBits.W), 0.U(cfg.fracBits.W)),
                                             Cat(fp_b.sign.asBool ^ fp_a.sign.asBool, (1<<cfg.expBits -1).U(cfg.expBits.W), 0.U(cfg.fracBits.W)))
      } else { // 乘法操作时
        specialResult := Mux(fp_a.sign.asBool ^ fp_b.sign.asBool, (1<<cfg.expBits-1).U(cfg.expBits.W), 0.U(cfg.expBits.W))
      }
    } .elsewhen(ab_operand_zero){ // a、b中包含零
      isSpecial := true.B
      if (op == 1) { // 加法操作时
        specialResult := Mux(info_a.isZero,fp_b.asUInt, fp_a.asUInt)
      } else if(op==2){ // 乘加操作时
        specialResult := fp_c.asUInt
      } else { // 乘法操作时
        specialResult := Cat(fp_a.sign ^ fp_b.sign, 0.U((cfg.expBits + cfg.fracBits).W))
      }
    }
    (isSpecial, specialResult)
  }
  

  def frac_round(din:UInt, mode:Int, din_len:Int, din_len_frac:Int, dout_len:Int, dout_len_frac:Int): (UInt, UInt)= {
    val clip_bits = din_len_frac - dout_len_frac
    
    val frac_pre = din(din_len_frac-1, clip_bits) // 不需要舍去的小数部分
    val dout_pre = din(din_len-1, clip_bits) // 不需要舍去的整数+小数部分

    val frac_0p5 = din(clip_bits-1) // 舍去部分的最高位，用来判断是否大于0.5
    val frac_other = Wire(Bool())
    if (clip_bits >= 2) {
      frac_other := din(clip_bits-2, 0).orR // 舍去部分的其他位，用来判断是否大于0.5
    } else {
      frac_other := 0.U(1.W).asBool // 如果舍去部分只有1位，则其他位为0
    }

    val round_toward_zero =  0.U(1.W)
    val round_to_nearest_even = frac_0p5 && (frac_other || frac_pre(0)) // 大于0.5或者等于0.5且最后1bit为奇数时进1，确保进位后最后1bit为0
    val round_to_nearest_odd = frac_0p5 && (frac_other || !frac_pre(0)) // 大于0.5或者等于0.5且最后1bit为偶数时进1，确保进位后最后1bit为1
    val round_bit = MuxCase(round_to_nearest_even, Array(
      (mode.U === 0.U) -> round_toward_zero,
      (mode.U === 1.U) -> round_to_nearest_odd
    ))
    val round_result_tmp = Wire(UInt((dout_pre.getWidth + 1).W))
    round_result_tmp := dout_pre +& round_bit // 带进位的加法
    val carry = round_result_tmp(round_result_tmp.getWidth-1)
    val round_result = round_result_tmp(round_result_tmp.getWidth-2,0)
    (carry, round_result)
  }
}
/// normal clip
class normal_clip(cfg:FPConfig, fracWidth:Int) extends Module {
  val io = IO(new Bundle {
    val frac_all = Input(UInt(fracWidth.W))
    val exp_all  = Input(UInt((cfg.expBits+1).W))
    val sign_flag = Input(UInt(1.W))
    val is_special = Input(Bool())
    val special_result = Input(UInt(cfg.width.W))

    val fp_c = Output(UInt(cfg.width.W))
  })
   //舍入处理
  // 1. 当exp_all不全为0时，说明不是subnomal，此时frac_all需要左移1bit来去掉隐藏的1；如果exp_all是全0,则frac_all不需要左移，因为没有隐藏的1；
  val final_frac = Wire(UInt((cfg.sigBits * 2 + 1).W))
  final_frac := Mux(io.exp_all.orR, io.frac_all << 1.U, io.frac_all)
  // 2. 当包含整数部分的frac_all不为全0时，指数部分不变；当包含整数部分的frac_all全为0时，指数部分也要为0，表示结果为0
  val final_exp = Mux(io.frac_all.orR, io.exp_all, 0.U)
  dontTouch(final_exp)
  // 3. 预处理，在本阶段，指数部分不用动，尾数部分位宽为cfg.fracBits+2位，其中低两bit用于舍入，判断舍掉的部分与0.5的关系
  val pre_round_frac = Wire(UInt((cfg.fracBits + 2).W))
  pre_round_frac := Cat(final_frac(final_frac.getWidth-1, final_frac.getWidth-1-cfg.fracBits), final_frac(final_frac.getWidth-2-cfg.fracBits, 0).orR) // cfg.fracBits + 2 bits
  dontTouch(pre_round_frac)
  val pre_round_exp = final_exp
  dontTouch(pre_round_exp)
  // 4. 舍入
  val (round_carry, round_frac) = FPHelpers.frac_round(pre_round_frac, 2, pre_round_frac.getWidth, pre_round_frac.getWidth, cfg.fracBits, cfg.fracBits)
  
  // 根据舍入结果，调整指数，并检查调整后的指数是否大于最大值
  val result_mult_exp = RegNext(pre_round_exp +& round_carry)
  val overflow = result_mult_exp > ((1 << cfg.expBits) - 1).U
  dontTouch(overflow)
  val result_mult_frac = RegNext(round_frac)
////////////////////////////////////////////////////////////////////stage 6//////////////////////////////////////////////
  // 最终输出
  val final_exp_wire = Mux(overflow, ((1 << cfg.expBits) - 1).U, result_mult_exp(cfg.expBits-1,0))
  dontTouch(final_exp_wire)
  val final_frac_wire = Mux(overflow, 0.U, result_mult_frac)
  dontTouch(final_frac_wire)
  val fp_c = Mux(io.is_special, io.special_result, Cat(io.sign_flag, final_exp_wire, final_frac_wire)) // io相关信号在外部进行了时钟对齐

  io.fp_c := RegNext(fp_c)
}


class fix2fp(cfg:FPConfig, dinWidth:Int=24, fracWidth:Int=21, addFunc:Int=0) extends Module {
  val intWidth = dinWidth - fracWidth // 当为mac运算时，intWidth为3bit; 当仅为乘或仅为加时，intWidth为2bit

  val io = IO(new Bundle {
    val fix_frac = Input(UInt(dinWidth.W))
    val fix_exp = Input(UInt((cfg.expBits+2).W))

    //val valid_in = Input(Bool())
    val sign_flag = Input(UInt(1.W))
    val is_special = Input(Bool())
    val special_result = Input(UInt(cfg.width.W))

    val fp_c = Output(UInt(cfg.width.W))
    //val valid_out = Output(Bool())
  })

///////////////////////////////////////////////////////////// stage 3 /////////////////////////////////////////////////////////////////
  // lzd
  val lzd = Module(new LZD_Wrapper(dinWidth))
  lzd.io.data := io.fix_frac
  val lzdCount = RegNext(lzd.io.zcnt)
  val lzdZero  = RegNext(lzd.io.allZero)
  // 与lzd的结果对齐时钟
  val fracMul_s3 = RegNext(io.fix_frac)
  val fix_exp = RegNext(io.fix_exp)

///////////////////////////////////////////////////////////////////////stage 4//////////////////////////////////////////////
  // 1. 指数部分需要减去lzd的结果
  // 因为发送给前导零检测模块的输入包含了2bit的整数位，这2bit的情况可能为00, 01, 10, 11；、
  // 当为00或者01时，整数部分的0不应该参与前导0检测，所以减去前导零的结果后，需要+1；
  // 当为10或者11时，尾数部分需要右移1bit，指数部分本来就需要+1；
  // 所以后续默认尾数部分只有1bit整数位
  val exp_all_wire = Wire(UInt((cfg.expBits+2).W))
  exp_all_wire := fix_exp - lzdCount + intWidth.U - 1.U// 这里没有进行位宽扩展，结果的位宽和fix_exp相同，即7bit有符号数，表示范围为-64 ~ +63；
  dontTouch(exp_all_wire)
  val exp_all_wire_msb = exp_all_wire(cfg.expBits+2-1) // 取最高bit位，表示正负性
  val exp_all_abs = (~exp_all_wire + 1.U)(cfg.expBits+2-2,0) // 取反加一，得到绝对值, 因为最小值为-BIAS-22，=-27，最大值为30+30+1=61，所以这里绝对值只需要6bit无符号整数
  dontTouch(exp_all_abs)
  // 上溢：exp_all_wire 为正数，并且其值大于等于最大值 (1<<cfg.expBits)-1
  val isOverflow = ~exp_all_wire_msb && exp_all_wire(exp_all_wire.getWidth-2,0) >= ((1 << cfg.expBits) - 1).U // 如果为正数，并且超过最大值
  // 下溢：exp_all_wire 为负数，并且负数的绝对值 大于 dinWidth，此时尾数部分不足以让指数部分变为>=0，所以会下溢
  // 说明一下，当fix_exp>=0时，是不会发生underflow的，因为此时最多对尾数部分不进行左移或者只进行部分左移
  val isUnderflow = exp_all_wire_msb && (exp_all_abs > dinWidth.U) //如果为负数，并且 绝对值 大于 dinWidth，表明输入的frac会被全部右移移位掉，变成0；则发生了下溢
  dontTouch(isOverflow)
  val exp_all = Reg(UInt((cfg.expBits+2).W)) // 扩宽了2bit，最高bit代表正负性，第二高位代表是否大于等于1 << cfg.expBits) - 1
  //val overflow = Reg(UInt(1.W))
  //val underflow = Reg(UInt(1.W))
  //exp_all := RegNext(Mux(exp_all_wire_msb, 0.U, exp_all_wire(cfg.expBits+2-2,0))) // 如果最高bit位为1,表示负数，即指数部分不够lzd的大小，指数部分最多变为0；
  when(fix_exp===0.U && fracMul_s3(dinWidth-1, dinWidth-1-(intWidth-1))===1.U){ // 因为如果此时fix_exp为0,即代表真实值+BIAS=0,即真实值=-15，而真实值最小只能是-14,所以这里要将字面值设置为1,代表真实值=-14，对应的尾数需要左移1bit，整数位变为1位
    exp_all := 1.U
    //overflow := false.B
    //underflow := false.B
  } .elsewhen(exp_all_wire_msb){ //考虑 exp_all_wire为负数，如果最高bit位为1,表示负数，即指数部分不够lzd的大小，指数部分最多变为0；
    exp_all := 0.U
    //overflow := false.B
    //when(isUnderflow){ // 如果负数负的太多，会下溢
    //  underflow := true.B
    //} .otherwise{
    //  underflow := false.B
    //}
  } .otherwise{ // 考虑exp_all_wire为正数，且没有超过上界的情况
    exp_all := exp_all_wire
    //overflow := false.B
    //underflow := false.B
  }

  // 2. 尾数部分需要左移lzd 或者 右移的结果
  val frac_right_shift = exp_all_wire_msb && (lzdCount < exp_all_abs) // 当exp_all_wire为负数，并且负的比lzdCount还多，说明 fix_exp 在减去 lzdCount 之前，就已经是负数了，此时需要将尾数右移
  dontTouch(frac_right_shift)
  val exp_shift0 = (fix_exp + intWidth.U - 1.U)(cfg.expBits-1,0) // 代表fix_exp是正数，经过运算之后变成了负数，此时尾数不能左移lzd的大小，而是只能左移exp_shift0位，为什么会有intWidth-1，是为了让尾数的整数部分只包含1bit
  dontTouch(exp_shift0)
  val exp_shift1 = (~fix_exp - s.U + 2.U)(cfg.expBits,0) // 即exp_shift0的补码； 当fix_exp为负数的时候，尾数需要右移exp_shift1位，并且
  dontTouch(exp_shift1)
  val exp_shift = Mux(exp_all_wire_msb, exp_shift0, lzdCount)(log2Ceil(dinWidth)-1,0) // 当exp_all_wire为负数时(此时不考虑fix_exp+1本来就为负数），表示指数部分不够lzd的大小，最多只能右移exp_shift0位，否则右移lzdCount位
  dontTouch(exp_shift)
  val frac_all = Reg(UInt((cfg.sigBits * 2+1).W)) // 23 bits，多一位是为了后续的舍入
  // 当fix_exp+1本身就是负数时，尾数需要右移exp_shift1 位，并且需要对移掉的部分进行舍入操作
  val frac_right_shift1 = Wire(UInt((cfg.sigBits * 2).W))
  val frac_right_shift1_sticky = Wire(UInt(1.W))
  frac_right_shift1 := fracMul_s3 >> exp_shift1
  frac_right_shift1_sticky := ( fracMul_s3 << ((cfg.sigBits*2).U-exp_shift1)  ).orR // 对移掉的部分进行舍入操作
  // 当 fix_exp+1 本身不是负数时，尾数需要左移exp_shift位
  val frac_left_shifted = (Cat(fracMul_s3, 0.U(1.W)) << exp_shift)(cfg.sigBits*2+1-1, 0) // 低位补0不影响结果，只是为了位宽一致，就像是在10进制小数最后补0一样
  when(frac_right_shift){
    frac_all := Cat(frac_right_shift1, frac_right_shift1_sticky)
  } .otherwise{
    frac_all := frac_left_shifted
  }

  // 3. 舍入处理
  val clip = Module(new normal_clip(cfg, dinWidth+1))
  clip.io.frac_all := frac_all
  clip.io.exp_all := exp_all 
  clip.io.sign_flag := io.sign_flag
  clip.io.is_special := io.is_special
  clip.io.special_result := io.special_result
  io.fp_c := clip.io.fp_c
  //io.valid_out := ShiftRegister(io.valid_in, 4)
}


//@chiselName
class FPMultiply(cfg: FPConfig) extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(cfg.width.W))
    val b = Input(UInt(cfg.width.W))
    val valid_in = Input(Bool())
    val out = Output(UInt(cfg.width.W))
    // status flags
    val valid_out = Output(Bool()) // e.g., 0 * inf => invalid (NaN)
    // val overflow = Output(Bool())
    // val underflow = Output(Bool())
  })

  // --- Unpack inputs ---
  val (aSign, aExp, aFrac) = FPHelpers.unpack(io.a, cfg)
  val (bSign, bExp, bFrac) = FPHelpers.unpack(io.b, cfg)

  // special encodings
  val (aIsZero, aIsInf, aIsSubnormal, aIsNaN) = FPHelpers.specialEncodingChecks(cfg, aExp, aFrac)
  val (bIsZero, bIsInf, bIsSubnormal, bIsNaN) = FPHelpers.specialEncodingChecks(cfg, bExp, bFrac)

  // stage 1
  val sign_s1 = RegNext(aSign ^ bSign)
  val aExp_s1 = RegNext(aExp + aIsSubnormal.asUInt) // if a is subnormal number, the actual bias is 14, so we add 1 here, later to subtract 15
  val bExp_s1 = RegNext(bExp + bIsSubnormal.asUInt) // 不需要扩位
  val aFrac_s1 = RegNext(Mux(aIsSubnormal, Cat(0.U(1.W), aFrac), Cat(1.U(1.W), aFrac))) // sigBits wide
  val bFrac_s1 = RegNext(Mux(bIsSubnormal, Cat(0.U(1.W), bFrac), Cat(1.U(1.W), bFrac)))	 // sigBits wide
  
  // stage 2
  val expAdd_s2 = RegNext(aExp_s1 +& bExp_s1)  // & 号表示带位宽扩展的加法或者减法
  val fracMul_s2 = RegNext(aFrac_s1 * bFrac_s1)

  val fix_exp_wire = expAdd_s2 -& cfg.bias.U
  dontTouch(fix_exp_wire)

  // delay sign 4 clcyles
  val sign_s5 = ShiftRegister(sign_s1, 4)
  // delay special result 5 cycles
  val (isSpecial, specialResult) = FPHelpers.specialResultGen(cfg, aSign ^ bSign, aIsZero, aIsInf, aIsNaN, bIsZero, bIsInf, bIsNaN)
  val specialResult_s5 = ShiftRegister(specialResult, 5)
  val isSpecial_s5 = ShiftRegister(isSpecial, 5)
  

  val fix2fp = Module(new fix2fp(cfg, cfg.sigBits*2, cfg.fracBits*2, 0))
  fix2fp.io.fix_frac := fracMul_s2
  fix2fp.io.fix_exp := fix_exp_wire
  fix2fp.io.sign_flag := sign_s1
  fix2fp.io.is_special := isSpecial_s5
  fix2fp.io.special_result := specialResult_s5
  val fp_c = fix2fp.io.fp_c

  io.out := fp_c
  // delay valid_in 6 cycles
  io.valid_out := ShiftRegister(io.valid_in, 6)
}

class fp_ma(cfg:FPConfig) extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(cfg.width.W))
    val b = Input(UInt(cfg.width.W))
    val c = Input(UInt(cfg.width.W))
    val valid_in = Input(Bool())
    val d = Output(UInt(cfg.width.W))
    // status flags
    val valid_out = Output(Bool())})

    // unpack inputs
    val (sign_a, exp_a, frac_a) = FPHelpers.unpack(io.a, cfg)
    val (sign_b, exp_b, frac_b) = FPHelpers.unpack(io.b, cfg)
    val (sign_c, exp_c, frac_c) = FPHelpers.unpack(io.c, cfg)
    
    val fp_a = io.a.asTypeOf(new fpNumber(cfg))
    val fp_b = io.b.asTypeOf(new fpNumber(cfg))
    val fp_c = io.c.asTypeOf(new fpNumber(cfg))
    val fpNumArray = Seq(fp_a, fp_b, fp_c)
    // special encodings
    val specialSignalsa = FPHelpers.specialEncodingChecks2(cfg, exp_a, frac_a, true.B)
    val specialSignalsb = FPHelpers.specialEncodingChecks2(cfg, exp_b, frac_b, true.B)
    val specialSignalsc = FPHelpers.specialEncodingChecks2(cfg, exp_c, frac_c, true.B)
    val specialSignalsArray = Seq(
      specialSignalsa,
      specialSignalsb,
      specialSignalsc
    )

    val (isSpecial, specialResult) = FPHelpers.specialResultGen2(cfg, 2, fpNumArray, specialSignalsArray)
    

    // stage 1
    val sign_ab_s1 = RegNext(sign_a ^ sign_b)
    val sign_c_s1 = RegNext(sign_c)
    val exp_a_s1 = RegNext(exp_a + specialSignalsa.isSubnormal.asUInt)
    val exp_b_s1 = RegNext(exp_b + specialSignalsb.isSubnormal.asUInt)
    val exp_c_s1 = RegNext(exp_c + specialSignalsc.isSubnormal.asUInt)
    val frac_a_s1 = RegNext(Mux(specialSignalsa.isSubnormal, Cat(0.U(1.W), frac_a), Cat(1.U(1.W), frac_a)))
    val frac_b_s1 = RegNext(Mux(specialSignalsb.isSubnormal, Cat(0.U(1.W), frac_b), Cat(1.U(1.W), frac_b)))
    val frac_c_s1 = RegNext(Mux(specialSignalsc.isSubnormal, Cat(0.U(1.W), frac_c), Cat(1.U(1.W), frac_c)))

    // stage 2
    val expAdd_s2 = RegNext(exp_a_s1 +& exp_b_s1)
    val fracMul_s2 = RegNext(frac_a_s1 * frac_b_s1)
    val exp_c_s2 = RegNext(exp_c_s1)
    val frac_c_s2 = RegNext(frac_c_s1)
    val sign_ab_s2 = RegNext(sign_ab_s1)
    val sign_c_s2 = RegNext(sign_c_s1)

    // compare the exp
    val exp_compare = expAdd_s2 >= (exp_c_s2 +& cfg.bias.U)
    val exp_same = expAdd_s2 === (exp_c_s2 +& cfg.bias.U)
    val exp_sub = Mux(exp_same, 0.U, Mux(exp_compare, expAdd_s2 -& (exp_c_s2 +& cfg.bias.U), (exp_c_s2 +& cfg.bias.U) -& expAdd_s2))(cfg.expBits+1-1,0)
    
    // stage 3
    val sign0_s3 = Reg(UInt(1.W))
    val sign1_s3 = Reg(UInt(1.W))
    val exp0_s3 = Reg(UInt((cfg.expBits+2).W))
    val exp1_s3 = Reg(UInt((cfg.expBits+2).W))
    val frac0_s3 = Reg(UInt((cfg.sigBits*2).W))
    val frac1_s3 = Reg(UInt((cfg.sigBits*2).W))

    when(exp_compare){
      sign0_s3 := sign_ab_s2
      sign1_s3 := sign_c_s2
      exp0_s3 := expAdd_s2 -& cfg.bias.U
      exp1_s3 := exp_c_s2
      frac0_s3 := fracMul_s2 // 2bit整数位
      frac1_s3 := Cat(0.U(1.W), frac_c_s2, 0.U(cfg.fracBits.W)) // 补0,对齐变成2bit整数位
    } .otherwise{
      sign0_s3 := sign_c_s2
      sign1_s3 := sign_ab_s2
      exp0_s3 := exp_c_s2
      exp1_s3 := expAdd_s2 -& cfg.bias.U
      frac0_s3 := Cat(0.U(1.W), frac_c_s2, 0.U(cfg.fracBits.W)) // 补0,对齐变成2bit整数位
      frac1_s3 := fracMul_s2 // 2bit整数位
    }
    val exp_sub_s3 = RegNext(exp_sub)

    // stage 4
    val frac1_shifted_round = Wire(UInt((cfg.sigBits*2*2).W))
    val frac1_tmp = Wire(UInt((cfg.sigBits*2+1).W)) // 多1bit，用于保留移掉的位的舍入 // 2bit整数位
    
    when(exp_sub_s3 >= (cfg.sigBits*2).U){
      frac1_shifted_round := frac1_s3.orR // 如果移位数大于等于尾数位宽，则表示被移位的数值全部移出，需要对移出的部分进行舍入
    } .otherwise{ // 如果移位数小于尾数位宽，就正常右移
      frac1_shifted_round := Cat(frac1_s3, 0.U((cfg.sigBits*2).W)) >> exp_sub_s3
    }
    frac1_tmp := Cat(frac1_shifted_round(cfg.sigBits*2*2-1, cfg.sigBits*2), frac1_shifted_round(cfg.sigBits*2-1,0).orR) // 取高cfg.sigBits*2位作为尾数，低位进行或运算作为舍入位

    val sign0_s4 = RegNext(sign0_s3)
    val sign1_s4 = RegNext(sign1_s3)
    val exp0_s4 = RegNext(exp0_s3)
    val exp1_s4 = RegNext(exp1_s3)
    val frac0_s4 = RegNext( Cat( frac0_s3, 0.U(1.W) ) ) // 2bit整数位
    val frac1_s4 = RegNext(frac1_tmp) // 2bit整数位

    // stage 5
    val exp_s5 = RegNext(exp0_s4)
    val sign_s5 = Reg(UInt(1.W))
    val frac_s5 = Reg(UInt((cfg.sigBits*2+2).W)) // 3bit 整数位
    // 如果相加的两个数的符号位相同
    when(sign0_s4 === sign1_s4){
      sign_s5 := sign0_s4
      frac_s5 := frac0_s4 +& frac1_s4 
    } .elsewhen(!sign0_s4.asBool && sign1_s4.asBool){ // 指数较大的数的符号位——sign0为正，sign1为负
      when(frac0_s4 >= frac1_s4){ // 如果 正数的尾数 >= 负数的尾数
        frac_s5 := frac0_s4 -& frac1_s4
        sign_s5 := 0.U
      } .otherwise{ // 如果 负数的尾数 > 正数的尾数
        frac_s5 := frac1_s4 -& frac0_s4
        sign_s5 := 1.U
      }
    } .otherwise{ // 指数较大的数的符号位sign0为负，sign1为正
      when(frac0_s4 >= frac1_s4){ // 如果 负数的尾数 >= 正数的尾数
        frac_s5 := frac0_s4 -& frac1_s4
        sign_s5 := Mux(frac0_s4 === frac1_s4, 0.U, 1.U) // 如果相等，结果为正0
      } .otherwise{ // 如果 正数的尾数 < 负数的尾数
        frac_s5 := frac1_s4 -& frac0_s4
        sign_s5 := 0.U
      }
    }

    val sign_s8 = ShiftRegister(sign_s5, 3)
    val valid_s5 = ShiftRegister(io.valid_in, 5)
    val isSpecial_s8 = ShiftRegister(isSpecial, 8)
    val specialResult_s8 = ShiftRegister(specialResult, 8)

    // stage 6 ~ stage 9
    val fix2fp = Module(new fix2fp(cfg, cfg.sigBits*2+2, cfg.fracBits*2, 1))
} 

object FPMultiply extends App {
  val cfg = FPConfig(5, 10)
  ChiselStage.emitSystemVerilogFile(
    new FPMultiply(cfg),
    firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info", "-default-layer-specialization=enable")
  )
}