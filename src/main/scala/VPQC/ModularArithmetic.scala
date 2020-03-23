
package VPQC
import chisel3._
import chisel3.util._

class ModIO extends Bundle
  with HasCommonParameters {
  // two operands
  val a = Input(UInt(DataWidth.W))
  val b = Input(UInt(DataWidth.W))
  // modulus
  val m = Input(UInt(DataWidth.W))
  // reduced value
  val c = Output(UInt(DataWidth.W))
}

class ModMulIO extends ModIO {
  // precomputed value
  val u = Input(UInt((DataWidth + 2).W))
  val n = Input(UInt(5.W))
}

class ModAdd extends Module
  with HasCommonParameters {
  val io = IO(new ModIO)

  val ctmp1 = Wire(UInt((DataWidth + 1).W))
  ctmp1 := io.a +& io.b
  val ctmp2 = Wire(UInt((DataWidth + 1).W))
  ctmp2 := ctmp1 -& io.m
  val flag = ctmp2(DataWidth)
  io.c := Mux(flag, ctmp1(DataWidth - 1, 0), ctmp2(DataWidth - 1, 0))
}

object ModAdd {
  def apply(a: UInt, b: UInt, m: UInt): UInt = {
    val inst = Module(new ModAdd())
    inst.io.a := a
    inst.io.b := b
    inst.io.m := m
    inst.io.c
  }
}

class ModSub extends Module
  with HasCommonParameters {
  val io = IO(new ModIO)

  val ctmp1 = Wire(UInt((DataWidth + 1).W))
  ctmp1 := io.a -& io.b
  val ctmp2 = Wire(UInt((DataWidth + 1).W))
  ctmp2 := ctmp1 +& io.m
  val flag = ctmp1(DataWidth)
  io.c := Mux(flag, ctmp2(DataWidth - 1, 0), ctmp1(DataWidth - 1, 0))
}

object ModSub {
  def apply(a: UInt, b: UInt, m: UInt): UInt = {
    val inst = Module(new ModSub())
    inst.io.a := a
    inst.io.b := b
    inst.io.m := m
    inst.io.c
  }
}

class ModMul extends Module
  with HasCommonParameters {
  val io = IO(new ModMulIO)

  val a = io.a
  val b = io.b
  val m = io.m
  val u = io.u
  val n = io.n

  // process 1
  val c = a * b
  val shift = c >> (n - 2.U)

  // process 2
  val mul1 = u * shift.asUInt()

  // process 3
  val qGuess = mul1 >> (n + 3.U)
  val mul2 = io.m * qGuess.asUInt()

  // process 4
  val z = c - mul2
  io.c := Mux(z < io.m, z, z - io.m)
}

object ModMul {
  def apply(a: UInt, b: UInt, m: UInt, u: UInt, n: UInt): UInt = {
    val inst = Module(new ModMul())
    inst.io.a := a
    inst.io.b := b
    inst.io.m := m
    inst.io.u := u
    inst.io.n := n
    inst.io.c
  }
}

// Note: This Module can be eliminated by sharing the
//       hardware resources in NTT Module
class VectorArithIO extends Bundle
  with HasCommonParameters {
  val valid = Input(Bool())
  // config
//  val m = Input(UInt(DataWidth.W))
//  val u = Input(UInt((DataWidth + 2).W))
//  val n = Input(UInt(5.W))
  val csrs = Input(new CSRIO)
  // input
  val addA = Input(Vec(ML, UInt(DataWidth.W)))
  val addB = Input(Vec(ML, UInt(DataWidth.W)))
  val subA = Input(Vec(ML, UInt(DataWidth.W)))
  val subB = Input(Vec(ML, UInt(DataWidth.W)))
  val mulA = Input(Vec(ML, UInt(DataWidth.W)))
  val mulB = Input(Vec(ML, UInt(DataWidth.W)))
  // output
  val addRes = Output(Vec(ML, UInt(DataWidth.W)))
  val subRes = Output(Vec(ML, UInt(DataWidth.W)))
  val mulRes = Output(Vec(ML, UInt(DataWidth.W)))

  val done = Output(Bool())
  val busy = Output(Bool())
  val wb = Output(Bool())
}

class VectorArith extends Module
  with HasCommonParameters {
  val io = IO(new VectorArithIO())


  val adds = VecInit(Seq.fill(ML)(Module(new ModAdd()).io))
  val subs = VecInit(Seq.fill(ML)(Module(new ModSub()).io))
  val muls = VecInit(Seq.fill(ML)(Module(new ModMul()).io))
  for (i <- 0 until ML) {
    adds(i).a := io.addA(i)
    adds(i).b := io.addB(i)
    adds(i).m := io.csrs.csrModulusq
    io.addRes(i) := adds(i).c
    subs(i).a := io.subA(i)
    subs(i).b := io.subB(i)
    subs(i).m := io.csrs.csrModulusq
    io.subRes(i) := subs(i).c
    muls(i).a := io.mulA(i)
    muls(i).b := io.mulB(i)
    muls(i).m := io.csrs.csrModulusq
    muls(i).u := io.csrs.csrBarretu
    muls(i).n := io.csrs.csrModulusLen
    io.mulRes(i) := muls(i).c
  }

  io.done := RegNext(io.valid)
  io.busy := false.B
  io.wb := true.B
}

object elaborateMod extends App {
  chisel3.Driver.execute(args, () => new VectorArith)
}