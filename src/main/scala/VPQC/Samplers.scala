
package VPQC

import chisel3._
import chisel3.util._

class BinomialSamplerIO extends Bundle
  with HasSamplerParameters
  with HasCommonParameters {
  val data1 = Input(UInt(DataWidth.W))
  val data2 = Input(UInt(DataWidth.W))
  val k     = Input(UInt(3.W))
  val q     = Input(UInt(DataWidth.W))
  val dataOut = Output(UInt(DataWidth.W))
}

class BinomialSampler extends Module
  with HasSamplerParameters
  with HasCommonParameters {
  val io = IO(new BinomialSamplerIO())

  val data1pop2 = Wire(Vec(DataWidth / 2, UInt(2.W)))
  val data2pop2 = Wire(Vec(DataWidth / 2, UInt(2.W)))
  for (i <- 0 until DataWidth / 2) {
    data1pop2(i) := io.data1(2 * i + 1) + io.data1(2 * i)
    data2pop2(i) := io.data2(2 * i + 1) + io.data2(2 * i)
  }

  val data1pop4 = Wire(Vec(DataWidth / 4, UInt(3.W)))
  val data2pop4 = Wire(Vec(DataWidth / 4, UInt(3.W)))
  for (i <- 0 until DataWidth / 4) {
    data1pop4(i) := data1pop2(2 * i + 1) + data1pop2(2 * i)
    data2pop4(i) := data2pop2(2 * i + 1) + data2pop2(2 * i)
  }

  val data1pop8 = Wire(Vec(DataWidth / 8, UInt(4.W)))
  val data2pop8 = Wire(Vec(DataWidth / 8, UInt(4.W)))
  for (i <- 0 until DataWidth / 8) {
    data1pop8(i) := data1pop4(2 * i + 1) + data1pop4(2 * i)
    data2pop8(i) := data2pop4(2 * i + 1) + data2pop4(2 * i)
  }

  val data1pop16 = Wire(UInt(5.W))
  val data2pop16 = Wire(UInt(5.W))
  if (DataWidth == 16) {
    data1pop16 := data1pop8(1) + data1pop8(0)
    data2pop16 := data2pop8(1) + data2pop8(0)

  } else {
    data1pop16 := data1pop8(0)
    data2pop16 := data2pop8(0)

  }

  // two minus operands
  val op1 = WireInit(0.U(5.W))
  val op2 = WireInit(0.U(5.W))

  switch(io.k) {
    // k = 1
    is(0.U) {
      op1 := io.data1(0)
      op2 := io.data2(0)
    }
    // k = 2
    is(1.U) {
      op1 := data1pop2(0)
      op2 := data2pop2(0)
    }
    // k = 4
    is(2.U) {
      op1 := data1pop4(0)
      op2 := data2pop4(0)
    }
    // k = 8
    is(3.U) {
      op1 := data1pop8(0)
      op2 := data2pop8(0)
    }
    // k = 16
    is(4.U) {
      op1 := data1pop16(0)
      op2 := data2pop16(0)
    }
  }

  val diff = op1 < op2
  val out = Mux(diff, op1 + io.q - op2, op1 - op2)
  io.dataOut := out
}

class RejectionSamplerIO extends Bundle
  with HasSamplerParameters
  with HasCommonParameters {
  val dataIn = Input(UInt((DataWidth * 2).W))
  val n = Input(UInt(5.W))
  val m = Input(UInt(DataWidth.W))
  val u = Input(UInt((DataWidth + 2).W))
  val bound = Input(UInt((DataWidth * 2).W))
  val dataOut = Output(UInt(DataWidth.W))
  val valid = Output(Bool())
}

// small rejection rate
class RejectionSampler extends Module
  with HasSamplerParameters {
  val io = IO(new RejectionSamplerIO())

  val failed = !(io.dataIn < io.bound)
  io.dataOut := Mux(failed, 0.U, MR(io.dataIn, io.n, io.m, io.u))
  io.valid := !failed
}

class SamplersIO extends Bundle
  with HasSamplerParameters
  with HasKeccakParameters
  with HasCommonParameters {
  // 0: rejection sample  1: binomial sample
  val valid = Input(Bool())
  val mode = Input(Bool())

  // from register
  val vectorReg1 = Input(Vec(ML, UInt(DataWidth.W)))
  val vectorReg2 = Input(Vec(ML, UInt(DataWidth.W)))

  // csr interface
  val csrs = Input(new CSRIO)

  // output
  val sampledData = Output(Vec(SamplerNum, UInt(DataWidth.W)))
  val sampledDataValid = Output(Bool())
  val done = Output(Bool())
  val busy = Output(Bool())
  val wb = Output(Bool())
}

class Samplers extends Module
  with HasSamplerParameters
  with HasCommonParameters {
  val io = IO(new SamplersIO())

  val rejectionSamplers = VecInit(Seq.fill(SamplerNum)(Module(new RejectionSampler()).io))
  val rejectionData = Wire(Vec(SamplerNum, UInt(DataWidth.W)))
  for (i <- 0 until SamplerNum) {
    rejectionSamplers(i).dataIn := Cat(io.vectorReg2(i), io.vectorReg1(i))
    rejectionSamplers(i).bound := io.csrs.csrBound
    rejectionSamplers(i).n := io.csrs.csrModulusLen
    rejectionSamplers(i).m := io.csrs.csrModulusq
    rejectionSamplers(i).u := io.csrs.csrBarretu
    rejectionData(i) := rejectionSamplers(i).dataOut
  }

  val binomialSamplers = VecInit(Seq.fill(SamplerNum)(Module(new BinomialSampler()).io))
  val binomialData = Wire(Vec(SamplerNum, UInt(DataWidth.W)))
  for (i <- 0 until SamplerNum) {
    binomialSamplers(i).data1 := io.vectorReg1(i)
    binomialSamplers(i).data2 := io.vectorReg2(i)
    binomialSamplers(i).k := io.csrs.csrBinomialk
    binomialSamplers(i).q := io.csrs.csrModulusq
    binomialData(i) := binomialSamplers(i).dataOut
  }

  io.sampledData := Mux(io.mode, binomialData, rejectionData)
  val rejectionValid = (0 until SamplerNum).map(i => rejectionSamplers(i).valid).reduce(_ && _)
  io.sampledDataValid := Mux(io.mode, true.B, rejectionValid)
  io.done := RegNext(io.valid)
  io.busy := false.B
  io.wb := true.B
}

object elaborateSamplers extends App {
  chisel3.Driver.execute(args, () => new Samplers())
}
