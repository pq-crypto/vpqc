
package VPQC

import chisel3._
import chisel3.util._

// Current simplified version does not apply
// resource sharing between ntt module and vector arithmetic module
class PQCExuIO extends Bundle
  with HasPQCInstructions
  with HasCommonParameters
  with HasNTTParameters {
  // for SHA3, from outside
  val seed = Input(new stateArray())
  val seedWrite = Input(Bool())
  // for ConstRam, from outside
  val twiddleData = Input(UInt((DataWidth * ButterflyNum).W))
  val twiddleAddr = Input(UInt(log2Ceil(Dimension / ButterflyNum).W))
  val twiddleWrite = Input(Bool())
  val wpos = Output(Bool())

  // pipeline
  val valid = Input(Vec(INSTR_QUANTITY, Bool()))
//  val rs1 = Input(UInt(64.W))
//  val rs2 = Input(UInt(64.W))
  val vrs1 = Input(Vec(ML, UInt(DataWidth.W)))
  val vrs2 = Input(Vec(ML, UInt(DataWidth.W)))
  val csrs = Input(new CSRIO)
  val done = Output(Bool())
  val busy = Output(Bool())
  val wb = Output(Bool())
  val vres1 = Output(Vec(ML, UInt(DataWidth.W)))
  val vres2 = Output(Vec(ML, UInt(DataWidth.W)))
}

class PQCExu extends Module
  with HasPQCInstructions
  with HasCommonParameters
  with HasNTTParameters {
  val io = IO(new PQCExuIO())

  val vrs1 = io.vrs1
  val vrs2 = io.vrs2

  val SHA3Core = Module(new KeccakWithFifo)
  SHA3Core.io.seed := io.seed
  SHA3Core.io.seedWrite := io.seedWrite
  SHA3Core.io.valid := io.valid(INSTR_FETCHRN)

  val Samplers = Module(new Samplers)
  Samplers.io.vectorReg1 := vrs1
  Samplers.io.vectorReg2 := vrs2
  Samplers.io.csrs := io.csrs
  Samplers.io.valid := io.valid(INSTR_SAMPLEBINOMIAL) || io.valid(INSTR_SAMPLEREJECTION)
  Samplers.io.mode := io.valid(INSTR_SAMPLEBINOMIAL) || !io.valid(INSTR_SAMPLEREJECTION)

  val VecArith = Module(new VectorArith)
  VecArith.io.addA := vrs1
  VecArith.io.addB := vrs2
  VecArith.io.subA := vrs1
  VecArith.io.subB := vrs2
  VecArith.io.mulA := vrs1
  VecArith.io.mulB := vrs2
  VecArith.io.csrs := io.csrs
  VecArith.io.valid := io.valid(INSTR_VADD) || io.valid(INSTR_VSUB) || io.valid(INSTR_VMUL)
  val vecData = Mux(io.valid(INSTR_VADD), VecArith.io.addRes,
    Mux(io.valid(INSTR_VSUB), VecArith.io.subRes,
      Mux(io.valid(INSTR_VMUL), VecArith.io.mulRes, VecInit(Seq.fill(ML)(0.U(DataWidth.W))))))

  val NTT = Module(new NTT)
  NTT.io.wa := io.twiddleAddr
  NTT.io.di := io.twiddleData
  NTT.io.we := io.twiddleWrite
  NTT.io.vectorReg1 := vrs1
  NTT.io.vectorReg2 := vrs2
  NTT.io.csrs := io.csrs
  NTT.io.valid := io.valid(INSTR_BUTTERFLY) || io.valid(INSTR_IBUTTERFLY)
  NTT.io.mode := !io.valid(INSTR_BUTTERFLY) && io.valid(INSTR_IBUTTERFLY)


  val done = SHA3Core.io.done || Samplers.io.done || NTT.io.done || VecArith.io.done
  val busy = SHA3Core.io.busy || Samplers.io.busy || NTT.io.busy || VecArith.io.done
  val NTTData1 = Wire(Vec(ButterflyNum , UInt(DataWidth.W)))
  val NTTData2 = Wire(Vec(ButterflyNum , UInt(DataWidth.W)))
  for (i <- 0 until ButterflyNum) {
    NTTData1(i) := NTT.io.dataOut(i)
  }
  for (i <- ButterflyNum until ButterflyNum*2) {
    NTTData2(i-ButterflyNum) := NTT.io.dataOut(i)
  }

  val vres1 = MuxCase(SHA3Core.io.prn, Array(
    Samplers.io.done -> Samplers.io.sampledData,
    VecArith.io.done -> vecData,
    NTT.io.done -> NTTData1
  ))
  val vres2 = MuxCase(SHA3Core.io.prn, Array(
    Samplers.io.done -> Samplers.io.sampledData,
    VecArith.io.done -> vecData,
    NTT.io.done -> NTTData2
  ))

  val wb = MuxCase(false.B, Array(
    SHA3Core.io.done -> SHA3Core.io.wb,
    Samplers.io.done -> Samplers.io.wb,
    VecArith.io.done -> VecArith.io.wb,
    NTT.io.done -> NTT.io.wb
  ))

  io.done := done
  io.vres1 := vres1
  io.vres2 := vres2
  io.busy := busy
  io.wb := wb
  io.wpos := NTT.io.done
}

class PQCExuNoMemIO extends Bundle
  with HasPQCInstructions
  with HasCommonParameters
  with HasNTTParameters {
  // for SHA3, from outside
  val seed = Input(new stateArray())
  val seedWrite = Input(Bool())
  // prefetch FIFO IO
  val writeData = Output(UInt((ML * DataWidth).W))
  val writeEnable = Output(Bool())
  val readEnable = Output(Bool())
  val readData = Input(UInt((ML * DataWidth).W))
  val readEmpty = Input(Bool())
  val writeFull = Input(Bool())

  // for ConstRam, from outside
  // ram from outside
  val ra = Output(UInt(log2Ceil(Dimension / ButterflyNum).W))
  val re = Output(Bool())
  val di = Input(UInt((ButterflyNum * DataWidth).W))

  val wpos = Output(Bool())

  // pipeline
  val valid = Input(Vec(INSTR_QUANTITY, Bool()))
  //  val rs1 = Input(UInt(64.W))
  //  val rs2 = Input(UInt(64.W))
  val vrs1 = Input(Vec(ML, UInt(DataWidth.W)))
  val vrs2 = Input(Vec(ML, UInt(DataWidth.W)))
  val csrs = Input(new CSRIO)
  val done = Output(Bool())
  val busy = Output(Bool())
  val wb = Output(Bool())
  val vres1 = Output(Vec(ML, UInt(DataWidth.W)))
  val vres2 = Output(Vec(ML, UInt(DataWidth.W)))
}

// resource sharing
class PQCExuNoMem extends Module
  with HasPQCInstructions
  with HasCommonParameters
  with HasNTTParameters {
  val io = IO(new PQCExuNoMemIO())

  val vrs1 = io.vrs1
  val vrs2 = io.vrs2

  val SHA3Core = Module(new KeccakNoFifo)
  SHA3Core.io.seed := io.seed
  SHA3Core.io.seedWrite := io.seedWrite
  SHA3Core.io.valid := io.valid(INSTR_FETCHRN)
  SHA3Core.io.readEmpty := io.readEmpty
  SHA3Core.io.readData := io.readData
  SHA3Core.io.writeFull := io.writeFull
  io.writeData := SHA3Core.io.writeData
  io.writeEnable := SHA3Core.io.writeEnable
  io.readEnable := SHA3Core.io.readEnable

  val Samplers = Module(new Samplers)
  Samplers.io.vectorReg1 := vrs1
  Samplers.io.vectorReg2 := vrs2
  Samplers.io.csrs := io.csrs
  Samplers.io.valid := io.valid(INSTR_SAMPLEBINOMIAL) || io.valid(INSTR_SAMPLEREJECTION)
  Samplers.io.mode := io.valid(INSTR_SAMPLEBINOMIAL) || !io.valid(INSTR_SAMPLEREJECTION)

  val NTT = Module(new NTTWithoutRamShare)
  NTT.io.di := io.di
  io.ra := NTT.io.ra
  io.re := NTT.io.re

  NTT.io.vectorReg1 := vrs1
  NTT.io.vectorReg2 := vrs2
  NTT.io.csrs := io.csrs
  NTT.io.valid := io.valid(INSTR_BUTTERFLY) || io.valid(INSTR_IBUTTERFLY)
  NTT.io.vecValid(0) := io.valid(INSTR_VADD)
  NTT.io.vecValid(1) := io.valid(INSTR_VSUB)
  NTT.io.vecValid(2) := io.valid(INSTR_VMUL)
  NTT.io.mode := io.valid(INSTR_BUTTERFLY) || !io.valid(INSTR_IBUTTERFLY)


  val done = SHA3Core.io.done || Samplers.io.done || NTT.io.done
  val busy = SHA3Core.io.busy || Samplers.io.busy || NTT.io.busy
  val NTTData1 = Wire(Vec(ButterflyNum , UInt(DataWidth.W)))
  val NTTData2 = Wire(Vec(ButterflyNum , UInt(DataWidth.W)))
  for (i <- 0 until ButterflyNum) {
    NTTData1(i) := NTT.io.dataOut(i)
  }
  for (i <- ButterflyNum until ButterflyNum*2) {
    NTTData2(i-ButterflyNum) := NTT.io.dataOut(i)
  }

  val vres1 = MuxCase(SHA3Core.io.prn, Array(
    Samplers.io.done -> Samplers.io.sampledData,
    NTT.io.done -> NTTData1
  ))
  val vres2 = MuxCase(SHA3Core.io.prn, Array(
    Samplers.io.done -> Samplers.io.sampledData,
    NTT.io.done -> NTTData2
  ))

  val wb = MuxCase(false.B, Array(
    SHA3Core.io.done -> SHA3Core.io.wb,
    Samplers.io.done -> Samplers.io.wb,
    NTT.io.done -> NTT.io.wb
  ))

  io.done := done
  io.vres1 := vres1
  io.vres2 := vres2
  io.busy := busy
  io.wb := wb
  io.wpos := NTT.io.done && NTT.io.valid
}