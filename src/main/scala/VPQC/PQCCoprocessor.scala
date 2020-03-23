
package VPQC

import chisel3._
import chisel3.util._
import utility._

// standalone coprocessor

class PQCCoprocessorIO extends Bundle
  with HasCommonParameters
  with HasNTTParameters
  with HasPQCInstructions {
  // for decode
  val instr = Input(new PQCInstruction)
  val rs1 = Input(UInt(32.W))
  val rs2 = Input(UInt(32.W))
  val rd = Output(UInt(32.W))
  val rdw = Output(Bool())
  val in_fire = Input(Bool())
  val rdy = Output(Bool())
  val busy = Output(Bool())

  // for Keccak core
  val seed = Input(new stateArray())
  val seedWrite = Input(Bool())

  // for twiddle factors / const RAM, from outside
  val twiddleData = Input(UInt((DataWidth * ButterflyNum).W))
  val twiddleAddr = Input(UInt(log2Ceil(Dimension / ButterflyNum).W))
  val twiddleWrite = Input(Bool())

  val constReadEnable = Input(Bool())
  val constReadAddr = Input(UInt(log2Ceil(2 * Dimension / ButterflyNum).W))
  val constWrite = Input(Bool())

  // vector interface
  val vectorOut = Output(Vec(ML, UInt(DataWidth.W)))
  val vectorWriteAddr = Output(UInt(32.W))
  val vectorWriteEnable = Output(Bool())

  val vectorIn = Input(Vec(ML, UInt(DataWidth.W)))
  val vectorReadaddr = Output(UInt(32.W))
  val vectorReadEnable = Output(Bool())
}

class CSRIO extends Bundle
  with HasCommonParameters {
  val csrBarretu = UInt((DataWidth + 2).W)
  val csrBound = UInt((DataWidth * 2).W)
  val csrBinomialk = UInt(3.W)
  val csrModulusq = UInt(DataWidth.W)
  val csrModulusLen = UInt(5.W)
  /**
    *
    *      9 ... 6        5 ... 0
    *     stage_cfg      iter_cfg
    */

  val csrButterflyCtrl = new Bundle {
    val stageCfg = UInt(4.W)
    val iterCfg = UInt(6.W)
  }
  val csrLsBaseAddr = UInt(32.W)
}

class PQCCoprocessor extends Module
  with HasCommonParameters
  with HasPQCInstructions {
  val io = IO(new PQCCoprocessorIO)

  // decode
  val decoder = Module(new PQCDecode)
  decoder.io.instr := io.instr
  decoder.io.rs1 := io.rs1
  decoder.io.rs2 := io.rs2
  decoder.io.in_fire := io.in_fire
  val doinstr = decoder.io.result

  // CSRs
  val csrBarretu = RegInit(0.U((DataWidth + 2).W))
  val csrBound = RegInit(0.U((DataWidth * 2).W))
  val csrBinomialk = RegInit(0.U(3.W))
  val csrModulusq = RegInit(0.U(DataWidth.W))
  val csrModulusLen = RegInit(0.U(5.W))
  val csrButterflyCtrl = RegInit(0.U(10.W))
  val csrLsBaseAddr = RegInit(0.U(32.W))

  // vector SRAM
  val vecSRAM = Module(new VectorRegister2Port)
  vecSRAM.io.vectorReadAddr1 := decoder.io.vrs1idx
  vecSRAM.io.vectorReadAddr2 := decoder.io.vrs2idx
  val vecop1 = vecSRAM.io.vectorReadValue1
  val vecop2 = vecSRAM.io.vectorReadValue2

  // const RAM
  val constRAM = Module(new SyncRam(dep = 64, dw = DataWidth * ML))
  constRAM.io.re := io.constReadEnable
  constRAM.io.we := io.constWrite
  constRAM.io.ra := io.constReadAddr
  constRAM.io.wa := io.twiddleAddr
  constRAM.io.di := io.twiddleData
  val constVal = Wire(Vec(ML, UInt(DataWidth.W)))
  for (i <- 0 until ML) {
    constVal(i) := constRAM.io.dout(DataWidth * i + DataWidth - 1, DataWidth * i)
  }

  // ======================================
  // EXU Stage
  // ======================================
  val exunit = Module(new PQCExu)

  exunit.io.seed := io.seed
  exunit.io.seedWrite := io.seedWrite
  exunit.io.twiddleData := io.twiddleData
  exunit.io.twiddleAddr := io.twiddleAddr
  exunit.io.twiddleWrite := io.twiddleWrite
  for (i <- 0 until INSTR_QUANTITY) {
    exunit.io.valid(i) := doinstr(i) && decoder.io.fire
  }

  exunit.io.vrs1 := vecop1
  exunit.io.vrs2 := Mux(io.constReadEnable, constVal, vecop2)
  exunit.io.csrs.csrBarretu := csrBarretu
  exunit.io.csrs.csrBound := csrBound
  exunit.io.csrs.csrBinomialk := csrBinomialk
  exunit.io.csrs.csrModulusq := csrModulusq
  exunit.io.csrs.csrModulusLen := csrModulusLen
  exunit.io.csrs.csrButterflyCtrl.stageCfg := csrButterflyCtrl(9, 6)
  exunit.io.csrs.csrButterflyCtrl.iterCfg := csrButterflyCtrl(5, 0)
  exunit.io.csrs.csrLsBaseAddr := csrLsBaseAddr

  vecSRAM.io.vectorWriteEnable1 := exunit.io.done && exunit.io.wb
  vecSRAM.io.vectorWriteEnable2 := exunit.io.done && exunit.io.wb && exunit.io.wpos
  vecSRAM.io.vectorWriteData1 := exunit.io.vres1
  vecSRAM.io.vectorWriteData2 := exunit.io.vres2
  vecSRAM.io.vectorWriteAddr1 := Mux(!RegNext((doinstr(INSTR_BUTTERFLY) || doinstr(INSTR_IBUTTERFLY))
                                      && decoder.io.fire), RegNext(decoder.io.vrdidx), RegNext(decoder.io.vrs1idx))
  vecSRAM.io.vectorWriteAddr2 := RegNext(decoder.io.vrs2idx)

  io.rd := 0.U
  io.rdw := false.B
  // CSRRW
  when(decoder.io.fire && doinstr(INSTR_CSRRW)){
    io.rdw:= true.B
    switch(decoder.io.vrs2idx) {
      is(0.U) {
        io.rd := csrBarretu
        csrBarretu := decoder.io.srs1
      }
      is(1.U) {
        io.rd := csrBound
        csrBound := decoder.io.srs1
      }
      is(2.U) {
        io.rd := csrBinomialk
        csrBinomialk := decoder.io.srs1
      }
      is(3.U) {
        io.rd := csrModulusq
        csrModulusq := decoder.io.srs1
      }
      is(4.U) {
        io.rd := csrModulusLen
        csrModulusLen := decoder.io.srs1
      }
      is(5.U) {
        io.rd := csrButterflyCtrl
        csrButterflyCtrl := decoder.io.srs1
      }
      is(6.U) {
        io.rd := csrLsBaseAddr
        csrLsBaseAddr := decoder.io.srs1
      }
    }
  }

  // CSRRWI
  when(decoder.io.fire && doinstr(INSTR_CSRRWI)){
    switch(decoder.io.vrs2idx) {
      is(0.U) {
        csrBarretu := decoder.io.vrs1idx
      }
      is(1.U) {
        csrBound := decoder.io.vrs1idx
      }
      is(2.U) {
        csrBinomialk := decoder.io.vrs1idx
      }
      is(3.U) {
        csrModulusq := decoder.io.vrs1idx
      }
      is(4.U) {
        csrModulusLen := decoder.io.vrs1idx
      }
      is(5.U) {
        csrButterflyCtrl := decoder.io.vrs1idx
      }
      is(6.U) {
        csrLsBaseAddr := decoder.io.vrs1idx
      }
    }
  }

  // additional connect
  decoder.io.busy := exunit.io.busy
  io.rdy := exunit.io.done && !exunit.io.busy
  io.busy := decoder.io.busy

  // memory
  io.vectorReadaddr := csrLsBaseAddr
  io.vectorWriteAddr := csrLsBaseAddr
  io.vectorOut := vecop1

  when(decoder.io.fire && doinstr(INSTR_VLD)) {
    vecSRAM.io.vectorWriteData1 := io.vectorIn
    vecSRAM.io.vectorWriteEnable1 := true.B
  }

  io.vectorReadEnable := decoder.io.fire && doinstr(INSTR_VLD)
  io.vectorWriteEnable := decoder.io.fire && doinstr(INSTR_VST)
}

object elaboratePQCCoprocessor extends App {
  chisel3.Driver.execute(args, () => new PQCCoprocessor())
}

/**
  *
  * ----------------------------------------------------------------------
  *
  *
**/
// for synthesis

class PQCCoprocessorNoMemIO extends Bundle
  with HasCommonParameters
  with HasNTTParameters
  with HasPQCInstructions {
  // for decode
  val instr = Input(new PQCInstruction)
  val rs1 = Input(UInt(32.W))
  val rs2 = Input(UInt(32.W))
  val rd = Output(UInt(32.W))
  val rdw = Output(Bool())
  val in_fire = Input(Bool())
  val rdy = Output(Bool())
  val busy = Output(Bool())

  // for SHA3
  val seed = Input(new stateArray())
  val seedWrite = Input(Bool())
  val writeData = Output(UInt((ML * DataWidth).W))
  val writeEnable = Output(Bool())
  val readEnable = Output(Bool())
  val readData = Input(UInt((ML * DataWidth).W))
  val readEmpty = Input(Bool())
  val writeFull = Input(Bool())

  // for vecop, from outside
  val vectorReadAddr1 = Output(UInt(5.W))
  val vectorReadAddr2 = Output(UInt(5.W))
  val vectorWriteData1 = Output(Vec(ML, UInt(DataWidth.W)))
  val vectorWriteAddr1 = Output(UInt(32.W))
  val vectorWriteEnable1 = Output(Bool())
  val vectorWriteData2 = Output(Vec(ML, UInt(DataWidth.W)))
  val vectorWriteAddr2 = Output(UInt(32.W))
  val vectorWriteEnable2 = Output(Bool())
  val vectorReadValue1 = Input(Vec(ML, UInt(DataWidth.W)))
  val vectorReadValue2 = Input(Vec(ML, UInt(DataWidth.W)))

  // for ConstRam, from outside
  // ram from outside
  val ra = Output(UInt(log2Ceil(Dimension / ButterflyNum).W))
  val re = Output(Bool())
  val di = Input(UInt((ButterflyNum * DataWidth).W))

  // vector interface
  val vectorOut = Output(Vec(ML, UInt(DataWidth.W)))
  val vectorWriteAddr = Output(UInt(32.W))
  val vectorWriteEnable = Output(Bool())

  val vectorIn = Input(Vec(ML, UInt(DataWidth.W)))
  val vectorReadaddr = Output(UInt(32.W))
  val vectorReadEnable = Output(Bool())
}

class PQCCoprocessorNoMem extends Module
  with HasCommonParameters
  with HasPQCInstructions {
  val io = IO(new PQCCoprocessorNoMemIO)

  // decode
  val decoder = Module(new PQCDecode)
  decoder.io.instr := io.instr
  decoder.io.rs1 := io.rs1
  decoder.io.rs2 := io.rs2
  decoder.io.in_fire := io.in_fire
  val doinstr = decoder.io.result

  // CSRs
  val csrBarretu = RegInit(0.U((DataWidth + 2).W))
  val csrBound = RegInit(0.U((DataWidth * 2).W))
  val csrBinomialk = RegInit(0.U(3.W))
  val csrModulusq = RegInit(0.U(DataWidth.W))
  val csrModulusLen = RegInit(0.U(5.W))
  val csrButterflyCtrl = RegInit(0.U(10.W))
  val csrLsBaseAddr = RegInit(0.U(32.W))

  // vector SRAM
  io.vectorReadAddr1 := decoder.io.vrs1idx
  io.vectorReadAddr2 := decoder.io.vrs2idx
  val vecop1 = io.vectorReadValue1
  val vecop2 = io.vectorReadValue2

  // ======================================
  // EXU Stage
  // ======================================
  val exunit = Module(new PQCExuNoMem)

  exunit.io.seed := io.seed
  exunit.io.seedWrite := io.seedWrite
  exunit.io.readEmpty := io.readEmpty
  exunit.io.readData := io.readData
  exunit.io.writeFull := io.writeFull
  io.writeData := exunit.io.writeData
  io.writeEnable := exunit.io.writeEnable
  io.readEnable := exunit.io.readEnable

  exunit.io.di := io.di
  io.re := exunit.io.re
  io.ra := exunit.io.ra
  for (i <- 0 until INSTR_QUANTITY) {
    exunit.io.valid(i) := doinstr(i) && decoder.io.fire
  }

  exunit.io.vrs1 := vecop1
  exunit.io.vrs2 := vecop2
  exunit.io.csrs.csrBarretu := csrBarretu
  exunit.io.csrs.csrBound := csrBound
  exunit.io.csrs.csrBinomialk := csrBinomialk
  exunit.io.csrs.csrModulusq := csrModulusq
  exunit.io.csrs.csrModulusLen := csrModulusLen
  exunit.io.csrs.csrButterflyCtrl.stageCfg := csrButterflyCtrl(9, 6)
  exunit.io.csrs.csrButterflyCtrl.iterCfg := csrButterflyCtrl(5, 0)
  exunit.io.csrs.csrLsBaseAddr := csrLsBaseAddr

  io.vectorWriteEnable1 := exunit.io.done && exunit.io.wb
  io.vectorWriteEnable2 := exunit.io.done && exunit.io.wb && exunit.io.wpos
  io.vectorWriteData1 := exunit.io.vres1
  io.vectorWriteData2 := exunit.io.vres2
  io.vectorWriteAddr1 := Mux(!RegNext((doinstr(INSTR_BUTTERFLY) || doinstr(INSTR_IBUTTERFLY))
                              && decoder.io.fire), RegNext(decoder.io.vrdidx), RegNext(decoder.io.vrs1idx))
  io.vectorWriteAddr2 := RegNext(decoder.io.vrs2idx)

  io.rd := 0.U
  io.rdw := false.B
  // CSRRW
  when(decoder.io.fire && doinstr(INSTR_CSRRW)){
    io.rdw := true.B
    switch(decoder.io.vrs2idx) {
      is(0.U) {
        io.rd := csrBarretu
        csrBarretu := decoder.io.srs1
      }
      is(1.U) {
        io.rd := csrBound
        csrBound := decoder.io.srs1
      }
      is(2.U) {
        io.rd := csrBinomialk
        csrBinomialk := decoder.io.srs1
      }
      is(3.U) {
        io.rd := csrModulusq
        csrModulusq := decoder.io.srs1
      }
      is(4.U) {
        io.rd := csrModulusLen
        csrModulusLen := decoder.io.srs1
      }
      is(5.U) {
        io.rd := csrButterflyCtrl
        csrButterflyCtrl := decoder.io.srs1
      }
      is(6.U) {
        io.rd := csrLsBaseAddr
        csrLsBaseAddr := decoder.io.srs1
      }
    }
  }

  // CSRRWI
  when(decoder.io.fire && doinstr(INSTR_CSRRWI)){
    switch(decoder.io.vrs2idx) {
      is(0.U) {
        csrBarretu := decoder.io.vrs1idx
      }
      is(1.U) {
        csrBound := decoder.io.vrs1idx
      }
      is(2.U) {
        csrBinomialk := decoder.io.vrs1idx
      }
      is(3.U) {
        csrModulusq := decoder.io.vrs1idx
      }
      is(4.U) {
        csrModulusLen := decoder.io.vrs1idx
      }
      is(5.U) {
        csrButterflyCtrl := decoder.io.vrs1idx
      }
      is(6.U) {
        csrLsBaseAddr := decoder.io.vrs1idx
      }
    }
  }

  // additional connect
  decoder.io.busy := exunit.io.busy
  io.rdy := exunit.io.done && !exunit.io.busy
  io.busy := decoder.io.busy

  // memory
  io.vectorReadaddr := csrLsBaseAddr
  io.vectorWriteAddr := csrLsBaseAddr
  io.vectorOut := vecop1

  when(decoder.io.fire && doinstr(INSTR_VLD)) {
    io.vectorWriteData1 := io.vectorIn
    io.vectorWriteEnable1 := true.B
  }

  io.vectorReadEnable := decoder.io.fire && doinstr(INSTR_VLD)
  io.vectorWriteEnable := decoder.io.fire && doinstr(INSTR_VST)
}

object elaboratePQCCoprocessorNoMem extends App {
  chisel3.Driver.execute(args, () => new PQCCoprocessorNoMem())
}