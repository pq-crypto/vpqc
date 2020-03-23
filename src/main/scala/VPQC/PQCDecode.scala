
package VPQC

import chisel3._

class PQCInstruction extends Bundle {
  val funct = UInt(7.W)
  val rs2 = UInt(5.W)
  val rs1 = UInt(5.W)
  val xd = Bool()
  val xs1 = Bool()
  val xs2 = Bool()
  val rd = UInt(5.W)
  val opcode = UInt(7.W)
}

class PQCDecode extends Module
  with HasPQCInstructions
  with HasCommonParameters {
  val io = IO(new Bundle{
    val instr = Input(new PQCInstruction)
    val rs1 = Input(UInt(32.W))
    val rs2 = Input(UInt(32.W))
    val in_fire = Input(Bool())
    val result = Output(Vec(INSTR_QUANTITY, Bool()))
    val busy = Input(Bool())
    val pqcBusy = Output(Bool())
    val srs1 = Output(UInt(32.W))
    val srs2 = Output(UInt(32.W))
    val vrs1idx = Output(UInt(5.W))
    val vrs2idx = Output(UInt(5.W))
    val vrdidx = Output(UInt(5.W))
    val fire = Output(Bool())
  })
  for(i <- 0 until INSTR_QUANTITY){
    io.result(i) := io.instr.funct === i.U
  }
  io.pqcBusy := io.busy
  io.vrs1idx := io.instr.rs1
  io.vrs2idx := io.instr.rs2
  io.vrdidx := io.instr.rd
  io.srs1 := io.rs1
  io.srs2 := io.rs2
  io.fire := io.in_fire
}

