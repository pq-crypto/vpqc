
package utility

import chisel3._
import chisel3.util._

// this is a synchronous-read, synchronous-write memory

class SyncRam(dep: Int, dw: Int) extends Module{

  val io = IO(new Bundle {
    //control signal
    val re = Input(Bool())
    val we = Input(Bool())

    //data signal
    val ra = Input(UInt(log2Ceil(dep).W))
    val wa = Input(UInt(log2Ceil(dep).W))
    val di = Input(UInt(dw.W))
    val dout = Output(UInt(dw.W))
  })

  // Create a synchronous-read, synchronous-write memory (like in FPGAs).
  val mem = SyncReadMem(dep, UInt(dw.W))
  // Create one write port and one read port.
  when (io.we) {
    mem.write(io.wa, io.di)
  }

  io.dout := mem.read(io.ra, io.re)
}
