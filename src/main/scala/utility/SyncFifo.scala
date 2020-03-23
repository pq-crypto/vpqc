
package utility

import chisel3._
import chisel3.util._

class SyncFifo[T <: Data](dep: Int, dataType: T) extends Module {
  val io = IO(new Bundle {
    val writeData = Input(dataType)
    val writeEnable = Input(Bool())
    val readEnable = Input(Bool())
    val readData = Output(dataType)
    val readEmpty = Output(Bool())
    val writeFull = Output(Bool())
  })

  val fifo = SyncReadMem(dep, dataType)
  val readPtr = RegInit(0.U((log2Ceil(dep) + 1).W))
  val writePtr = RegInit(0.U((log2Ceil(dep) + 1).W))
  val readAddr = readPtr(log2Ceil(dep)-1, 0)
  val writeAddr = writePtr(log2Ceil(dep)-1, 0)

  io.readEmpty := (readPtr === writePtr)
  io.writeFull := (readAddr === writeAddr) && (readPtr(log2Ceil(dep)) ^ writePtr(log2Ceil(dep)))

  io.readData := fifo.read(readAddr)
  when(io.readEnable && !io.readEmpty) {
    readPtr := readPtr + 1.U
  }
  when(io.writeEnable && !io.writeFull) {
    fifo.write(writeAddr, io.writeData)
    writePtr := writePtr + 1.U
  }
}

object elaborateSyncFifo extends App {
  chisel3.Driver.execute(args, () => new SyncFifo(dep = 32, dataType = UInt(512.W)))
}
