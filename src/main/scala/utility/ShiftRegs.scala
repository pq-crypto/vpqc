
package utility

import chisel3._

// paramized shift register
class ShiftRegs(val n: Int, val w: Int) extends Module {
  val io = IO(new Bundle {
    val in    = Input(UInt(w.W))
    val out   = Output(UInt(w.W))
  })

  val initValues = Seq.fill(n) { 0.U(w.W) }

  val delays = RegInit(VecInit(initValues))
  for (i <- 0 until n) {
    if (i == 0) {
      delays(0) := io.in
    }
    else {
      delays(i) := delays(i-1)
    }
  }
  io.out := delays(n-1)
}

// object vecshift using apply method
object ShiftRegs {
  def apply (n : Int, w : Int) (in: UInt) : UInt = {
    val inst = Module(new ShiftRegs (n, w))
    inst.io.in := in
    inst.io.out
  }
}
