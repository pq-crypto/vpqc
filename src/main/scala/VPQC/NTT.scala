
package VPQC

import chisel3._
import chisel3.util._
import scala.math._
import utility._
import fsm._

/*
class MRFixIO extends Bundle
  with HasNTTCommonParameters {
  val a = Input(UInt((DataWidth * 2).W))
  val ar = Output(UInt(DataWidth.W))
}

class MRFix extends Module
  with HasNTTCommonParameters
  with HasMRParameters {
  val io = IO(new MRFixIO())

  // 16bit
  val tmp1 = io.a >> 12.U
  // 20bit
  val tmp2 = (tmp1 << 1.U) +& (tmp1 << 3.U)
  // 32bit
  val mul  = Wire(UInt(32.W))
  mul := (tmp2 << 12.U) + (tmp2 << 8.U) + (tmp2 << 4.U) + (tmp1 << 3.U) - tmp1
  // 15bit
  val qGuess = mul >> 17.U
  // 28bit
  val qM = Wire(UInt((DataWidth * 2).W))
  qM := qGuess + (qGuess << 12.U) + (qGuess << 13.U)

  val z = io.a - qM
  io.ar := Mux(z < MRq.asUInt(), z, z-MRq.asUInt())
}

object MRFix {
  def apply(a: UInt, ar: UInt): Module = {
    val inst = Module(new MRFix())
    inst.io.a := a
    ar := inst.io.ar
    inst
  }
  def apply(a: UInt): UInt = {
    val inst = Module(new MRFix())
    inst.io.a := a
    inst.io.ar
  }
}
*/

// add configurability

class MRIO extends Bundle
  with HasCommonParameters {
  val a = Input(UInt((DataWidth * 2).W))
  val n = Input(UInt(5.W))
  val m = Input(UInt(DataWidth.W))
  val u = Input(UInt((DataWidth + 2).W))
  val ar = Output(UInt(DataWidth.W))
}

class MR extends Module
  with HasCommonParameters {
  val io = IO(new MRIO)

  val shift1 = io.a >> (io.n - 2.U)
  val mul1 = io.u * shift1.asUInt
  val qGuess = mul1 >> (io.n + 3.U)
  // TODO: check if pipeline is needed
  val mul2 = qGuess * io.m
  val z = io.a - mul2

  io.ar := Mux(z < io.m, z, z-io.m)
}

object MR {
  def apply(a: UInt, n: UInt, m: UInt, u: UInt): UInt = {
    val inst = Module(new MR())
    inst.io.a := a
    inst.io.n := n
    inst.io.m := m
    inst.io.u := u
    inst.io.ar
  }
}

object elaborateMR extends App {
  chisel3.Driver.execute(args, () => new MR)
}

// no resource sharing version
class ButterflyIO extends Bundle
  with HasCommonParameters {
  // input two data
  val a = Input(UInt(DataWidth.W))
  val b = Input(UInt(DataWidth.W))
  // twiddle factor
  val wn = Input(UInt(DataWidth.W))
  // control whether DIT or DIF
  val mode = Input(Bool())
  // config for ModMul
  val m = Input(UInt(DataWidth.W))
  val u = Input(UInt((DataWidth + 2).W))
  val n = Input(UInt(5.W))
  // output two data
  val aout = Output(UInt(DataWidth.W))
  val bout = Output(UInt(DataWidth.W))
}

// DIT && DIF butterfly
// 0 : DIT
// 1 : DIF
class Butterfly extends Module
  with HasCommonParameters {
  val io = IO(new ButterflyIO)

  val a = io.a
  val b = io.b
  val wn = io.wn

  // pre-process
  val aout1 = ModAdd(a, b, io.m)
  val bout1 = ModSub(a, b, io.m)
  val amux = Mux(io.mode, aout1, a)
  val bmux = Mux(io.mode, bout1, b)

  // post-process
  val amul = amux
  val bmul = ModMul(a = bmux, b = wn, m = io.m, u = io.u, n = io.n)

  val aout2 = ModAdd(amul, bmul, io.m)
  val bout2 = ModSub(amul, bmul, io.m)

  io.aout := Mux(io.mode, amul, aout2)
  io.bout := Mux(io.mode, bmul, bout2)
}

object elaborateButterfly extends App {
  chisel3.Driver.execute(args, () => new Butterfly)
}

// resource sharing version
class ButterflyShareIO extends Bundle
  with HasCommonParameters {
  // input two data
  val a = Input(UInt(DataWidth.W))
  val b = Input(UInt(DataWidth.W))
  // twiddle factor
  val wn = Input(UInt(DataWidth.W))
  // control whether DIT or DIF
  val mode = Input(Bool())
  val vecValid = Input(Vec(3, Bool()))
  // config for ModMul
  val m = Input(UInt(DataWidth.W))
  val u = Input(UInt((DataWidth + 2).W))
  val n = Input(UInt(5.W))
  // output two data
  val aout = Output(UInt(DataWidth.W))
  val bout = Output(UInt(DataWidth.W))
}

// DIT && DIF butterfly
// 0 : DIT
// 1 : DIF
class ButterflyShare extends Module
  with HasCommonParameters {
  val io = IO(new ButterflyShareIO)

  val a = io.a
  val b = io.b
  val wn = io.wn

  // pre-process
  val aout1 = ModAdd(a, b, io.m)
  val bout1 = ModSub(a, b, io.m)
  val amux = Mux(io.mode, aout1, a)
  val bmux = Mux(io.mode, bout1, b)

  // post-process
  val mul1 = Mux(io.vecValid(2), a, bmux)
  val mul2 = Mux(io.vecValid(2), b, wn)
  val amul = amux
  val bmul = ModMul(a = mul1, b = mul2, m = io.m, u = io.u, n = io.n)

  val aout2 = ModAdd(amul, bmul, io.m)
  val bout2 = ModSub(amul, bmul, io.m)

  io.aout := Mux(io.vecValid(0), aout1,
              Mux(io.vecValid(1), bout1,
               Mux(io.vecValid(2), bmul,
                 Mux(io.mode, amul, aout2))))
  io.bout := Mux(io.mode, bmul, bout2)
}

// permutation network
class PermNetIO extends Bundle
  with HasCommonParameters
  with HasNTTParameters {
  val in = Input(Vec(ButterflyNum * 2, UInt(DataWidth.W)))
  val config = Input(Vec(log2Ceil(ButterflyNum), Bool()))
  val out = Output(Vec(ButterflyNum * 2, UInt(DataWidth.W)))
}
class PermNetIn extends Module
  with HasCommonParameters
  with HasNTTParameters {
  val io = IO(new PermNetIO)

  def perm(split: Int): Vec[UInt] = {
    val perm = Wire(Vec(ButterflyNum * 2, UInt(DataWidth.W)))
    for(i <- 0 until ButterflyNum * 2 / split) {
      for (j <- 0 until split) {
        if (j % 2 == 0) {
          perm(i * split + j) := io.in(i * split + j / 2)
        } else {
          perm(i * split + j) := io.in(i * split + split / 2 + (j - 1) / 2)
        }
      }
    }
    perm
  }

  val multiPerm = Wire(Vec(log2Ceil(ButterflyNum), Vec(ButterflyNum * 2, UInt(DataWidth.W))))
  for (i <- 0 until log2Ceil(ButterflyNum)) {
    multiPerm(i) := perm(pow(2, i + 2).toInt)
  }

  val out = Mux1H(io.config, multiPerm)
  io.out := Mux(!(io.config.reduce(_ || _)), io.in, out)
}
object elaboratePermNet extends App {
  chisel3.Driver.execute(args, () => new PermNetOut)
}

class PermNetOut extends Module
  with HasCommonParameters
  with HasNTTParameters {
  val io = IO(new PermNetIO)

  def perm(split: Int): Vec[UInt] = {
    val perm = Wire(Vec(ButterflyNum * 2, UInt(DataWidth.W)))
    for(i <- 0 until ButterflyNum * 2 / split) {
      for (j <- 0 until split) {
        if (j < split / 2) {
          perm(i * split + j) := io.in(i * split + j * 2)
        } else {
          perm(i * split + j) := io.in(i * split + 1 + (j - split / 2) * 2)
        }
      }
    }
    perm
  }

  val multiPerm = Wire(Vec(log2Ceil(ButterflyNum), Vec(ButterflyNum * 2, UInt(DataWidth.W))))
  for (i <- 0 until log2Ceil(ButterflyNum)) {
    multiPerm(i) := perm(pow(2, i + 2).toInt)
  }

  val out = Mux1H(io.config, multiPerm)
  io.out := Mux(!(io.config.reduce(_ || _)), io.in, out)
}

class NTTIO extends Bundle
  with HasCommonParameters
  with HasNTTParameters {
  val valid = Input(Bool())
  val mode = Input(Bool())

  // ram write interface
  val wa = Input(UInt(log2Ceil(Dimension / ButterflyNum).W))
  val di = Input(UInt((DataWidth * ButterflyNum).W))
  val we = Input(Bool())

  // from register
  val vectorReg1 = Input(Vec(ML, UInt(DataWidth.W)))
  val vectorReg2 = Input(Vec(ML, UInt(DataWidth.W)))

  // csr interface
  val csrs = Input(new CSRIO)

  // output
  val dataOut = Output(Vec(ButterflyNum * 2, UInt(DataWidth.W)))
  val done = Output(Bool())
  val busy = Output(Bool())
  val wb = Output(Bool())
//  val wpos = Output(Bool())
}

// This NTT Module does not apply resource sharing with vector arithmetic units
class NTT extends Module
  with HasCommonParameters
  with HasNTTParameters {
  val io = IO(new NTTIO)


  io.wb := true.B
  io.done := RegNext(io.valid)
  io.busy := false.B

  // permutation in
  val permNet = Module(new PermNetIn)
  for (i <- 0 until ButterflyNum*2) {
    if (i < ButterflyNum) {
      permNet.io.in(i) := io.vectorReg1(i)
    }
    else {
      permNet.io.in(i) := io.vectorReg2(i - ButterflyNum)
    }
  }

  for (i <- 0 until log2Ceil(ButterflyNum)) {
    permNet.io.config(i) := false.B
  }

  // to support NTT and INTT : stage i -> stage n-1-i

  if (ButterflyNum == 4) {
    when(io.csrs.csrButterflyCtrl.stageCfg === 0.U) {

    } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 1.U) {
      permNet.io.config(0) := true.B
    } .otherwise {
      permNet.io.config(1) := true.B
    }
  } else if (ButterflyNum == 8) {
      when(io.csrs.csrButterflyCtrl.stageCfg === 0.U) {

      } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 1.U) {
        permNet.io.config(0) := true.B
      } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 2.U) {
        permNet.io.config(1) := true.B
      } .otherwise {
        permNet.io.config(2) := true.B
      }
  } else if (ButterflyNum == 16) {
      when(io.csrs.csrButterflyCtrl.stageCfg === 0.U) {

      } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 1.U) {
        permNet.io.config(0) := true.B
      } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 2.U) {
        permNet.io.config(1) := true.B
      } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 3.U) {
        permNet.io.config(2) := true.B
      } .otherwise { // split = 32
        permNet.io.config(3) := true.B
      }
  } else if (ButterflyNum == 32) {
      when(io.csrs.csrButterflyCtrl.stageCfg === 0.U) {

      } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 1.U) {
        permNet.io.config(0) := true.B
      } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 2.U) {
        permNet.io.config(1) := true.B
      } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 3.U) {
        permNet.io.config(2) := true.B
      } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 4.U) {  // split = 32
        permNet.io.config(3) := true.B
      }.otherwise {
        permNet.io.config(4) := true.B
      }
  } else if (ButterflyNum == 64) {
      when(io.csrs.csrButterflyCtrl.stageCfg === 0.U) {

      } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 1.U) {
        permNet.io.config(0) := true.B
      } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 2.U) {
        permNet.io.config(1) := true.B
      } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 3.U) {
        permNet.io.config(2) := true.B
      } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 4.U) {
        permNet.io.config(3) := true.B
      }.elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 5.U) {
        permNet.io.config(4) := true.B
      }.otherwise {
        permNet.io.config(5) := true.B
      }
  }

  // wn addr prepare
  val wnBaseAddr1 = Wire(UInt(log2Ceil(Dimension / ButterflyNum).W))
  val wnBaseAddr2 = Wire(UInt(log2Ceil(Dimension / ButterflyNum).W))
  // contain stage info
  wnBaseAddr1 := Mux(io.csrs.csrButterflyCtrl.stageCfg < log2Ceil(ButterflyNum).asUInt(),
    0.U, 1.U << (io.csrs.csrButterflyCtrl.stageCfg - log2Ceil(ButterflyNum).asUInt()))
  // contain iter info
  wnBaseAddr2 := Mux(io.csrs.csrButterflyCtrl.stageCfg > log2Ceil(ButterflyNum).asUInt(),
    io.csrs.csrButterflyCtrl.iterCfg + wnBaseAddr1, wnBaseAddr1)


  // wn consts ram
  /**
    *
    *    (1)  {1 1} {1 1/4} {1 1/8 ..} {1 1/16 ... 7/16}
    *    (2)  1 1/32 ... 15/32
    *    (3)  ...
    *
    *    32 line 1 1/1024 ... 511/1024
    *
    */
  val twiddleRam = Module(new SyncRam(dep = Dimension / ButterflyNum, dw = DataWidth * ButterflyNum))
  twiddleRam.io.re := io.valid
  twiddleRam.io.we := io.we
  twiddleRam.io.ra := wnBaseAddr2
  twiddleRam.io.wa := io.wa
  twiddleRam.io.di := io.di


  // butterfly PEs
  val PEs = VecInit(Seq.fill(ButterflyNum)(Module(new Butterfly()).io))
  for (i <- 0 until ButterflyNum) {
    PEs(i).a := permNet.io.out(2 * i)
    PEs(i).b := permNet.io.out(2 * i + 1)

    PEs(i).wn := twiddleRam.io.dout(DataWidth * i + DataWidth - 1, DataWidth * i)
    PEs(i).mode := io.mode

    // wn assign
    if (ButterflyNum == 4) {
      when(io.csrs.csrButterflyCtrl.stageCfg < log2Ceil(ButterflyNum).asUInt()) {
        switch(io.csrs.csrButterflyCtrl.stageCfg) {
          is(0.U) {
            PEs(i).wn := twiddleRam.io.dout(DataWidth - 1, 0)
          }
          is(1.U) {
            PEs(i).wn := twiddleRam.io.dout(DataWidth * (i % 2 + 2) + DataWidth - 1, DataWidth * (i % 2 + 2))
          }
        }
      }
    } else if (ButterflyNum == 8) {
      when(io.csrs.csrButterflyCtrl.stageCfg < log2Ceil(ButterflyNum).asUInt()) {
        switch(io.csrs.csrButterflyCtrl.stageCfg) {
          is(0.U) {
            PEs(i).wn := twiddleRam.io.dout(DataWidth - 1, 0)
          }
          is(1.U) {
            PEs(i).wn := twiddleRam.io.dout(DataWidth * (i % 2 + 2) + DataWidth - 1, DataWidth * (i % 2 + 2))
          }
          is(2.U) {
            PEs(i).wn := twiddleRam.io.dout(DataWidth * (i % 4 + 4) + DataWidth - 1, DataWidth * (i % 4 + 4))
          }
        }
      }
    } else if (ButterflyNum == 16) {
      when(io.csrs.csrButterflyCtrl.stageCfg < log2Ceil(ButterflyNum).asUInt()) {
        switch(io.csrs.csrButterflyCtrl.stageCfg) {
          is(0.U) {
            PEs(i).wn := twiddleRam.io.dout(DataWidth - 1, 0)
          }
          is(1.U) {
            PEs(i).wn := twiddleRam.io.dout(DataWidth * (i % 2 + 2) + DataWidth - 1, DataWidth * (i % 2 + 2))
          }
          is(2.U) {
            PEs(i).wn := twiddleRam.io.dout(DataWidth * (i % 4 + 4) + DataWidth - 1, DataWidth * (i % 4 + 4))
          }
          is(3.U) {
            PEs(i).wn := twiddleRam.io.dout(DataWidth * (i % 8 + 8) + DataWidth - 1, DataWidth * (i % 8 + 8))
          }
        }
      }
    } else if (ButterflyNum == 32) {
      when(io.csrs.csrButterflyCtrl.stageCfg < log2Ceil(ButterflyNum).asUInt()) {
        switch(io.csrs.csrButterflyCtrl.stageCfg) {
          is(0.U) {
            PEs(i).wn := twiddleRam.io.dout(DataWidth - 1, 0)
          }
          is(1.U) {
            PEs(i).wn := twiddleRam.io.dout(DataWidth * (i % 2 + 2) + DataWidth - 1, DataWidth * (i % 2 + 2))
          }
          is(2.U) {
            PEs(i).wn := twiddleRam.io.dout(DataWidth * (i % 4 + 4) + DataWidth - 1, DataWidth * (i % 4 + 4))
          }
          is(3.U) {
            PEs(i).wn := twiddleRam.io.dout(DataWidth * (i % 8 + 8) + DataWidth - 1, DataWidth * (i % 8 + 8))
          }
          is(4.U) {
            PEs(i).wn := twiddleRam.io.dout(DataWidth * (i % 16 + 16) + DataWidth - 1, DataWidth * (i % 16 + 16))
          }
        }
      }
    } else if (ButterflyNum == 64) {
      when(io.csrs.csrButterflyCtrl.stageCfg < log2Ceil(ButterflyNum).asUInt()) {
        switch(io.csrs.csrButterflyCtrl.stageCfg) {
          is(0.U) {
            PEs(i).wn := twiddleRam.io.dout(DataWidth - 1, 0)
          }
          is(1.U) {
            PEs(i).wn := twiddleRam.io.dout(DataWidth * (i % 2 + 2) + DataWidth - 1, DataWidth * (i % 2 + 2))
          }
          is(2.U) {
            PEs(i).wn := twiddleRam.io.dout(DataWidth * (i % 4 + 4) + DataWidth - 1, DataWidth * (i % 4 + 4))
          }
          is(3.U) {
            PEs(i).wn := twiddleRam.io.dout(DataWidth * (i % 8 + 8) + DataWidth - 1, DataWidth * (i % 8 + 8))
          }
          is(4.U) {
            PEs(i).wn := twiddleRam.io.dout(DataWidth * (i % 16 + 16) + DataWidth - 1, DataWidth * (i % 16 + 16))
          }
          is(5.U) {
            PEs(i).wn := twiddleRam.io.dout(DataWidth * (i % 32 + 32) + DataWidth - 1, DataWidth * (i % 32 + 32))
          }
        }
      }
    }

    PEs(i).n := io.csrs.csrModulusLen
    PEs(i).m := io.csrs.csrModulusq
    PEs(i).u := io.csrs.csrBarretu

  }

  // permutation out
  val permNetOut = Module(new PermNetOut)
  permNetOut.io.config := permNet.io.config
  for (i <- 0 until ButterflyNum) {
    permNetOut.io.in(2 * i) := PEs(i).aout
    permNetOut.io.in(2 * i + 1) := PEs(i).bout
  }
  io.dataOut := permNetOut.io.out
}

class NTTWithoutRam extends Module
  with HasCommonParameters
  with HasNTTParameters {
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val mode = Input(Bool())

    // ram from outside
    val ra = Output(UInt(log2Ceil(Dimension / ButterflyNum).W))
    val re = Output(Bool())
    val di = Input(UInt((ButterflyNum * DataWidth).W))

    // from register
    val vectorReg1 = Input(Vec(ML, UInt(DataWidth.W)))
    val vectorReg2 = Input(Vec(ML, UInt(DataWidth.W)))

    // csr interface
    val csrs = Input(new CSRIO)

    // output
    val dataOut = Output(Vec(ButterflyNum * 2, UInt(DataWidth.W)))
    val done = Output(Bool())
    val busy = Output(Bool())
    val wb = Output(Bool())
  })


  io.wb := true.B
  io.done := RegNext(io.valid)
  io.busy := false.B

  // permutation in
  val permNet = Module(new PermNetIn)
  for (i <- 0 until ButterflyNum*2) {
    if (i < ButterflyNum) {
      permNet.io.in(i) := io.vectorReg1(i)
    }
    else {
      permNet.io.in(i) := io.vectorReg2(i - ButterflyNum)
    }
  }

  // to support NTT and INTT : stage i -> stage n-1-i
  for (i <- 0 until log2Ceil(ButterflyNum)) {
    permNet.io.config(i) := false.B
  }
  if (ButterflyNum == 4) {
    when(io.csrs.csrButterflyCtrl.stageCfg === 0.U) {

    } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 1.U) {
      permNet.io.config(0) := true.B
    } .otherwise {
      permNet.io.config(1) := true.B
    }
  } else if (ButterflyNum == 8) {
    when(io.csrs.csrButterflyCtrl.stageCfg === 0.U) {

    } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 1.U) {
      permNet.io.config(0) := true.B
    } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 2.U) {
      permNet.io.config(1) := true.B
    } .otherwise {
      permNet.io.config(2) := true.B
    }
  } else if (ButterflyNum == 16) {
    when(io.csrs.csrButterflyCtrl.stageCfg === 0.U) {

    } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 1.U) {
      permNet.io.config(0) := true.B
    } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 2.U) {
      permNet.io.config(1) := true.B
    } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 3.U) {
      permNet.io.config(2) := true.B
    } .otherwise { // split = 32
      permNet.io.config(3) := true.B
    }
  } else if (ButterflyNum == 32) {
    when(io.csrs.csrButterflyCtrl.stageCfg === 0.U) {

    } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 1.U) {
      permNet.io.config(0) := true.B
    } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 2.U) {
      permNet.io.config(1) := true.B
    } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 3.U) {
      permNet.io.config(2) := true.B
    } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 4.U) {  // split = 32
      permNet.io.config(3) := true.B
    }.otherwise {
      permNet.io.config(4) := true.B
    }
  } else if (ButterflyNum == 64) {
    when(io.csrs.csrButterflyCtrl.stageCfg === 0.U) {

    } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 1.U) {
      permNet.io.config(0) := true.B
    } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 2.U) {
      permNet.io.config(1) := true.B
    } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 3.U) {
      permNet.io.config(2) := true.B
    } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 4.U) {
      permNet.io.config(3) := true.B
    }.elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 5.U) {
      permNet.io.config(4) := true.B
    }.otherwise {
      permNet.io.config(5) := true.B
    }
  }

  // wn addr prepare
  val wnBaseAddr1 = Wire(UInt(log2Ceil(Dimension / ButterflyNum).W))
  val wnBaseAddr2 = Wire(UInt(log2Ceil(Dimension / ButterflyNum).W))
  // contain stage info
  wnBaseAddr1 := Mux(io.csrs.csrButterflyCtrl.stageCfg < log2Ceil(ButterflyNum).asUInt(),
    0.U, 1.U << (io.csrs.csrButterflyCtrl.stageCfg - log2Ceil(ButterflyNum).asUInt()))
  // contain iter info
  wnBaseAddr2 := Mux(io.csrs.csrButterflyCtrl.stageCfg > log2Ceil(ButterflyNum).asUInt(),
    io.csrs.csrButterflyCtrl.iterCfg + wnBaseAddr1, wnBaseAddr1)

  io.re := io.valid
  io.ra := wnBaseAddr2

  // butterfly PEs
  val PEs = VecInit(Seq.fill(ButterflyNum)(Module(new Butterfly()).io))
  for (i <- 0 until ButterflyNum) {
    PEs(i).a := permNet.io.out(2 * i)
    PEs(i).b := permNet.io.out(2 * i + 1)
    // wn assign
    PEs(i).wn := io.di(DataWidth * i + DataWidth - 1, DataWidth * i)
    PEs(i).mode := io.mode
    if (ButterflyNum == 4) {
      when(io.csrs.csrButterflyCtrl.stageCfg < log2Ceil(ButterflyNum).asUInt()) {
        switch(io.csrs.csrButterflyCtrl.stageCfg) {
          is(0.U) {
            PEs(i).wn := io.di(DataWidth - 1, 0)
          }
          is(1.U) {
            PEs(i).wn := io.di(DataWidth * (i % 2 + 2) + DataWidth - 1, DataWidth * (i % 2 + 2))
          }
        }
      }
    } else if (ButterflyNum == 8) {
      when(io.csrs.csrButterflyCtrl.stageCfg < log2Ceil(ButterflyNum).asUInt()) {
        switch(io.csrs.csrButterflyCtrl.stageCfg) {
          is(0.U) {
            PEs(i).wn := io.di(DataWidth - 1, 0)
          }
          is(1.U) {
            PEs(i).wn := io.di(DataWidth * (i % 2 + 2) + DataWidth - 1, DataWidth * (i % 2 + 2))
          }
          is(2.U) {
            PEs(i).wn := io.di(DataWidth * (i % 4 + 4) + DataWidth - 1, DataWidth * (i % 4 + 4))
          }
        }
      }
    } else if (ButterflyNum == 16) {
      when(io.csrs.csrButterflyCtrl.stageCfg < log2Ceil(ButterflyNum).asUInt()) {
        switch(io.csrs.csrButterflyCtrl.stageCfg) {
          is(0.U) {
            PEs(i).wn := io.di(DataWidth - 1, 0)
          }
          is(1.U) {
            PEs(i).wn := io.di(DataWidth * (i % 2 + 2) + DataWidth - 1, DataWidth * (i % 2 + 2))
          }
          is(2.U) {
            PEs(i).wn := io.di(DataWidth * (i % 4 + 4) + DataWidth - 1, DataWidth * (i % 4 + 4))
          }
          is(3.U) {
            PEs(i).wn := io.di(DataWidth * (i % 8 + 8) + DataWidth - 1, DataWidth * (i % 8 + 8))
          }
        }
      }
    } else if (ButterflyNum == 32) {
      when(io.csrs.csrButterflyCtrl.stageCfg < log2Ceil(ButterflyNum).asUInt()) {
        switch(io.csrs.csrButterflyCtrl.stageCfg) {
          is(0.U) {
            PEs(i).wn := io.di(DataWidth - 1, 0)
          }
          is(1.U) {
            PEs(i).wn := io.di(DataWidth * (i % 2 + 2) + DataWidth - 1, DataWidth * (i % 2 + 2))
          }
          is(2.U) {
            PEs(i).wn := io.di(DataWidth * (i % 4 + 4) + DataWidth - 1, DataWidth * (i % 4 + 4))
          }
          is(3.U) {
            PEs(i).wn := io.di(DataWidth * (i % 8 + 8) + DataWidth - 1, DataWidth * (i % 8 + 8))
          }
          is(4.U) {
            PEs(i).wn := io.di(DataWidth * (i % 16 + 16) + DataWidth - 1, DataWidth * (i % 16 + 16))
          }
        }
      }
    } else if (ButterflyNum == 64) {
      when(io.csrs.csrButterflyCtrl.stageCfg < log2Ceil(ButterflyNum).asUInt()) {
        switch(io.csrs.csrButterflyCtrl.stageCfg) {
          is(0.U) {
            PEs(i).wn := io.di(DataWidth - 1, 0)
          }
          is(1.U) {
            PEs(i).wn := io.di(DataWidth * (i % 2 + 2) + DataWidth - 1, DataWidth * (i % 2 + 2))
          }
          is(2.U) {
            PEs(i).wn := io.di(DataWidth * (i % 4 + 4) + DataWidth - 1, DataWidth * (i % 4 + 4))
          }
          is(3.U) {
            PEs(i).wn := io.di(DataWidth * (i % 8 + 8) + DataWidth - 1, DataWidth * (i % 8 + 8))
          }
          is(4.U) {
            PEs(i).wn := io.di(DataWidth * (i % 16 + 16) + DataWidth - 1, DataWidth * (i % 16 + 16))
          }
          is(5.U) {
            PEs(i).wn := io.di(DataWidth * (i % 32 + 32) + DataWidth - 1, DataWidth * (i % 32 + 32))
          }
        }
      }
    }

    PEs(i).n := io.csrs.csrModulusLen
    PEs(i).m := io.csrs.csrModulusq
    PEs(i).u := io.csrs.csrBarretu

  }

  // permutation out
  val permNetOut = Module(new PermNetOut)
  permNetOut.io.config := permNet.io.config
  for (i <- 0 until ButterflyNum) {
    permNetOut.io.in(2 * i) := PEs(i).aout
    permNetOut.io.in(2 * i + 1) := PEs(i).bout
  }
  io.dataOut := permNetOut.io.out
}

class NTTWithoutRamShare extends Module
  with HasCommonParameters
  with HasNTTParameters {
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val mode = Input(Bool())
    val vecValid = Input(Vec(3, Bool()))

    // ram from outside
    val ra = Output(UInt(log2Ceil(Dimension / ButterflyNum).W))
    val re = Output(Bool())
    val di = Input(UInt((ButterflyNum * DataWidth).W))

    // from register
    val vectorReg1 = Input(Vec(ML, UInt(DataWidth.W)))
    val vectorReg2 = Input(Vec(ML, UInt(DataWidth.W)))

    // csr interface
    val csrs = Input(new CSRIO)

    // output
    val dataOut = Output(Vec(ButterflyNum * 2, UInt(DataWidth.W)))
    val done = Output(Bool())
    val busy = Output(Bool())
    val wb = Output(Bool())
  })


  io.wb := true.B
  io.done := RegNext(io.valid)
  io.busy := false.B

  // permutation in
  val permNet = Module(new PermNetIn)
  for (i <- 0 until ButterflyNum*2) {
    if (i < ButterflyNum) {
      permNet.io.in(i) := io.vectorReg1(i)
    }
    else {
      permNet.io.in(i) := io.vectorReg2(i - ButterflyNum)
    }
  }

  // to support NTT and INTT : stage i -> stage n-1-i
  for (i <- 0 until log2Ceil(ButterflyNum)) {
    permNet.io.config(i) := false.B
  }
  if (ButterflyNum == 4) {
    when(io.csrs.csrButterflyCtrl.stageCfg === 0.U) {

    } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 1.U) {
      permNet.io.config(0) := true.B
    } .otherwise {
      permNet.io.config(1) := true.B
    }
  } else if (ButterflyNum == 8) {
    when(io.csrs.csrButterflyCtrl.stageCfg === 0.U) {

    } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 1.U) {
      permNet.io.config(0) := true.B
    } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 2.U) {
      permNet.io.config(1) := true.B
    } .otherwise {
      permNet.io.config(2) := true.B
    }
  } else if (ButterflyNum == 16) {
    when(io.csrs.csrButterflyCtrl.stageCfg === 0.U) {

    } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 1.U) {
      permNet.io.config(0) := true.B
    } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 2.U) {
      permNet.io.config(1) := true.B
    } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 3.U) {
      permNet.io.config(2) := true.B
    } .otherwise { // split = 32
      permNet.io.config(3) := true.B
    }
  } else if (ButterflyNum == 32) {
    when(io.csrs.csrButterflyCtrl.stageCfg === 0.U) {

    } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 1.U) {
      permNet.io.config(0) := true.B
    } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 2.U) {
      permNet.io.config(1) := true.B
    } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 3.U) {
      permNet.io.config(2) := true.B
    } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 4.U) {  // split = 32
      permNet.io.config(3) := true.B
    }.otherwise {
      permNet.io.config(4) := true.B
    }
  } else if (ButterflyNum == 64) {
    when(io.csrs.csrButterflyCtrl.stageCfg === 0.U) {

    } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 1.U) {
      permNet.io.config(0) := true.B
    } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 2.U) {
      permNet.io.config(1) := true.B
    } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 3.U) {
      permNet.io.config(2) := true.B
    } .elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 4.U) {
      permNet.io.config(3) := true.B
    }.elsewhen(io.csrs.csrButterflyCtrl.stageCfg === 5.U) {
      permNet.io.config(4) := true.B
    }.otherwise {
      permNet.io.config(5) := true.B
    }
  }

  // wn addr prepare
  val wnBaseAddr1 = Wire(UInt(log2Ceil(Dimension / ButterflyNum).W))
  val wnBaseAddr2 = Wire(UInt(log2Ceil(Dimension / ButterflyNum).W))
  // contain stage info
  wnBaseAddr1 := Mux(io.csrs.csrButterflyCtrl.stageCfg < log2Ceil(ButterflyNum).asUInt(),
    0.U, 1.U << (io.csrs.csrButterflyCtrl.stageCfg - log2Ceil(ButterflyNum).asUInt()))
  // contain iter info
  wnBaseAddr2 := Mux(io.csrs.csrButterflyCtrl.stageCfg > log2Ceil(ButterflyNum).asUInt(),
    io.csrs.csrButterflyCtrl.iterCfg + wnBaseAddr1, wnBaseAddr1)

  io.re := io.valid
  io.ra := wnBaseAddr2

  // butterfly PEs
  val PEs = VecInit(Seq.fill(ButterflyNum)(Module(new ButterflyShare()).io))
  for (i <- 0 until ButterflyNum) {
    PEs(i).a := Mux(io.valid, permNet.io.out(2 * i), io.vectorReg1(i))
    PEs(i).b := Mux(io.valid, permNet.io.out(2 * i + 1), io.vectorReg2(i))
    // wn assign
    PEs(i).wn := io.di(DataWidth * i + DataWidth - 1, DataWidth * i)
    PEs(i).mode := io.mode
    PEs(i).vecValid := io.vecValid
    if (ButterflyNum == 4) {
      when(io.csrs.csrButterflyCtrl.stageCfg < log2Ceil(ButterflyNum).asUInt()) {
        switch(io.csrs.csrButterflyCtrl.stageCfg) {
          is(0.U) {
            PEs(i).wn := io.di(DataWidth - 1, 0)
          }
          is(1.U) {
            PEs(i).wn := io.di(DataWidth * (i % 2 + 2) + DataWidth - 1, DataWidth * (i % 2 + 2))
          }
        }
      }
    } else if (ButterflyNum == 8) {
      when(io.csrs.csrButterflyCtrl.stageCfg < log2Ceil(ButterflyNum).asUInt()) {
        switch(io.csrs.csrButterflyCtrl.stageCfg) {
          is(0.U) {
            PEs(i).wn := io.di(DataWidth - 1, 0)
          }
          is(1.U) {
            PEs(i).wn := io.di(DataWidth * (i % 2 + 2) + DataWidth - 1, DataWidth * (i % 2 + 2))
          }
          is(2.U) {
            PEs(i).wn := io.di(DataWidth * (i % 4 + 4) + DataWidth - 1, DataWidth * (i % 4 + 4))
          }
        }
      }
    } else if (ButterflyNum == 16) {
      when(io.csrs.csrButterflyCtrl.stageCfg < log2Ceil(ButterflyNum).asUInt()) {
        switch(io.csrs.csrButterflyCtrl.stageCfg) {
          is(0.U) {
            PEs(i).wn := io.di(DataWidth - 1, 0)
          }
          is(1.U) {
            PEs(i).wn := io.di(DataWidth * (i % 2 + 2) + DataWidth - 1, DataWidth * (i % 2 + 2))
          }
          is(2.U) {
            PEs(i).wn := io.di(DataWidth * (i % 4 + 4) + DataWidth - 1, DataWidth * (i % 4 + 4))
          }
          is(3.U) {
            PEs(i).wn := io.di(DataWidth * (i % 8 + 8) + DataWidth - 1, DataWidth * (i % 8 + 8))
          }
        }
      }
    } else if (ButterflyNum == 32) {
      when(io.csrs.csrButterflyCtrl.stageCfg < log2Ceil(ButterflyNum).asUInt()) {
        switch(io.csrs.csrButterflyCtrl.stageCfg) {
          is(0.U) {
            PEs(i).wn := io.di(DataWidth - 1, 0)
          }
          is(1.U) {
            PEs(i).wn := io.di(DataWidth * (i % 2 + 2) + DataWidth - 1, DataWidth * (i % 2 + 2))
          }
          is(2.U) {
            PEs(i).wn := io.di(DataWidth * (i % 4 + 4) + DataWidth - 1, DataWidth * (i % 4 + 4))
          }
          is(3.U) {
            PEs(i).wn := io.di(DataWidth * (i % 8 + 8) + DataWidth - 1, DataWidth * (i % 8 + 8))
          }
          is(4.U) {
            PEs(i).wn := io.di(DataWidth * (i % 16 + 16) + DataWidth - 1, DataWidth * (i % 16 + 16))
          }
        }
      }
    } else if (ButterflyNum == 64) {
      when(io.csrs.csrButterflyCtrl.stageCfg < log2Ceil(ButterflyNum).asUInt()) {
        switch(io.csrs.csrButterflyCtrl.stageCfg) {
          is(0.U) {
            PEs(i).wn := io.di(DataWidth - 1, 0)
          }
          is(1.U) {
            PEs(i).wn := io.di(DataWidth * (i % 2 + 2) + DataWidth - 1, DataWidth * (i % 2 + 2))
          }
          is(2.U) {
            PEs(i).wn := io.di(DataWidth * (i % 4 + 4) + DataWidth - 1, DataWidth * (i % 4 + 4))
          }
          is(3.U) {
            PEs(i).wn := io.di(DataWidth * (i % 8 + 8) + DataWidth - 1, DataWidth * (i % 8 + 8))
          }
          is(4.U) {
            PEs(i).wn := io.di(DataWidth * (i % 16 + 16) + DataWidth - 1, DataWidth * (i % 16 + 16))
          }
          is(5.U) {
            PEs(i).wn := io.di(DataWidth * (i % 32 + 32) + DataWidth - 1, DataWidth * (i % 32 + 32))
          }
        }
      }
    }

    PEs(i).n := io.csrs.csrModulusLen
    PEs(i).m := io.csrs.csrModulusq
    PEs(i).u := io.csrs.csrBarretu

  }

  // permutation out
  val permNetOut = Module(new PermNetOut)
  permNetOut.io.config := permNet.io.config
  val vecArithOut = Wire(Vec(2 * ButterflyNum, UInt(DataWidth.W)))
  for (i <- 0 until ButterflyNum) {
    vecArithOut(i) := PEs(i).aout
    vecArithOut(i + ButterflyNum) := 0.U
    permNetOut.io.in(2 * i) := PEs(i).aout
    permNetOut.io.in(2 * i + 1) := PEs(i).bout
  }
  io.dataOut := Mux(io.valid, permNetOut.io.out, vecArithOut)
}

object elaborateNTTWithoutRamShare extends App {
  chisel3.Driver.execute(args, () => new NTTWithoutRamShare)
}

object elaborateNTTWithoutRam extends App {
  chisel3.Driver.execute(args, () => new NTTWithoutRam)
}

object elaborateNTT extends App {
  chisel3.Driver.execute(args, () => new NTT)
}

//class SwitchIO extends Bundle
//  with HasCommonParameters {
//  val a = Input(UInt(DataWidth.W))
//  val b = Input(UInt(DataWidth.W))
//  val sel = Input(Bool())
//  val aout = Output(UInt(DataWidth.W))
//  val bout = Output(UInt(DataWidth.W))
//}
//
//class Switch extends Module {
//  val io = IO(new SwitchIO())
//  io.aout := Mux(io.sel, io.b, io.a)
//  io.bout := Mux(io.sel, io.a, io.b)
//}
//object Switch {
//  def apply(a: UInt, b: UInt, sel: Bool, aout: UInt, bout: UInt): Module = {
//    val inst = Module(new Switch)
//    inst.io.a := a
//    inst.io.b := b
//    inst.io.sel := sel
//    aout := inst.io.aout
//    bout := inst.io.bout
//    inst
//  }
//}

//class NTTR2MDCIO extends Bundle
//  with HasNTTCommonParameters {
//  val dIn = Input(UInt(DataWidth.W))
//  val dInValid = Input(Bool())
//  val dOut1 = Output(UInt(DataWidth.W))
//  // val addrOut1 = Output(UInt(AddrWidth.W))
//  val dOut2 = Output(UInt(DataWidth.W))
//  // val addrOut2 = Output(UInt(AddrWidth.W))
//  val dOutValid = Output(Bool())
//}
//
//class NTTR2MDC extends Module
//  with HasMRParameters
//  with HasNTTCommonParameters
//  with HasNTTParameters {
//  val io = IO(new NTTR2MDCIO())
//
//  val stages: Int = AddrWidth
//
//  val wn  = RegInit(VecInit(Seq.fill(stages)(1.U(DataWidth.W))))
//  val cnt = RegInit(0.U((stages).W))
//  when(io.dInValid){
//    cnt := cnt + 1.U
//    for (i <- 0 until stages - 1) {
//      val res = MRFix(wn(i) * WN(i).asUInt())
//      wn(i) := Mux(res === (MRq.asUInt() - 1.U), 1.U, res)
//    }
//  }
//
//  val out1 = VecInit(Seq.fill(stages)(0.U(DataWidth.W)))
//  val out2 = VecInit(Seq.fill(stages)(0.U(DataWidth.W)))
//  /***
//    *
//    * pre modular reduction
//    *
//    *
//    */
//
//  val dIn = Mux(io.dIn < MRq.asUInt(), io.dIn, io.dIn - MRq.asUInt())
//
//  for (i <- 0 until stages){
//    if (i == 0) {
//      val regIn = ShiftRegister(dIn, VectorLength / 2)
//      val BFOut1 = Wire(UInt(DataWidth.W))
//      val BFOut2 = Wire(UInt(DataWidth.W))
//      BF2(regIn, dIn, BFOut1, BFOut2)
//      /**
//        *  can add pipeline if necessary
//         *
//      **/
//      val mulRes = MRFix(BFOut2 * wn(i))
//      val switchIn1 = BFOut1
//      val switchIn2 = ShiftRegister(mulRes, VectorLength/4)
//      val swCtrl = cnt(stages-2)
//      Switch(switchIn1, switchIn2, swCtrl, out1(0), out2(0))
//    }
//    else if (i < stages - 1){
//      val regIn = ShiftRegister(out1(i-1), (VectorLength/pow(2, i + 1)).toInt)
//      val BFOut1 = Wire(UInt(14.W))
//      val BFOut2 = Wire(UInt(14.W))
//      BF2(regIn, out2(i-1), BFOut1, BFOut2)
//      /**
//        *  can add pipeline if necessary
//        *
//        **/
//      val mulRes = MRFix(BFOut2 * wn(i))
//      val switchIn1 = BFOut1
//      val switchIn2 = ShiftRegister(mulRes, (VectorLength/pow(2, i + 2)).toInt)
//      val swCtrl = cnt(stages-2-i)
//      Switch(switchIn1, switchIn2, swCtrl, out1(i), out2(i))
//    }
//    else {
//      val regIn = ShiftRegister(out1(i-1), (VectorLength/pow(2, i + 1)).toInt)
//      BF2(regIn, out2(i-1), out1(i), out2(i))
//    }
//  }
//  io.dOut1 := RegNext(out1(stages - 1))
//  io.dOut2 := RegNext(out2(stages - 1))
//  io.dOutValid := ShiftRegister(io.dInValid, VectorLength)
//
//}
