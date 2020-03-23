
package VPQC

import chisel3._
import chisel3.util._
import utility.SyncFifo
import fsm._

/**
  *
  *  Description  :   implement Keccakc, that is Keccak-p[1600,24]
  *                   look FIPS PUB 202 for more information
  *
**/

/**
  *
  *   index = (y,x,z)
  *
**/
class stateArray extends Bundle {
  val s = Vec(5, Vec(5, (UInt(64.W))))
}


class ProcIO extends Bundle
  with HasKeccakParameters {
  val in = Input(new stateArray())
  val out = Output(new stateArray())
}

class Proc1 extends Module
  with HasKeccakParameters {
  val io = IO(new ProcIO)

  val state = io.in.s
  val columnsXorSqueez = WireInit(VecInit(Seq.fill(5)(VecInit(Seq.fill(64)(false.B)))))
  // step 1
  for (x <- 0 until 5) {
    for (z <- 0 until 64) {
      columnsXorSqueez(x)(z) := (0 until 5).map(y => state(y)(x)(z)).reduce(_ ^ _)
    }
  }
  val columnsXorInter = WireInit(VecInit(Seq.fill(5)(VecInit(Seq.fill(64)(false.B)))))
  // step 2
  for (x <- 0 until 5) {
    for (z <- 0 until 64) {
      columnsXorInter(x)(z) := columnsXorSqueez(mod(x - 1, 5))(z) ^ columnsXorSqueez(mod(x + 1, 5))(mod(z - 1, 64))
    }
  }
  // step 3
  for (y <- 0 until 5) {
    for (x <- 0 until 5) {
      io.out.s(y)(x) := state(y)(x) ^ columnsXorInter(x).asUInt()
    }
  }
}

class Proc2 extends Module
  with HasKeccakParameters {
  val io = IO(new ProcIO())

  val state = io.in.s
  for (y <- 0 until 5) {
    for (x <- 0 until 5) {
      var shamt = mod(proc2param(y)(x), 64)
      if (shamt == 0) {
        io.out.s(y)(x) := state(y)(x)
      } else {
        io.out.s(y)(x) := Cat(state(y)(x)(63 - shamt, 0), state(y)(x)(63, 64 - shamt))
      }
    }
  }
}

class Proc3 extends Module
  with HasKeccakParameters {
  val io = IO(new ProcIO())

  val state = io.in.s
  for (y <- 0 until 5) {
    for (x <- 0 until 5) {
      io.out.s(y)(x) := state(x)(mod(x + 3 * y, 5))
    }
  }
}

class Proc4 extends Module
  with HasKeccakParameters {
  val io = IO(new ProcIO())

  val state = io.in.s
  for (y <- 0 until 5) {
    for (x <- 0 until 5) {
      io.out.s(y)(x) := (~state(y)(mod(x + 1, 5)) & state(y)(mod(x + 2, 5))) ^ state(y)(x)
    }
  }
}

class Proc5(ir: Int) extends Module
  with HasKeccakParameters {
  val io = IO(new ProcIO())

  val state = io.in.s
  val RC = WireInit(VecInit(Seq.fill(64)(false.B)))
  for (j <- 0 to 6) {
    RC((1 << j) - 1) := rc(j + ir * 7).asUInt()
  }
  val nextState = Wire(new stateArray())
  nextState.s := state
  nextState.s(0)(0) := state(0)(0) ^ RC.asUInt()
  io.out.s := nextState.s
}

class KeccakCoreIO extends Bundle
  with HasKeccakParameters {
  val valid = Input(Bool())
  val seed = Input(new stateArray())
  val seedWrite = Input(Bool())
  val prngNumber = Output(new stateArray())
  val done = Output(Bool())
  val initialized = Output(Bool())
}

/**
  *  cnt   valid        state                            done
  *   0      0                                             0
  *   0      1
  *   1      done care
  *   .
  *   .
  *   .
  *   24                end point write to final value
  *   0                                                    1
  */
class KeccakCore extends Module
  with HasKeccakParameters {
  val io = IO(new KeccakCoreIO())

  // count control
  val cnt = RegInit(0.U(5.W))
  val busy = cnt =/= 0.U
  val countBegin = io.valid || busy
  val state = Wire(Vec(5, new stateArray()))
  val stateReg = Reg((new stateArray()))
  val completeFlag = cnt === RoundsNum.asUInt()
  val initialized = RegInit(false.B)

  when(countBegin) {
    cnt := Mux(completeFlag, 0.U, cnt + 1.U)
  }

  when(io.seedWrite) {
    stateReg := io.seed
    initialized := true.B
  } .elsewhen(cnt =/= 0.U) {
    stateReg := state(4)
  }

  // module instantion
  val proc1 = Module(new Proc1())
  proc1.io.in := Mux(cnt === 0.U, 0.U.asTypeOf(new stateArray()), stateReg)
  state(0) := proc1.io.out
  val proc2 = Module(new Proc2())
  proc2.io.in := state(0)
  state(1) := proc2.io.out
  val proc3 = Module(new Proc3())
  proc3.io.in := state(1)
  state(2) := proc3.io.out
  val proc4 = Module(new Proc4())
  proc4.io.in := state(2)
  state(3) := proc4.io.out
  val proc5s = (0 until RoundsNum).map(i => Module(new Proc5(i)).io)
  for (i <- 0 until RoundsNum) {
    proc5s(i).in := state(3)
  }
  state(4) := MuxLookup(cnt, proc5s(0).out, (2 to RoundsNum).map(i => (i.asUInt() -> proc5s(i-1).out)))

  io.prngNumber := stateReg
  io.done := RegNext(completeFlag)
  io.initialized := initialized
}

class KeccakWithFifo extends Module
  with HasCommonParameters
  with HasKeccakParameters {
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val seed = Input(new stateArray())
    val seedWrite = Input(Bool())
    val prn = Output(Vec(ML, UInt(DataWidth.W)))
    val done = Output(Bool())
    val busy = Output(Bool())
    val wb = Output(Bool())
  })

  val prng = Module(new KeccakCore)
  prng.io.seed := io.seed
  prng.io.seedWrite := io.seedWrite
  val fifo = Module(new SyncFifo(dep = 32, dataType = UInt((ML * DataWidth).W)))

  // fill the buffer if it is not full
  prng.io.valid := !fifo.io.writeFull && prng.io.initialized
  // write prn to buffer
  fifo.io.writeData := prng.io.prngNumber.s.asUInt()
  fifo.io.writeEnable := prng.io.done
  fifo.io.readEnable := io.valid

  // get random number
  for (i <- 0 until ML) {
    io.prn(i) := fifo.io.readData(DataWidth * i + DataWidth - 1, DataWidth * i)
  }
  io.wb := true.B
  io.done := false.B
  io.busy := false.B

  val fsm = InstanciateFSM(new FSM{
    entryState(stateName = "Idle")
      .act{
        io.done := false.B
        io.busy := false.B
      }
      .when(io.valid && fifo.io.readEmpty).transferTo(destName = "Wait")
      .when(io.valid && !fifo.io.readEmpty).transferTo(destName = "Read")

    state(stateName = "Wait")
      .act{
        io.done := false.B
        io.busy := true.B
      }
      .when(!fifo.io.readEmpty).transferTo(destName = "Read")

    state(stateName = "Read")
      .act{
        io.done := true.B
        io.busy := false.B
      }
      .when(io.valid && fifo.io.readEmpty).transferTo(destName = "Wait")
      .when(io.valid && !fifo.io.readEmpty).transferTo(destName = "Read")
      .otherwise.transferToEnd
  })

}

// for synthesis
class KeccakNoFifo extends Module
  with HasCommonParameters
  with HasKeccakParameters {
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val seed = Input(new stateArray())
    val seedWrite = Input(Bool())
    val prn = Output(Vec(ML, UInt(DataWidth.W)))
    val done = Output(Bool())
    val busy = Output(Bool())
    val wb = Output(Bool())
    // from/to syncfifo
    val writeData = Output(UInt((ML * DataWidth).W))
    val writeEnable = Output(Bool())
    val readEnable = Output(Bool())
    val readData = Input(UInt((ML * DataWidth).W))
    val readEmpty = Input(Bool())
    val writeFull = Input(Bool())
  })
  val prng = Module(new KeccakCore)
  prng.io.seed := io.seed
  prng.io.seedWrite := io.seedWrite
  prng.io.valid := !io.writeFull && prng.io.initialized

  // write prn to buffer
  io.writeData := prng.io.prngNumber.s.asUInt()
  io.writeEnable := prng.io.done
  io.readEnable := io.valid

  // get random number
  for (i <- 0 until ML) {
    io.prn(i) := io.readData(DataWidth * i + DataWidth - 1, DataWidth * i)
  }
  io.wb := true.B
  io.done := false.B
  io.busy := false.B

  val fsm = InstanciateFSM(new FSM{
    entryState(stateName = "Idle")
      .act{
        io.done := false.B
        io.busy := false.B
      }
      .when(io.valid && io.readEmpty).transferTo(destName = "Wait")
      .when(io.valid && !io.readEmpty).transferTo(destName = "Read")

    state(stateName = "Wait")
      .act{
        io.done := false.B
        io.busy := true.B
      }
      .when(!io.readEmpty).transferTo(destName = "Read")

    state(stateName = "Read")
      .act{
        io.done := true.B
        io.busy := false.B
      }
      .when(io.valid && io.readEmpty).transferTo(destName = "Wait")
      .when(io.valid && !io.readEmpty).transferTo(destName = "Read")
      .otherwise.transferToEnd
  })

//  val s = prng.io.prngNumber.s.asUInt()
//  for (i <- 0 until ML) {
//    io.prn(i) := s(DataWidth * i + DataWidth - 1, DataWidth * i)
//  }
//  io.wb := true.B
//  io.done := RegNext(io.valid)
//  io.busy := false.B
}

object elaborateKeccak extends App {
  chisel3.Driver.execute(args, () => new KeccakNoFifo)
}
