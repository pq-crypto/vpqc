
package VPQC

import chisel3.iotesters
import chisel3.iotesters.PeekPokeTester
import chisel3.util._
import scala.util._

class ProcTest(c: PQCCoprocessor) extends PeekPokeTester(c)
  with HasNTTParameters
  with HasCommonParameters
  with HasPQCInstructions {

  /*  instruction encoding style  */
  //  func7	rs2	rs1	reserved	rd	00010	11
  def instr(f: Int, rs1: Int, rs2: Int, rd: Int): Int= {
    (f << 25) | (rs2 << 20) | (rs1 << 15) | (rd << 7) | 11
  }

  // init
  var r = Random
  poke(c.io.instr.rs1, 0)
  poke(c.io.instr.rs2, 0)
  poke(c.io.instr.rd, 0)
  poke(c.io.instr.funct, INSTR_FETCHRN) // nop
  poke(c.io.in_fire, 0)

  poke(c.io.twiddleData, 0)
  poke(c.io.twiddleAddr, 0)
  poke(c.io.twiddleWrite, false)
  step(1)

  // configure csrs
  poke(c.io.in_fire, 1)
  poke(c.io.instr.funct, INSTR_CSRRW)
  poke(c.io.instr.rs2, 0)
  poke(c.io.rs1, scala.math.pow(2, 29).toLong / 12289)
  step(1)
  poke(c.io.instr.rs2, 1)
  poke(c.io.rs1, scala.math.pow(2, 32).toLong / 12289 * 12289)
  step(1)
  poke(c.io.instr.rs2, 2)
  poke(c.io.rs1, log2Ceil(8))
  step(1)
  poke(c.io.instr.rs2, 3)
  poke(c.io.rs1, 12289)
  step(1)
  poke(c.io.instr.rs2, 4)
  poke(c.io.rs1, 14)
  step(1)
  poke(c.io.instr.rs2, 5)
  poke(c.io.rs1, 0 << 6 | 0)
  step(1)
  poke(c.io.in_fire, 0)

  // write seed
  for (y <- 0 until 5) {
    for (x <- 0 until 5) {
      poke(c.io.seed.s(y)(x), r.nextLong())
    }
  }
  poke(c.io.seedWrite, 1)
  step(1)
  poke(c.io.seedWrite, 0)


  // waite for the buffer-filling
  var sample_time = 0
  step(24 * 16 + 1)
  sample_time += 24*16 + 1

  // test fetchrn
  poke(c.io.in_fire, 1)
  poke(c.io.instr.funct, INSTR_FETCHRN)
  poke(c.io.instr.rs1, 0)
  poke(c.io.instr.rs2, 0)
  poke(c.io.instr.rd, 0)

  for (i <- 0 until 16) {
    poke(c.io.instr.rd, i)
    step(1)
    sample_time += 1
  }
  poke(c.io.in_fire, 0)
  step(1)
  sample_time += 1

  // test sample
  poke(c.io.in_fire, 1)
  poke(c.io.instr.funct, INSTR_SAMPLEBINOMIAL)

  for (i <- 0 until 8) {
    poke(c.io.instr.rs1, 2 * i)
    poke(c.io.instr.rs2, 2 * i + 1)
    poke(c.io.instr.rd, i)
    step(1)
    sample_time += 1
  }
  poke(c.io.in_fire, 0)
  step(1)
  sample_time += 1

  // peek sampled value
  var s = new Array[Int](256)
  poke(c.io.in_fire, 1)
  for (i <- 0 until 8) {
    poke(c.io.instr.funct, INSTR_VST)
    poke(c.io.instr.rs1, i)
    step(1)
    for (j <- 0 until 32) {
      s(32*i + j) = peek(c.io.vectorOut(j)).toInt
    }
  }
  poke(c.io.in_fire, 0)

  // write const ram
  var w256 = new Array[Int](256)
  for (i <- 0 until 256) {
    if (i == 0) {
      w256(i) = 1
    } else {
      w256(i) = (w256(i-1) * 9) % 12289
    }
  }

  var dataBuf = Array.ofDim[BigInt](256 / ButterflyNum, ButterflyNum)
  var idx = 0
  var div = 64
  for (i <- 0 until 256 / ButterflyNum) {
    for (j <- 0 until ButterflyNum) {
      if (i == 0) {
        dataBuf(i)(0) = w256(0)
        dataBuf(i)(1) = w256(0)
        dataBuf(i)(2) = w256(0)
        dataBuf(i)(3) = w256(256/4)
        dataBuf(i)(4) = w256(0)
        dataBuf(i)(5) = w256(256/8)
        dataBuf(i)(6) = w256(256*2/8)
        dataBuf(i)(7) = w256(256*3/8)
        dataBuf(i)(8) = w256(0)
        dataBuf(i)(9) = w256(256/16)
        dataBuf(i)(10) = w256(256*2/16)
        dataBuf(i)(11) = w256(256*3/16)
        dataBuf(i)(12) = w256(256*4/16)
        dataBuf(i)(13) = w256(256*5/16)
        dataBuf(i)(14) = w256(256*6/16)
        dataBuf(i)(15) = w256(256*7/16)
        dataBuf(i)(16) = w256(0)
        dataBuf(i)(17) = w256(256/32)
        dataBuf(i)(18) = w256(256*2/32)
        dataBuf(i)(19) = w256(256*3/32)
        dataBuf(i)(20) = w256(256*4/32)
        dataBuf(i)(21) = w256(256*5/32)
        dataBuf(i)(22) = w256(256*6/32)
        dataBuf(i)(23) = w256(256*7/32)
        dataBuf(i)(24) = w256(256*8/32)
        dataBuf(i)(25) = w256(256*9/32)
        dataBuf(i)(26) = w256(256*10/32)
        dataBuf(i)(27) = w256(256*11/32)
        dataBuf(i)(28) = w256(256*12/32)
        dataBuf(i)(29) = w256(256*13/32)
        dataBuf(i)(30) = w256(256*14/32)
        dataBuf(i)(31) = w256(256*15/32)
      } else {
        dataBuf(i)(j) = w256(256 * idx / div)
        idx = idx + 1
      }
    }
    if (idx == (div / 2)) {
      idx = 0
      div = div * 2
    }
  }

  var a:BigInt = 0
  for (i <- 0 until 256 / ButterflyNum) {
    poke(c.io.twiddleAddr, i)
    a = 0
    for (j <- 0 until ButterflyNum) {
      a = a | ((dataBuf(i)(j) & 0xffff) << (16 * j))
    }
    poke(c.io.twiddleData, a)
    poke(c.io.twiddleWrite, true)
    step(1)
  }
  poke(c.io.twiddleWrite, false)
  step(1)

  // raw ntt
  var len = 256
  def ntt(x: Array[Int]): Array[Int] = {
    var res = new Array[Int](len)
    for (i <- 0 until len) {
      res(i) = 0
    }
    for (i <- 0 until len) {
      for (j <- 0 until len) {
        res(i) += (x(j) * w256((i*j) % len)) % 12289
      }
      res(i) = res(i) % 12289
    }
    res
  }
  def range(a: Int, upBound: Int, downBound: Int) : Int = {
    assert(upBound < 32)
    assert(downBound >= 0)
    return (a >> downBound) & (0xffffffff >>> (31-upBound+downBound))
  }
  def reverse(a: Int, len: Int): Int = {
    var res: Int = 0
    for(i <- 0 until len) {
      res = res | range(a, i, i) << (len-1-i)
    }
    res
  }
  var sInOrder = new Array[Int](256)
  for (i <- 0 until 256) {
    sInOrder(reverse(i, log2Ceil(len))) = s(i)
  }
  val rawRes = ntt(sInOrder)

  // iterative ntt (assume bit reverse)
  for(ii <- 1 to 8) {
    var i = 1 << ii
    if(ButterflyNum >= i/2) {
      for(j <- 0 to 255 by 2*ButterflyNum) {
        for(k1 <- 0 to 2*ButterflyNum-1 by i) {
          for(k2 <- 0 to i/2 - 1) {
            var aIn = s(k1 + k2 + j)
            var bIn = s(k1 + k2 + j + i / 2)
            var mul = (bIn * w256(256 * k2 / i)) % 12289
            if (aIn < mul) {
              s(k1 + k2 + j + i / 2) = aIn + 12289 - mul
            } else {
              s(k1 + k2 + j + i / 2) = aIn - mul
            }
            s(k1 + k2 + j) = (aIn + mul) % 12289
          }
        }
      }
    }
    else {
      for(k1 <- 0 to i/2 - 1 by ButterflyNum) {
        for (j <- 0 to 255 by i) {
          for (k2 <- 0 to ButterflyNum - 1) {
            var aIn = s(k1 + k2 + j)
            var bIn = s(k1 + k2 + j + i / 2)
            var mul = (bIn * w256(256 * (k1+k2) / i)) % 12289
            if (aIn < mul) {
              s(k1 + k2 + j + i / 2) = aIn + 12289 - mul
            } else {
              s(k1 + k2 + j + i / 2) = aIn - mul
            }
            s(k1 + k2 + j) = (aIn + mul) % 12289
          }
        }
      }
    }
  }

  // test butterfly
  var ntt_time = 0
  poke(c.io.in_fire, 1)
  for (stage <- 0 until 8) {
    var i = 2 << stage
    if(stage < 6) {
      poke(c.io.instr.funct, INSTR_CSRRW)
      poke(c.io.instr.rs2, 5)
      poke(c.io.rs1, stage << 6 | 0)
      step(1)
      ntt_time += 1
      poke(c.io.instr.funct, INSTR_BUTTERFLY)
      for (iter <- 0 until 4) {
        poke(c.io.instr.rs1, iter * 2)
        poke(c.io.instr.rs2, iter * 2 + 1)
        step(1)
        ntt_time += 1
      }
    }
    else {
      for (iter <- 0 until i / (2 * ButterflyNum)) {
        poke(c.io.instr.funct, INSTR_CSRRW)
        poke(c.io.instr.rs2, 5)
        poke(c.io.rs1, stage << 6 | iter)
        // simulate CSRRW
        step(1)
        ntt_time += 1
        poke(c.io.instr.funct, INSTR_BUTTERFLY)
        for (k <- 0 until 256 / i) {
          poke(c.io.instr.rs1, 2 * k * i / (2 * ButterflyNum) + iter)
          poke(c.io.instr.rs2, (2 * k + 1) * i / (2 * ButterflyNum) + iter)
          step(1)
          ntt_time += 1
        }
      }
    }
  }
  poke(c.io.in_fire, 0)
  step(1)
  ntt_time += 1

  // peek ntt value
  var nttRes = new Array[Int](256)
  poke(c.io.in_fire, 1)
  for (i <- 0 until 8) {
    poke(c.io.instr.funct, INSTR_VST)
    poke(c.io.instr.rs1, i)
    step(1)
    for (j <- 0 until 32) {
      nttRes(32 * i + j) = peek(c.io.vectorOut(j)).toInt
    }
  }
  poke(c.io.in_fire, 0)

  // expect
  for (i <- 0 until 256) {
    // iterative | recursive | harware
    printf("%d: 0x%x\t%d: 0x%x\t%d: 0x%x\n", i, s(i), i, rawRes(i), i, nttRes(i))
  }
  // performance
  printf("Binomial Sampling time(Dimension = 256): %d cycles\n", sample_time)
  printf("NTT time(Dimension = 256): %d cycles\n", ntt_time)
}

object TestTopTestSimple extends App {
  iotesters.Driver.execute(Array(), () => new PQCCoprocessor) {
    c => new ProcTest(c)
  }
}

object TestTopTestMain extends App {
  iotesters.Driver.execute(Array("--backend-name", "verilator"), () => new PQCCoprocessor) {
    c => new ProcTest(c)
  }
}
