
package VPQC

import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import chisel3.util._

import scala.util._


//class MRTest(c: MR) extends PeekPokeTester(c) {
//  var a = new scala.util.Random
//  poke(c.io.n, 14)
//  for (i <- 0 until 1000) {
//    var aIn = a.nextInt(1 << 28)
//    poke(c.io.a, aIn)
//    var m = (1 << 13) + a.nextInt(1 << 13)
//    poke(c.io.m, m)
//    poke(c.io.u, scala.math.pow(2, 29).toLong / m)
//    step(1)
//    expect(c.io.ar, aIn % m)
//  }
//}
//
//object MRTestMain extends App {
//  iotesters.Driver.execute(Array(), () => new MR) {
//    c => new MRTest(c)
//  }
//}

class BTest(c: Butterfly) extends PeekPokeTester(c) {
  var s = new scala.util.Random
  poke(c.io.n, 14)

  for (i <- 0 until 1000) {
    var m = (1 << 13) + s.nextInt(1 << 13)
    poke(c.io.m, m)
    poke(c.io.u, scala.math.pow(2, 29).toLong / m)

    var a = s.nextInt(m)
    var b = s.nextInt(m)
    var wn = s.nextInt(m)
    poke(c.io.a, a)
    poke(c.io.b, b)
    poke(c.io.wn, wn)

    step(1)
    var mul = (b * wn) % m
    var expectB = 0
    if (a < mul) {
      expectB = a + m- mul
    } else {
      expectB = a - mul
    }
    expect(c.io.aout, (a + mul) % m)
    expect(c.io.bout, expectB)
  }
}

object BTestMain extends App {
  iotesters.Driver.execute(Array(), () => new Butterfly) {
    c => new BTest(c)
  }
}

class PermTest(c: PermNetIn) extends PeekPokeTester(c)
  with HasNTTParameters {
  var r = scala.util.Random
  for (i <- 0 until 4) {
    for (j <- 0 until 1) {
      for (k <- 0 until ButterflyNum * 2) {
        poke(c.io.in(k), r.nextInt(1 << 16))
      }
      for (k <- 0 until 4) {
        poke(c.io.config(k), 0)
      }
      poke(c.io.config(i), 1)
      print("Config:\t" + peek(c.io.config) + "\n")
      for (k <- 0 until ButterflyNum * 2) {
        print(k + ":\t" + peek(c.io.in(k)) + "->" + peek(c.io.out(k)) + "\n")
      }
    }
  }
}

object PermTestMain extends App {
  iotesters.Driver.execute(Array(), () => new PermNetIn) {
    c => new PermTest(c)
  }
}

class NTTTest(c: NTT) extends PeekPokeTester(c)
  with HasNTTParameters {
  // test butterfly

  var r = Random
  // configure csrs
  poke(c.io.mode, 0)
  poke(c.io.valid, 0)

  poke(c.io.csrs.csrBinomialk, log2Ceil(8))
  poke(c.io.csrs.csrModulusq, 12289)
  poke(c.io.csrs.csrBarretu, scala.math.pow(2, 29).toLong / 12289)
  poke(c.io.csrs.csrModulusLen, 14)
  poke(c.io.csrs.csrBound, scala.math.pow(2, 32).toLong / 12289 * 12289)
  poke(c.io.csrs.csrButterflyCtrl.stageCfg, 0)
  poke(c.io.csrs.csrButterflyCtrl.iterCfg, 0)

  // 1: DIF
  // 0: DIT
  var s = new Array[Int](256)
  for (i <- 0 until 8) {
    for (j <- 0 until 32) {
      s(32*i + j) = r.nextInt() & 0xffff
    }
  }

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
  var fill_time = 0
  for (i <- 0 until 256 / ButterflyNum) {
    poke(c.io.wa, i)
    a = 0
    for (j <- 0 until ButterflyNum) {
      a = a | ((dataBuf(i)(j) & 0xffff) << (16 * j))
    }
    poke(c.io.di, a)
    poke(c.io.we, true)
    step(1)
    fill_time += 1
  }
  poke(c.io.we, false)
  step(1)
  fill_time += 1

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
  var sit = new Array[Int](256)
  for (i <- 0 until 256) {
    sit(i) = s(i)
  }
  for(ii <- 1 to 8) {
    var i = 1 << ii
    if(ButterflyNum >= i/2) {
      for(j <- 0 to 255 by 2*ButterflyNum) {
        for(k1 <- 0 to 2*ButterflyNum-1 by i) {
          for(k2 <- 0 to i/2 - 1) {
            var aIn = sit(k1 + k2 + j)
            var bIn = sit(k1 + k2 + j + i / 2)
            var mul = (bIn * w256(256 * k2 / i)) % 12289
            if (aIn < mul) {
              sit(k1 + k2 + j + i / 2) = aIn + 12289 - mul
            } else {
              sit(k1 + k2 + j + i / 2) = aIn - mul
            }
            sit(k1 + k2 + j) = (aIn + mul) % 12289
          }
        }
      }
    }
    else {
      for(k1 <- 0 to i/2 - 1 by ButterflyNum) {
        for (j <- 0 to 255 by i) {
          for (k2 <- 0 to ButterflyNum - 1) {
            var aIn = sit(k1 + k2 + j)
            var bIn = sit(k1 + k2 + j + i / 2)
            var mul = (bIn * w256(256 * (k1+k2) / i)) % 12289
            if (aIn < mul) {
              sit(k1 + k2 + j + i / 2) = aIn + 12289 - mul
            } else {
              sit(k1 + k2 + j + i / 2) = aIn - mul
            }
            sit(k1 + k2 + j) = (aIn + mul) % 12289
          }
        }
      }
    }
  }

  var ntt_time = 0
  poke(c.io.mode, 0)
  poke(c.io.valid, 1)
  for (stage <- 0 until 8) {
    var i = 2 << stage
    if(stage < 6) {
      poke(c.io.valid, 0)
      poke(c.io.csrs.csrButterflyCtrl.stageCfg, stage)
      poke(c.io.csrs.csrButterflyCtrl.iterCfg, 0)
      step(1)
      ntt_time += 1
      poke(c.io.valid, 1)
      for (iter <- 0 until 4) {
        for (j <- 0 until 32) {
          poke(c.io.vectorReg1(j), s(iter * 2 * 32 + j))
        }
        for (j <- 0 until 32) {
          poke(c.io.vectorReg2(j), s((iter * 2 + 1) * 32 + j))
        }
        step(1)
        ntt_time += 1
        var res = peek(c.io.dataOut)
        for (j <- 0 until 32) {
          s(iter * 2 * 32 + j) = res(j).toInt & 0xffff
        }
        for (j <- 0 until 32) {
          s((iter * 2 + 1) * 32 + j) = res(32 + j).toInt & 0xffff
        }
      }
    }
    else {
      for (iter <- 0 until i / (2 * ButterflyNum)) {
        poke(c.io.valid, 0)
        poke(c.io.csrs.csrButterflyCtrl.stageCfg, stage)
        poke(c.io.csrs.csrButterflyCtrl.iterCfg, iter)
        step(1)
        ntt_time += 1
        poke(c.io.valid, 1)
        for (k <- 0 until 256 / i) {
          for (j <- 0 until 32) {
            poke(c.io.vectorReg1(j), s((2 * k * i / (2 * ButterflyNum) + iter) * 32 + j))
          }
          for (j <- 0 until 32) {
            poke(c.io.vectorReg2(j), s(((2 * k + 1) * i / (2 * ButterflyNum) + iter) * 32 + j ))
          }
          step(1)
          ntt_time += 1
          var res = peek(c.io.dataOut)
          for (j <- 0 until 32) {
            s((2 * k * i / (2 * ButterflyNum) + iter) * 32 + j) = res(j).toInt & 0xffff
          }
          for (j <- 0 until 32) {
            s(((2 * k + 1) * i / (2 * ButterflyNum) + iter) * 32 + j ) = res(32+j).toInt & 0xffff
          }
        }
      }
    }
  }
  poke(c.io.valid, 0)
  step(1)
  ntt_time += 1

  // expect
  for (i <- 0 until 256) {
    // iterative | recursive | harware
    printf("%d: 0x%x\t%d: 0x%x\t%d: 0x%x\n", i, sit(i), i, rawRes(i), i, s(i))
  }

  // performance
  printf("NTT time(Dimension = 256): %d cycles\n", ntt_time)
}

object NTTTestSimple extends App {
  iotesters.Driver.execute(Array(), () => new NTT) {
    c => new NTTTest(c)
  }
}

object NTTTestMain extends App {
  iotesters.Driver.execute(Array("--backend-name", "verilator"), () => new NTT) {
    c => new NTTTest(c)
  }
}

//class NTTTest(c:NTTR2MDC) extends PeekPokeTester(c) {
//
//  var len = 512
//  def ntt(x: Array[Int]): Array[Int] = {
//    var wn = new Array[Int](len)
//    for (i <- 0 until len) {
//      if (i == 0){
//        wn(i) = 1
//      } else {
//        wn(i) = (wn(i-1) * 3) % 12289
//      }
//    }
//    var res = new Array[Int](len)
//    for (i <- 0 until len) {
//      res(i) = 0
//    }
//    for (i <- 0 until len) {
//      for (j <- 0 until len) {
//        res(i) += (x(j) * wn((i*j) % len)) % 12289
//      }
//      res(i) = res(i) % 12289
//    }
//    res
//  }
//
//  def range(a: Int, upBound: Int, downBound: Int) : Int = {
//    assert(upBound < 32)
//    assert(downBound >= 0)
//    return (a >> downBound) & (0xffffffff >>> (31-upBound+downBound))
//  }
//
//  def reverse(a: Int, len: Int): Int = {
//    var res: Int = 0
//    for(i <- 0 until len) {
//      res = res | range(a, i, i) << (len-1-i)
//    }
//    res
//  }
//
//  var l = 14
//  val r = new scala.util.Random
//  var bound: Double = math.pow(2.0, l)
//  var iterNum: Int = 100
//
//  for (t <- 0 until iterNum) {
//    var a = new Array[Int](len)
//    for (i <- 0 until len) {
//      a(i) = r.nextInt(bound.toInt)
//      poke(c.io.dIn, a(i) & 0x3fff)
//      poke(c.io.dInValid, 1)
//      step(1)
//    }
//    var ref = ntt(a)
//
//    for (i <- 0 until len / 2) {
//      var ref1 = ref(reverse(i * 2, log2Ceil(len)))
//      expect(c.io.dOut1, ref1)
//
//      var ref2 = ref(reverse(i * 2 + 1, log2Ceil(len)))
//      expect(c.io.dOut2, ref2)
//
//      expect(c.io.dOutValid, 1)
//
//      a(reverse(i * 2, log2Ceil(len))) = peek(c.io.dOut1).toInt
//      a(reverse(i * 2 + 1, log2Ceil(len))) = peek(c.io.dOut2).toInt
//      step(1)
//    }
////    for (i <- 0 until len) {
////      print(ref(i) + "\n")
////    }
////    for (i <- 0 until len) {
////      print(a(i) + "\n")
////    }
//  }
//}
//
//object NTTTestMain extends App {
//  iotesters.Driver.execute(Array(), () => new NTTR2MDC) {
//    c => new NTTTest(c)
//  }
//}