
package VPQC

import chisel3.iotesters
import chisel3.iotesters.PeekPokeTester
import chisel3.util._
import scala.util._

class SamplersTest(c: Samplers) extends PeekPokeTester(c){

  // write seed
  var r = Random

  poke(c.io.csrs.csrBinomialk, log2Ceil(8))
  poke(c.io.csrs.csrModulusq, 12289)
  poke(c.io.csrs.csrBarretu, scala.math.pow(2, 29).toLong / 12289)
  poke(c.io.csrs.csrModulusLen, 14)
  poke(c.io.csrs.csrBound, scala.math.pow(2, 32).toLong / 12289 * 12289)

  // rejection sample test
  var iterNum = 16
  for (i <- 0 until iterNum) {
    poke(c.io.mode, 0)
    for (i <- 0 until 32) {
      poke(c.io.vectorReg1(i), r.nextInt() & 0xffff)
      poke(c.io.vectorReg2(i), r.nextInt() & 0xffff)
    }
    step(1)
  }

  // binomial(gauss) sample test
  for (i <- 0 until iterNum) {
    poke(c.io.mode, 1)
    for (i <- 0 until 32) {
      poke(c.io.vectorReg1(i), r.nextInt() & 0xffff)
      poke(c.io.vectorReg2(i), r.nextInt() & 0xffff)
    }
    step(1)
  }
}

object SamplersTestSimple extends App {
  iotesters.Driver.execute(Array(), () => new Samplers) {
    c => new SamplersTest(c)
  }
}

object SamplersTestMain extends App {
  iotesters.Driver.execute(Array("--backend-name", "verilator"), () => new Samplers) {
    c => new SamplersTest(c)
  }
}
