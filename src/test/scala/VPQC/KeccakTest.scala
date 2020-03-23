
package VPQC

import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

// test value from: SHAKE128
// https://csrc.nist.gov/projects/cryptographic-standards-and-guidelines/example-values
class KeccakTest(c: KeccakCore) extends PeekPokeTester(c) {
  var message = new Array[Long](1600 / 64)
  for (i <- 0 until 1600 / 64) {
    if (i < 21) {
      message(i) = 0xA3A3A3A3A3A3A3A3L
    }
    else {
      message(i) = 0L
    }
  }
  for (y <- 0 until 5) {
    for (x <- 0 until 5) {
      poke(c.io.seed.s(y)(x), message(y * 5 + x))
    }
  }
  poke(c.io.seedWrite, 1)
  step(1)
  poke(c.io.seedWrite, 0)
  poke(c.io.valid, 1)
  step(1)
  poke(c.io.valid, 0)
  step(24)

  expect(c.io.done, 1)
  var res = new Array[BigInt](1600 / 64)
  for (y <- 0 until 5) {
    for (x <- 0 until 5) {
      res(y * 5 + x) = peek(c.io.prngNumber.s(y)(x))
      printf("res[%d] = %x\n", y * 5 + x, res(y * 5 + x))
    }
  }
}
object KeccakTestMain extends App {
  iotesters.Driver.execute(Array("--backend-name", "verilator"), () => new KeccakCore) {
    c => new KeccakTest(c)
  }
}

