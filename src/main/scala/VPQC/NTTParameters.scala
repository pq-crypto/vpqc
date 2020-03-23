
package VPQC

import chisel3.util._
import scala.math._

// using Barrett reduction
//trait HasMRParameters {
//  val MRalpha = 16 + 1
//  val MRbeta = -2
//  val MRq = 12289
//  val MRu = (2 << (14 + MRalpha)) / MRq
//}

//trait HasNTTCommonParameters {
//  val DataWidth = 16
//  val VectorLength = 512
//  // 9 bits
//  val AddrWidth = log2Ceil(VectorLength)
//}

trait HasNTTParameters {
  /**
    *  For example in NEWHOPE512 parameters
    *  r = 10968, w = 3, w-1 = 8193, r-1 = 3656, n-1 = 12265
    *  w is root of unity, 3 ^ 512 = 1, 3 ^ 256 = 12289 - 1, 3 ^ 128 = 1479
    **/

  val ButterflyNum = 32
  val Dimension = 1024 //ntt dimension

  val WN = new Array[Int](9)
  for (i <- 0 until 9 - 1) {
    if (i == 0) {
      WN(i) = 3
    } else {
      WN(i) = (pow(WN(i-1), 2) % 12289).toInt
    }
  }
}