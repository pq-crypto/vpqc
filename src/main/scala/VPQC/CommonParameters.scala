
package VPQC

trait HasCommonParameters {
  val ML = 32
  val DataWidth = 16
}

//trait HasPQCCSR{
//  val CSRWIDTH : Int = 64
//  val CSRLENGTH : Int = 6 // 这里修改CSR寄存器的个数
//
//  // 把每个CSR的名字和编号写在下面，必须以CSR_开头
//  val csrBarretu = Input(UInt((DataWidth + 2).W))
//  val csrBound = Input(UInt((DataWidth * 2).W))
//  val csrBinomialk = Input(UInt(3.W))
//  val csrModulusq = Input(UInt(DataWidth.W))
//  val csrModulusLen = Input(UInt(5.W))
//}

trait HasPQCInstructions {
  val INSTR_QUANTITY = 12 // number of custom instructions

  /*  instruction encoding style  */
  //  func7	rs2	rs1	reserved	rd	00010	11

  // PQC
  val INSTR_FETCHRN = 0
  val INSTR_SAMPLEBINOMIAL = 1
  val INSTR_SAMPLEREJECTION = 2
  val INSTR_BUTTERFLY = 3
  val INSTR_IBUTTERFLY = 4
  val INSTR_CSRRW = 5
  val INSTR_CSRRWI = 6
  val INSTR_VLD = 7
  val INSTR_VST = 8

  val INSTR_VADD = 9
  val INSTR_VSUB = 10
  val INSTR_VMUL = 11
//
//  // LOAD STORE
//  val INSTR_VLD = 7
//  val INSTR_VLDS = 8
//  val INSTR_VLDX = 9
//  val INSTR_VST = 10
//  val INSTR_VSTS = 11
//  val INSTR_VSTX = 12

}
