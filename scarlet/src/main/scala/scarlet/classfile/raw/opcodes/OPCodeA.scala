package scarlet.classfile.raw.opcodes

sealed abstract class OPCodeA[+A](val id: Int)
object OPCodeA {

  case object NOP         extends OPCodeA[Nothing](0x00)
  case object ACONST_NULL extends OPCodeA[Nothing](0x01)

  case object ICONST_M1 extends OPCodeA[Nothing](0x02)
  case object ICONST_0  extends OPCodeA[Nothing](0x03)
  case object ICONST_1  extends OPCodeA[Nothing](0x04)
  case object ICONST_2  extends OPCodeA[Nothing](0x05)
  case object ICONST_3  extends OPCodeA[Nothing](0x06)
  case object ICONST_4  extends OPCodeA[Nothing](0x07)
  case object ICONST_5  extends OPCodeA[Nothing](0x08)
  case object LCONST_0  extends OPCodeA[Nothing](0x09)
  case object LCONST_1  extends OPCodeA[Nothing](0x0a)
  case object FCONST_0  extends OPCodeA[Nothing](0x0b)
  case object FCONST_1  extends OPCodeA[Nothing](0x0c)
  case object FCONST_2  extends OPCodeA[Nothing](0x0d)
  case object DCONST_0  extends OPCodeA[Nothing](0x0e)
  case object DCONST_1  extends OPCodeA[Nothing](0x0f)

  case class BIPUSH[A](byte: A)            extends OPCodeA[A](0x10)
  case class SIPUSH[A](byte1: A, byte2: A) extends OPCodeA[A](0x11)

  case class LDC[A](index: A)                        extends OPCodeA[A](0x12)
  case class LDC_W[A](indexByte1: A, indexByte2: A)  extends OPCodeA[A](0x13)
  case class LDC2_W[A](indexByte1: A, indexByte2: A) extends OPCodeA[A](0x14)

  case class ILOAD[A](index: A) extends OPCodeA[A](0x15)
  case class LLOAD[A](index: A) extends OPCodeA[A](0x16)
  case class FLOAD[A](index: A) extends OPCodeA[A](0x17)
  case class DLOAD[A](index: A) extends OPCodeA[A](0x18)
  case class ALOAD[A](index: A) extends OPCodeA[A](0x19)

  case object ILOAD_0 extends OPCodeA[Nothing](0x1a)
  case object ILOAD_1 extends OPCodeA[Nothing](0x1b)
  case object ILOAD_2 extends OPCodeA[Nothing](0x1c)
  case object ILOAD_3 extends OPCodeA[Nothing](0x1d)
  case object LLOAD_0 extends OPCodeA[Nothing](0x1e)
  case object LLOAD_1 extends OPCodeA[Nothing](0x1f)
  case object LLOAD_2 extends OPCodeA[Nothing](0x20)
  case object LLOAD_3 extends OPCodeA[Nothing](0x21)
  case object FLOAD_0 extends OPCodeA[Nothing](0x22)
  case object FLOAD_1 extends OPCodeA[Nothing](0x23)
  case object FLOAD_2 extends OPCodeA[Nothing](0x24)
  case object FLOAD_3 extends OPCodeA[Nothing](0x25)
  case object DLOAD_0 extends OPCodeA[Nothing](0x26)
  case object DLOAD_1 extends OPCodeA[Nothing](0x27)
  case object DLOAD_2 extends OPCodeA[Nothing](0x28)
  case object DLOAD_3 extends OPCodeA[Nothing](0x29)
  case object ALOAD_0 extends OPCodeA[Nothing](0x2a)
  case object ALOAD_1 extends OPCodeA[Nothing](0x2b)
  case object ALOAD_2 extends OPCodeA[Nothing](0x2c)
  case object ALOAD_3 extends OPCodeA[Nothing](0x2d)
  case object IALOAD  extends OPCodeA[Nothing](0x2e)
  case object LALOAD  extends OPCodeA[Nothing](0x2f)
  case object FALOAD  extends OPCodeA[Nothing](0x30)
  case object DALOAD  extends OPCodeA[Nothing](0x31)
  case object AALOAD  extends OPCodeA[Nothing](0x32)
  case object BALOAD  extends OPCodeA[Nothing](0x33)
  case object CALOAD  extends OPCodeA[Nothing](0x34)
  case object SALOAD  extends OPCodeA[Nothing](0x35)

  case class ISTORE[A](index: A) extends OPCodeA[A](0x36)
  case class LSTORE[A](index: A) extends OPCodeA[A](0x37)
  case class FSTORE[A](index: A) extends OPCodeA[A](0x38)
  case class DSTORE[A](index: A) extends OPCodeA[A](0x39)
  case class ASTORE[A](index: A) extends OPCodeA[A](0x3a)
  case object ISTORE_0           extends OPCodeA[Nothing](0x3b)
  case object ISTORE_1           extends OPCodeA[Nothing](0x3c)
  case object ISTORE_2           extends OPCodeA[Nothing](0x3d)
  case object ISTORE_3           extends OPCodeA[Nothing](0x3e)
  case object LSTORE_0           extends OPCodeA[Nothing](0x3f)
  case object LSTORE_1           extends OPCodeA[Nothing](0x40)
  case object LSTORE_2           extends OPCodeA[Nothing](0x41)
  case object LSTORE_3           extends OPCodeA[Nothing](0x42)
  case object FSTORE_0           extends OPCodeA[Nothing](0x43)
  case object FSTORE_1           extends OPCodeA[Nothing](0x44)
  case object FSTORE_2           extends OPCodeA[Nothing](0x45)
  case object FSTORE_3           extends OPCodeA[Nothing](0x46)
  case object DSTORE_0           extends OPCodeA[Nothing](0x47)
  case object DSTORE_1           extends OPCodeA[Nothing](0x48)
  case object DSTORE_2           extends OPCodeA[Nothing](0x49)
  case object DSTORE_3           extends OPCodeA[Nothing](0x4a)
  case object ASTORE_1           extends OPCodeA[Nothing](0x4b)
  case object ASTORE_2           extends OPCodeA[Nothing](0x4c)
  case object ASTORE_3           extends OPCodeA[Nothing](0x4d)
  case object ASTORE_4           extends OPCodeA[Nothing](0x4e)
  case object IASTORE            extends OPCodeA[Nothing](0x4f)
  case object LASTORE            extends OPCodeA[Nothing](0x50)
  case object FASTORE            extends OPCodeA[Nothing](0x51)
  case object DASTORE            extends OPCodeA[Nothing](0x52)
  case object AASTORE            extends OPCodeA[Nothing](0x53)
  case object BASTORE            extends OPCodeA[Nothing](0x54)
  case object CASTORE            extends OPCodeA[Nothing](0x55)
  case object SASTORE            extends OPCodeA[Nothing](0x56)

  case object POP     extends OPCodeA[Nothing](0x57)
  case object POP2    extends OPCodeA[Nothing](0x58)
  case object DUP     extends OPCodeA[Nothing](0x59)
  case object DUP_X1  extends OPCodeA[Nothing](0x5a)
  case object DUP_X2  extends OPCodeA[Nothing](0x5b)
  case object DUP2    extends OPCodeA[Nothing](0x5c)
  case object DUP2_X1 extends OPCodeA[Nothing](0x5d)
  case object DUP2_X2 extends OPCodeA[Nothing](0x5e)
  case object SWAP    extends OPCodeA[Nothing](0x5f)

  case object IADD  extends OPCodeA[Nothing](0x60)
  case object LADD  extends OPCodeA[Nothing](0x61)
  case object FADD  extends OPCodeA[Nothing](0x62)
  case object DADD  extends OPCodeA[Nothing](0x63)
  case object ISUB  extends OPCodeA[Nothing](0x64)
  case object LSUB  extends OPCodeA[Nothing](0x65)
  case object FSUB  extends OPCodeA[Nothing](0x66)
  case object DSUB  extends OPCodeA[Nothing](0x67)
  case object IMUL  extends OPCodeA[Nothing](0x68)
  case object LMUL  extends OPCodeA[Nothing](0x69)
  case object FMUL  extends OPCodeA[Nothing](0x6a)
  case object DMUL  extends OPCodeA[Nothing](0x6b)
  case object IDIV  extends OPCodeA[Nothing](0x6c)
  case object LDIV  extends OPCodeA[Nothing](0x6d)
  case object FDIV  extends OPCodeA[Nothing](0x6e)
  case object DDIV  extends OPCodeA[Nothing](0x6f)
  case object IREM  extends OPCodeA[Nothing](0x70)
  case object LREM  extends OPCodeA[Nothing](0x71)
  case object FREM  extends OPCodeA[Nothing](0x72)
  case object DREM  extends OPCodeA[Nothing](0x73)
  case object INEG  extends OPCodeA[Nothing](0x74)
  case object LNEG  extends OPCodeA[Nothing](0x75)
  case object FNEG  extends OPCodeA[Nothing](0x76)
  case object DNEG  extends OPCodeA[Nothing](0x77)
  case object ISHL  extends OPCodeA[Nothing](0x78)
  case object LSHL  extends OPCodeA[Nothing](0x79)
  case object ISHR  extends OPCodeA[Nothing](0x7a)
  case object LSHR  extends OPCodeA[Nothing](0x7b)
  case object LUSHR extends OPCodeA[Nothing](0x7d)
  case object IUSHR extends OPCodeA[Nothing](0x7c)
  case object IAND  extends OPCodeA[Nothing](0x7e)
  case object LAND  extends OPCodeA[Nothing](0x7f)
  case object IOR   extends OPCodeA[Nothing](0x80)
  case object LOR   extends OPCodeA[Nothing](0x81)
  case object IXOR  extends OPCodeA[Nothing](0x82)
  case object LXOR  extends OPCodeA[Nothing](0x83)

  case class IINC[A](index: A, const: A) extends OPCodeA[A](0x84)

  case object I2L extends OPCodeA[Nothing](0x85)
  case object I2F extends OPCodeA[Nothing](0x86)
  case object I2D extends OPCodeA[Nothing](0x87)
  case object L2I extends OPCodeA[Nothing](0x88)
  case object L2F extends OPCodeA[Nothing](0x89)
  case object L2D extends OPCodeA[Nothing](0x8a)
  case object F2I extends OPCodeA[Nothing](0x8b)
  case object F2L extends OPCodeA[Nothing](0x8c)
  case object F2D extends OPCodeA[Nothing](0x8d)
  case object D2I extends OPCodeA[Nothing](0x8e)
  case object D2L extends OPCodeA[Nothing](0x8f)
  case object D2F extends OPCodeA[Nothing](0x90)
  case object I2B extends OPCodeA[Nothing](0x91)
  case object I2C extends OPCodeA[Nothing](0x92)
  case object I2S extends OPCodeA[Nothing](0x93)

  case object LCMP  extends OPCodeA[Nothing](0x94)
  case object FCMPL extends OPCodeA[Nothing](0x95)
  case object FCMPG extends OPCodeA[Nothing](0x96)
  case object DCMPL extends OPCodeA[Nothing](0x97)
  case object DCMPG extends OPCodeA[Nothing](0x98)

  case class IFEQ[A](branchByte1: A, branchByte2: A)      extends OPCodeA[A](0x99)
  case class IFNE[A](branchByte1: A, branchByte2: A)      extends OPCodeA[A](0x9a)
  case class IFLT[A](branchByte1: A, branchByte2: A)      extends OPCodeA[A](0x9b)
  case class IFGE[A](branchByte1: A, branchByte2: A)      extends OPCodeA[A](0x9c)
  case class IFGT[A](branchByte1: A, branchByte2: A)      extends OPCodeA[A](0x9d)
  case class IFLE[A](branchByte1: A, branchByte2: A)      extends OPCodeA[A](0x9e)
  case class IF_ICMPEQ[A](branchByte1: A, branchByte2: A) extends OPCodeA[A](0x9f)
  case class IF_ICMPNE[A](branchByte1: A, branchByte2: A) extends OPCodeA[A](0xa0)
  case class IF_ICMPLT[A](branchByte1: A, branchByte2: A) extends OPCodeA[A](0xa1)
  case class IF_ICMPGE[A](branchByte1: A, branchByte2: A) extends OPCodeA[A](0xa2)
  case class IF_ICMPGT[A](branchByte1: A, branchByte2: A) extends OPCodeA[A](0xa3)
  case class IF_ICMPLE[A](branchByte1: A, branchByte2: A) extends OPCodeA[A](0xa4)
  case class IF_ACMPEQ[A](branchByte1: A, branchByte2: A) extends OPCodeA[A](0xa5)
  case class IF_ACMPNE[A](branchByte1: A, branchByte2: A) extends OPCodeA[A](0xa6)

  case class GOTO[A](branchByte1: A, branchByte2: A) extends OPCodeA[A](0xa7)
  case class JSR[A](branchByte1: A, branchByte2: A)  extends OPCodeA[A](0xa8)
  case class RET[A](index: A)                        extends OPCodeA[A](0xa9)

  case class LOOKUPSWITCH[A](
      defaultbyte1: A,
      defaultbyte2: A,
      defaultbyte3: A,
      defaultbyte4: A,
      npairs1: A,
      npairs2: A,
      npairs3: A,
      npairs4: A,
      pairs: Vector[A]
  ) extends OPCodeA[A](0xab)
  case class TABLESWITCH[A](
      defaultbyte1: A,
      defaultbyte2: A,
      defaultbyte3: A,
      defaultbyte4: A,
      lowbyte1: A,
      lowbyte2: A,
      lowbyte3: A,
      lowbyte4: A,
      highbyte1: A,
      highbyte2: A,
      highbyte3: A,
      highbyte4: A,
      offsets: Vector[A]
  ) extends OPCodeA[A](0xaa)

  case object IRETURN extends OPCodeA[Nothing](0xac)
  case object LRETURN extends OPCodeA[Nothing](0xad)
  case object FRETURN extends OPCodeA[Nothing](0xae)
  case object DRETURN extends OPCodeA[Nothing](0xaf)
  case object ARETURN extends OPCodeA[Nothing](0xb0)
  case object RETURN  extends OPCodeA[Nothing](0xb1)

  case class GETSTATIC[A](indexByte1: A, indexByte2: A) extends OPCodeA[A](0xb2)
  case class PUTSTATIC[A](indexByte1: A, indexByte2: A) extends OPCodeA[A](0xb3)
  case class GETFIELD[A](indexByte1: A, indexByte2: A)  extends OPCodeA[A](0xb4)
  case class PUTFIELD[A](indexByte1: A, indexByte2: A)  extends OPCodeA[A](0xb5)

  case class INVOKEVIRTUAL[A](indexByte1: A, indexByte2: A)                       extends OPCodeA[A](0xb6)
  case class INVOKESPECIAL[A](indexByte1: A, indexByte2: A)                       extends OPCodeA[A](0xb7)
  case class INVOKESTATIC[A](indexByte1: A, indexByte2: A)                        extends OPCodeA[A](0xb8)
  case class INVOKEINTERFACE[A](indexByte1: A, indexByte2: A, count: A, dummy: A) extends OPCodeA[A](0xb9)
  case class INVOKEDYNAMIC[A](indexByte1: A, indexByte2: A, dummy1: A, dummy2: A) extends OPCodeA[A](0xba)

  case class NEW[A](indexByte1: A, indexByte2: A)       extends OPCodeA[A](0xbb)
  case class NEWARRAY[A](atype: A)                      extends OPCodeA[A](0xbc)
  case class ANEWARRAY[A](indexByte1: A, indexByte2: A) extends OPCodeA[A](0xbd)
  case object ARRAYLENGTH                               extends OPCodeA[Nothing](0xbe)

  case object ATHROW                                     extends OPCodeA[Nothing](0xbf)
  case class CHECKCAST[A](indexByte1: A, indexByte2: A)  extends OPCodeA[A](0xc0)
  case class INSTANCEOF[A](indexByte1: A, indexByte2: A) extends OPCodeA[A](0xc1)
  case object MONITORENTER                               extends OPCodeA[Nothing](0xc2)
  case object MONITOREXIT                                extends OPCodeA[Nothing](0xc3)

  sealed abstract class WIDE[A]                                                      extends OPCodeA[A](0xc4) with Product with Serializable
  case class WIDELOADSTORERET[A](code: A, indexByte1: A, indexByte2: A)              extends WIDE[A]
  case class WIDEIINC[A](indexByte1: A, indexByte2: A, constByte1: A, constByte2: A) extends WIDE[A]

  case class MULTIANEWARRAY[A](indexByte1: A, indexByte2: A, dimensions: A)            extends OPCodeA[A](0xc5)
  case class IFNULL[A](branchByte1: A, branchByte2: A)                                 extends OPCodeA[A](0xc6)
  case class IFNONNULL[A](branchByte1: A, branchByte2: A)                              extends OPCodeA[A](0xc7)
  case class GOTO_W[A](branchByte1: A, branchByte2: A, branchByte3: A, branchByte4: A) extends OPCodeA[A](0xc8)
  case class JSR_W[A](branchByte1: A, branchByte2: A, branchByte3: A, branchByte4: A)  extends OPCodeA[A](0xc9)
  case object BREAKPOINT                                                               extends OPCodeA[Nothing](0xca)

  case object IMPDEP1 extends OPCodeA[Nothing](0xfe)
  case object IMPDEP2 extends OPCodeA[Nothing](0xfF)
}
