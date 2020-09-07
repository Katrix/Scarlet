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
  case object LCONST_1  extends OPCodeA[Nothing](0x0A)
  case object FCONST_0  extends OPCodeA[Nothing](0x0B)
  case object FCONST_1  extends OPCodeA[Nothing](0x0C)
  case object FCONST_2  extends OPCodeA[Nothing](0x0D)
  case object DCONST_0  extends OPCodeA[Nothing](0x0E)
  case object DCONST_1  extends OPCodeA[Nothing](0x0F)

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

  case object ILOAD_0 extends OPCodeA[Nothing](0x1A)
  case object ILOAD_1 extends OPCodeA[Nothing](0x1B)
  case object ILOAD_2 extends OPCodeA[Nothing](0x1C)
  case object ILOAD_3 extends OPCodeA[Nothing](0x1D)
  case object LLOAD_0 extends OPCodeA[Nothing](0x1E)
  case object LLOAD_1 extends OPCodeA[Nothing](0x1F)
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
  case object ALOAD_0 extends OPCodeA[Nothing](0x2A)
  case object ALOAD_1 extends OPCodeA[Nothing](0x2B)
  case object ALOAD_2 extends OPCodeA[Nothing](0x2C)
  case object ALOAD_3 extends OPCodeA[Nothing](0x2D)
  case object IALOAD  extends OPCodeA[Nothing](0x2E)
  case object LALOAD  extends OPCodeA[Nothing](0x2F)
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
  case class ASTORE[A](index: A) extends OPCodeA[A](0x3A)
  case object ISTORE_0           extends OPCodeA[Nothing](0x3B)
  case object ISTORE_1           extends OPCodeA[Nothing](0x3C)
  case object ISTORE_2           extends OPCodeA[Nothing](0x3D)
  case object ISTORE_3           extends OPCodeA[Nothing](0x3E)
  case object LSTORE_0           extends OPCodeA[Nothing](0x3F)
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
  case object DSTORE_3           extends OPCodeA[Nothing](0x4A)
  case object ASTORE_1           extends OPCodeA[Nothing](0x4B)
  case object ASTORE_2           extends OPCodeA[Nothing](0x4C)
  case object ASTORE_3           extends OPCodeA[Nothing](0x4D)
  case object ASTORE_4           extends OPCodeA[Nothing](0x4E)
  case object IASTORE            extends OPCodeA[Nothing](0x4F)
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
  case object DUP_X1  extends OPCodeA[Nothing](0x5A)
  case object DUP_X2  extends OPCodeA[Nothing](0x5B)
  case object DUP2    extends OPCodeA[Nothing](0x5C)
  case object DUP2_X1 extends OPCodeA[Nothing](0x5D)
  case object DUP2_X2 extends OPCodeA[Nothing](0x5E)
  case object SWAP    extends OPCodeA[Nothing](0x5F)

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
  case object FMUL  extends OPCodeA[Nothing](0x6A)
  case object DMUL  extends OPCodeA[Nothing](0x6B)
  case object IDIV  extends OPCodeA[Nothing](0x6C)
  case object LDIV  extends OPCodeA[Nothing](0x6D)
  case object FDIV  extends OPCodeA[Nothing](0x6E)
  case object DDIV  extends OPCodeA[Nothing](0x6F)
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
  case object ISHR  extends OPCodeA[Nothing](0x7A)
  case object LSHR  extends OPCodeA[Nothing](0x7B)
  case object LUSHR extends OPCodeA[Nothing](0x7D)
  case object IUSHR extends OPCodeA[Nothing](0x7C)
  case object IAND  extends OPCodeA[Nothing](0x7E)
  case object LAND  extends OPCodeA[Nothing](0x7F)
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
  case object L2D extends OPCodeA[Nothing](0x8A)
  case object F2I extends OPCodeA[Nothing](0x8B)
  case object F2L extends OPCodeA[Nothing](0x8C)
  case object F2D extends OPCodeA[Nothing](0x8D)
  case object D2I extends OPCodeA[Nothing](0x8E)
  case object D2L extends OPCodeA[Nothing](0x8F)
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
  case class IFNE[A](branchByte1: A, branchByte2: A)      extends OPCodeA[A](0x9A)
  case class IFLT[A](branchByte1: A, branchByte2: A)      extends OPCodeA[A](0x9B)
  case class IFGE[A](branchByte1: A, branchByte2: A)      extends OPCodeA[A](0x9C)
  case class IFGT[A](branchByte1: A, branchByte2: A)      extends OPCodeA[A](0x9D)
  case class IFLE[A](branchByte1: A, branchByte2: A)      extends OPCodeA[A](0x9E)
  case class IF_ICMPEQ[A](branchByte1: A, branchByte2: A) extends OPCodeA[A](0x9F)
  case class IF_ICMPNE[A](branchByte1: A, branchByte2: A) extends OPCodeA[A](0xA0)
  case class IF_ICMPLT[A](branchByte1: A, branchByte2: A) extends OPCodeA[A](0xA1)
  case class IF_ICMPGE[A](branchByte1: A, branchByte2: A) extends OPCodeA[A](0xA2)
  case class IF_ICMPGT[A](branchByte1: A, branchByte2: A) extends OPCodeA[A](0xA3)
  case class IF_ICMPLE[A](branchByte1: A, branchByte2: A) extends OPCodeA[A](0xA4)
  case class IF_ACMPEQ[A](branchByte1: A, branchByte2: A) extends OPCodeA[A](0xA5)
  case class IF_ACMPNE[A](branchByte1: A, branchByte2: A) extends OPCodeA[A](0xA6)

  case class GOTO[A](branchByte1: A, branchByte2: A) extends OPCodeA[A](0xA7)
  case class JSR[A](branchByte1: A, branchByte2: A)  extends OPCodeA[A](0xA8)
  case class RET[A](index: A)                        extends OPCodeA[A](0xA9)

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
  ) extends OPCodeA[A](0xAB)
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
  ) extends OPCodeA[A](0xAA)

  case object IRETURN extends OPCodeA[Nothing](0xAC)
  case object LRETURN extends OPCodeA[Nothing](0xAD)
  case object FRETURN extends OPCodeA[Nothing](0xAE)
  case object DRETURN extends OPCodeA[Nothing](0xAF)
  case object ARETURN extends OPCodeA[Nothing](0xB0)
  case object RETURN  extends OPCodeA[Nothing](0xB1)

  case class GETSTATIC[A](indexByte1: A, indexByte2: A) extends OPCodeA[A](0xB2)
  case class PUTSTATIC[A](indexByte1: A, indexByte2: A) extends OPCodeA[A](0xB3)
  case class GETFIELD[A](indexByte1: A, indexByte2: A)  extends OPCodeA[A](0xB4)
  case class PUTFIELD[A](indexByte1: A, indexByte2: A)  extends OPCodeA[A](0xB5)

  case class INVOKEVIRTUAL[A](indexByte1: A, indexByte2: A)                       extends OPCodeA[A](0xB6)
  case class INVOKESPECIAL[A](indexByte1: A, indexByte2: A)                       extends OPCodeA[A](0xB7)
  case class INVOKESTATIC[A](indexByte1: A, indexByte2: A)                        extends OPCodeA[A](0xB8)
  case class INVOKEINTERFACE[A](indexByte1: A, indexByte2: A, count: A, dummy: A) extends OPCodeA[A](0xB9)
  case class INVOKEDYNAMIC[A](indexByte1: A, indexByte2: A, dummy1: A, dummy2: A) extends OPCodeA[A](0xBA)

  case class NEW[A](indexByte1: A, indexByte2: A)       extends OPCodeA[A](0xBB)
  case class NEWARRAY[A](atype: A)                      extends OPCodeA[A](0xBC)
  case class ANEWARRAY[A](indexByte1: A, indexByte2: A) extends OPCodeA[A](0xBD)
  case object ARRAYLENGTH                               extends OPCodeA[Nothing](0xBE)

  case object ATHROW                                     extends OPCodeA[Nothing](0xBF)
  case class CHECKCAST[A](indexByte1: A, indexByte2: A)  extends OPCodeA[A](0xC0)
  case class INSTANCEOF[A](indexByte1: A, indexByte2: A) extends OPCodeA[A](0xC1)
  case object MONITORENTER                               extends OPCodeA[Nothing](0xC2)
  case object MONITOREXIT                                extends OPCodeA[Nothing](0xC3)

  sealed abstract class WIDE[A]                                                      extends OPCodeA[A](0xC4) with Product with Serializable
  case class WIDELOADSTORERET[A](code: A, indexByte1: A, indexByte2: A)              extends WIDE[A]
  case class WIDEIINC[A](indexByte1: A, indexByte2: A, constByte1: A, constByte2: A) extends WIDE[A]

  case class MULTIANEWARRAY[A](indexByte1: A, indexByte2: A, dimensions: A)            extends OPCodeA[A](0xC5)
  case class IFNULL[A](branchByte1: A, branchByte2: A)                                 extends OPCodeA[A](0xC6)
  case class IFNONNULL[A](branchByte1: A, branchByte2: A)                              extends OPCodeA[A](0xC7)
  case class GOTO_W[A](branchByte1: A, branchByte2: A, branchByte3: A, branchByte4: A) extends OPCodeA[A](0xC8)
  case class JSR_W[A](branchByte1: A, branchByte2: A, branchByte3: A, branchByte4: A)  extends OPCodeA[A](0xC9)
  case object BREAKPOINT                                                               extends OPCodeA[Nothing](0xCA)

  case object IMPDEP1 extends OPCodeA[Nothing](0xFE)
  case object IMPDEP2 extends OPCodeA[Nothing](0xFF)
}
