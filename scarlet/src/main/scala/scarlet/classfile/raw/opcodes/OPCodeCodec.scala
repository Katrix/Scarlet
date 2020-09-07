package scarlet.classfile.raw.opcodes

import scala.collection.immutable.LongMap
import scarlet.classfile.raw.opcodes.OPCodeA._
import scodec._
import scodec.bits._
import scodec.codecs._
import shapeless._
import shapeless.ops.hlist._

import scala.annotation.{nowarn, unused}

/**
  * Contains codecs for a single opcode, and a sequence of them
  */
object OPCodeCodec {

  /**
    * A higher kinded type which will always be present. Used because we don't
    * care about the second parameter on Generic1.
    */
  private trait Trivial[F[_]]
  @unused private object Trivial {
    implicit def present[F[_]]: Trivial[F] = new Trivial[F] {}
  }
  private case class OPCodeEntry[A <: OPCode](code: Int, unapply: OPCode => Option[A], codec: Codec[A])

  private class PNPartiallyApplied[A[B] <: OPCodeA[B]](private val b: Boolean = true) extends AnyVal {
    def mk[Repr1[_] <: HList, ArgCount <: Nat](
        implicit gen: Generic1.Aux[A, Trivial, Repr1],
        @unused argCount: Length.Aux[Repr1[Unit], ArgCount],
        fillCodecs: Fill.Aux[ArgCount, Codec[Int], Repr1[Codec[Int]]],
        toHlistCoded: ToHListCodec.Aux[Repr1[Codec[Int]], Repr1[Int]]
    ): OPCodeEntry[A[Int]] = {
      val codecs = fillCodecs(uint8)
      val codeId = gen.from(codecs).id

      val codeCodec = toHlistCoded(codecs).xmap[A[Int]](gen.from, gen.to)
      val unapply: OPCode => Option[A[Int]] = code => {
        if (code.id == codeId) Some(code.asInstanceOf[A[Int]])
        else None
      }

      OPCodeEntry(codeId, unapply, codeCodec)
    }
  }

  private val tableSwitchId  = TABLESWITCH((), (), (), (), (), (), (), (), (), (), (), (), Vector.empty).id
  private val lookupSwitchId = LOOKUPSWITCH((), (), (), (), (), (), (), (), Vector.empty).id

  /**
    * Codec for opcodes that don't have any operands.
    */
  private def p0[A <: OPCodeA[Nothing]](code: A): OPCodeEntry[A] =
    OPCodeEntry(code.id, e => if (e == code) Some(code) else None, provide(code))

  /**
    * Codec builder for opcodes with N operands. Need to call [[PNPartiallyApplied.mk]]
    * to get the codec.
    */
  private def pN[A[B] <: OPCodeA[B]] = new PNPartiallyApplied[A]()

  private val tableSwitchCodec: Codec[TABLESWITCH[Int]] = (
    uint8 ~ uint8 ~ uint8 ~ uint8 ~
      uint8 ~ uint8 ~ uint8 ~ uint8 ~
      uint8 ~ uint8 ~ uint8 ~ uint8
  ).flatZip {
    case _ ~ _ ~ _ ~ _ ~ low1 ~ low2 ~ low3 ~ low4 ~ hi1 ~ hi2 ~ hi3 ~ hi4 =>
      val low  = (low1 << 24) | (low2 << 16) | (low3 << 8) | low4
      val high = (hi1 << 24) | (hi2 << 16) | (hi3 << 8) | hi4
      val num  = high - low + 1

      vectorOfN(provide(num * 4), uint8)
  }.xmapc[TABLESWITCH[Int]] {
    case def1 ~ def2 ~ def3 ~ def4 ~ low1 ~ low2 ~ low3 ~ low4 ~ hi1 ~ hi2 ~ hi3 ~ hi4 ~ offsets =>
      TABLESWITCH(def1, def2, def3, def4, low1, low2, low3, low4, hi1, hi2, hi3, hi4, offsets)
  } {
    case TABLESWITCH(def1, def2, def3, def4, low1, low2, low3, low4, hi1, hi2, hi3, hi4, offsets) =>
      def1 ~ def2 ~ def3 ~ def4 ~ low1 ~ low2 ~ low3 ~ low4 ~ hi1 ~ hi2 ~ hi3 ~ hi4 ~ offsets
  }

  private val tableSwitchEntry: OPCodeEntry[TABLESWITCH[Int]] = {
    val unapply: PartialFunction[OPCode, TABLESWITCH[Int]] = { case op: TABLESWITCH[Int] => op }

    OPCodeEntry(
      tableSwitchId,
      unapply.lift,
      tableSwitchCodec
    )
  }

  private val lookupSwitchCodec: Codec[LOOKUPSWITCH[Int]] = (
    uint8 ~ uint8 ~ uint8 ~ uint8 ~
      uint8 ~ uint8 ~ uint8 ~ uint8
  ).flatZip {
    case _ ~ _ ~ _ ~ _ ~ npairs1 ~ npairs2 ~ npairs3 ~ npairs4 =>
      val npairs = (npairs1 << 24) | (npairs2 << 16) | (npairs3 << 8) | npairs4

      vectorOfN(provide(npairs * 2 * 4), uint8)
  }.xmapc {
    case def1 ~ def2 ~ def3 ~ def4 ~ npairs1 ~ npairs2 ~ npairs3 ~ npairs4 ~ pairs =>
      LOOKUPSWITCH(def1, def2, def3, def4, npairs1, npairs2, npairs3, npairs4, pairs)
  } {
    case LOOKUPSWITCH(def1, def2, def3, def4, npairs1, npairs2, npairs3, npairs4, pairs) =>
      def1 ~ def2 ~ def3 ~ def4 ~ npairs1 ~ npairs2 ~ npairs3 ~ npairs4 ~ pairs
  }

  private val lookupSwitchEntry: OPCodeEntry[LOOKUPSWITCH[Int]] = {
    val unapply: PartialFunction[OPCode, LOOKUPSWITCH[Int]] = { case op: LOOKUPSWITCH[Int] => op }

    OPCodeEntry(
      lookupSwitchId,
      unapply.lift,
      lookupSwitchCodec
    )
  }

  private val iincId = IINC((), ()).id

  private val wideCodec = uint8
    .flatZip {
      case `iincId` =>
        (uint8 ~ uint8 ~ uint8 ~ uint8).xmapc {
          case idxByte1 ~ idxByte2 ~ constByte1 ~ constByte2 =>
            WIDEIINC(idxByte1, idxByte2, constByte1, constByte2): WIDE[Int]
        } {
          case WIDEIINC(indexByte1, indexByte2, constByte1, constByte2) =>
            indexByte1 ~ indexByte2 ~ constByte1 ~ constByte2
          case _ => sys.error("Impossible")
        }
      case otherCode =>
        (uint8 ~ uint8).xmapc {
          case idxByte1 ~ idxByte2 => WIDELOADSTORERET(otherCode, idxByte1, idxByte2): WIDE[Int]
        } {
          case WIDELOADSTORERET(_, indexByte1, indexByte2) => indexByte1 ~ indexByte2
          case _                                           => sys.error("Impossible")
        }
    }
    .xmapc[WIDE[Int]](_._2) {
      case op @ WIDEIINC(_, _, _, _)         => iincId -> (op: WIDE[Int])
      case op @ WIDELOADSTORERET(code, _, _) => code   -> (op: WIDE[Int])
    }

  private val wideEntry: OPCodeEntry[WIDE[Int]] = {
    val unapply: PartialFunction[OPCode, WIDE[Int]] = {
      case op: WIDEIINC[Int]         => op
      case op: WIDELOADSTORERET[Int] => op
    }

    OPCodeEntry(
      WIDELOADSTORERET((), (), ()).id,
      unapply.lift,
      wideCodec
    )
  }

  /**
    * A codec for a single opcode.
    */
  val codec: Codec[OPCode] = {
    @nowarn
    val opcodes: Seq[OPCodeEntry[_ <: OPCode]] = Seq(
      p0(NOP),
      p0(ACONST_NULL),
      p0(ICONST_M1),
      p0(ICONST_0),
      p0(ICONST_1),
      p0(ICONST_2),
      p0(ICONST_3),
      p0(ICONST_4),
      p0(ICONST_5),
      p0(LCONST_0),
      p0(LCONST_1),
      p0(FCONST_0),
      p0(FCONST_1),
      p0(FCONST_2),
      p0(DCONST_0),
      p0(DCONST_1),
      pN[BIPUSH].mk,
      pN[SIPUSH].mk,
      pN[LDC].mk,
      pN[LDC_W].mk,
      pN[LDC2_W].mk,
      pN[ILOAD].mk,
      pN[LLOAD].mk,
      pN[FLOAD].mk,
      pN[DLOAD].mk,
      pN[ALOAD].mk,
      p0(ILOAD_0),
      p0(ILOAD_1),
      p0(ILOAD_2),
      p0(ILOAD_3),
      p0(LLOAD_0),
      p0(LLOAD_1),
      p0(LLOAD_2),
      p0(LLOAD_3),
      p0(FLOAD_0),
      p0(FLOAD_1),
      p0(FLOAD_2),
      p0(FLOAD_3),
      p0(DLOAD_0),
      p0(DLOAD_1),
      p0(DLOAD_2),
      p0(DLOAD_3),
      p0(ALOAD_0),
      p0(ALOAD_1),
      p0(ALOAD_2),
      p0(ALOAD_3),
      p0(IALOAD),
      p0(LALOAD),
      p0(FALOAD),
      p0(DALOAD),
      p0(AALOAD),
      p0(BALOAD),
      p0(CALOAD),
      p0(SALOAD),
      pN[ISTORE].mk,
      pN[LSTORE].mk,
      pN[FSTORE].mk,
      pN[DSTORE].mk,
      pN[ASTORE].mk,
      p0(ISTORE_0),
      p0(ISTORE_1),
      p0(ISTORE_2),
      p0(ISTORE_3),
      p0(LSTORE_0),
      p0(LSTORE_1),
      p0(LSTORE_2),
      p0(LSTORE_3),
      p0(FSTORE_0),
      p0(FSTORE_1),
      p0(FSTORE_2),
      p0(FSTORE_3),
      p0(DSTORE_0),
      p0(DSTORE_1),
      p0(DSTORE_2),
      p0(DSTORE_3),
      p0(ASTORE_1),
      p0(ASTORE_2),
      p0(ASTORE_3),
      p0(ASTORE_4),
      p0(IASTORE),
      p0(LASTORE),
      p0(FASTORE),
      p0(DASTORE),
      p0(AASTORE),
      p0(BASTORE),
      p0(CASTORE),
      p0(SASTORE),
      p0(POP),
      p0(POP2),
      p0(DUP),
      p0(DUP_X1),
      p0(DUP_X2),
      p0(DUP2),
      p0(DUP2_X1),
      p0(DUP2_X2),
      p0(SWAP),
      p0(IADD),
      p0(LADD),
      p0(FADD),
      p0(DADD),
      p0(ISUB),
      p0(LSUB),
      p0(FSUB),
      p0(DSUB),
      p0(IMUL),
      p0(LMUL),
      p0(FMUL),
      p0(DMUL),
      p0(IDIV),
      p0(LDIV),
      p0(FDIV),
      p0(DDIV),
      p0(IREM),
      p0(LREM),
      p0(FREM),
      p0(DREM),
      p0(INEG),
      p0(LNEG),
      p0(FNEG),
      p0(DNEG),
      p0(ISHL),
      p0(LSHL),
      p0(ISHR),
      p0(LSHR),
      p0(LUSHR),
      p0(IUSHR),
      p0(IAND),
      p0(LAND),
      p0(IOR),
      p0(LOR),
      p0(IXOR),
      p0(LXOR),
      pN[IINC].mk,
      p0(I2L),
      p0(I2F),
      p0(I2D),
      p0(L2I),
      p0(L2F),
      p0(L2D),
      p0(F2I),
      p0(F2L),
      p0(F2D),
      p0(D2I),
      p0(D2L),
      p0(D2F),
      p0(I2B),
      p0(I2C),
      p0(I2S),
      p0(LCMP),
      p0(FCMPL),
      p0(FCMPG),
      p0(DCMPL),
      p0(DCMPG),
      pN[IFEQ].mk,
      pN[IFNE].mk,
      pN[IFLT].mk,
      pN[IFGE].mk,
      pN[IFGT].mk,
      pN[IFLE].mk,
      pN[IF_ICMPEQ].mk,
      pN[IF_ICMPNE].mk,
      pN[IF_ICMPLT].mk,
      pN[IF_ICMPGE].mk,
      pN[IF_ICMPGT].mk,
      pN[IF_ICMPLE].mk,
      pN[IF_ACMPEQ].mk,
      pN[IF_ACMPNE].mk,
      pN[GOTO].mk,
      pN[JSR].mk,
      pN[RET].mk,
      lookupSwitchEntry,
      tableSwitchEntry,
      p0(IRETURN),
      p0(LRETURN),
      p0(FRETURN),
      p0(DRETURN),
      p0(ARETURN),
      p0(RETURN),
      pN[GETSTATIC].mk,
      pN[PUTSTATIC].mk,
      pN[GETFIELD].mk,
      pN[PUTFIELD].mk,
      pN[INVOKEVIRTUAL].mk,
      pN[INVOKESPECIAL].mk,
      pN[INVOKESTATIC].mk,
      pN[INVOKEINTERFACE].mk,
      pN[INVOKEDYNAMIC].mk,
      pN[NEW].mk,
      pN[NEWARRAY].mk,
      pN[ANEWARRAY].mk,
      p0(ARRAYLENGTH),
      p0(ATHROW),
      pN[CHECKCAST].mk,
      pN[INSTANCEOF].mk,
      p0(MONITORENTER),
      p0(MONITOREXIT),
      wideEntry,
      pN[MULTIANEWARRAY].mk,
      pN[IFNULL].mk,
      pN[IFNONNULL].mk,
      pN[GOTO_W].mk,
      pN[JSR_W].mk,
      p0(BREAKPOINT),
      p0(IMPDEP1),
      p0(IMPDEP2)
    )

    opcodes.foldLeft(discriminated[OPCode].by(uint8)) {
      case (acc, OPCodeEntry(opcodeId, unapply, codec)) =>
        acc.subcaseO(opcodeId)(unapply)(codec)
    }
  }

  /**
    * A codec for a bunch of opcodes. The key of the resulting map contains
    * the offset from 0 that the given opcode is placed at. The keys are not
    * used when encoding.
    */
  val opCodesCodec: Codec[LongMap[OPCode]] =
    new Codec[LongMap[OPCode]] {
      override def sizeBound: SizeBound = SizeBound.atMost(65534L * 8L)

      private val tableSwitchIdVec  = BitVector.fromByte(tableSwitchId.toByte)
      private val lookupSwitchIdVec = BitVector.fromByte(lookupSwitchId.toByte)

      override def encode(value: LongMap[OPCode]): Attempt[BitVector] = {
        //Adapted from Encoder.encodeSeq

        val buf          = new collection.mutable.ArrayBuffer[BitVector](value.size)
        var writtenBytes = 0L
        value foreach {
          case (_, a) =>
            codec.encode(a) match {
              case Attempt.Successful(aa) =>
                if (aa.startsWith(tableSwitchIdVec) || aa.startsWith(lookupSwitchIdVec)) {
                  val modWritten = writtenBytes     % 4
                  val padding    = (4 - modWritten) % 4
                  val toWrite    = aa.padLeft(padding * 8)
                  writtenBytes += toWrite.length
                  buf += toWrite
                } else {
                  writtenBytes += aa.length
                  buf += aa
                }
              case Attempt.Failure(err) => {
                  return Attempt.failure(err.pushContext(buf.size.toString))
                }: @nowarn
            }
        }

        def merge(offset: Int, size: Int): BitVector = size match {
          case 0 => BitVector.empty
          case 1 => buf(offset)
          case _ =>
            val half = size / 2
            merge(offset, half) ++ merge(offset + half, half + (if (size % 2 == 0) 0 else 1))
        }

        Attempt.successful(merge(0, buf.size))
      }

      override def decode(bits: BitVector): Attempt[DecodeResult[LongMap[OPCode]]] = {
        //Adapted from Decoder.decodeCollect

        val bldr               = LongMap.newBuilder[OPCode]
        var remaining          = bits
        var count              = 0
        var error: Option[Err] = None
        var decodedBytes       = 0L

        // Max method size is 65534 bytes
        // https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.7.3
        while (decodedBytes < 65534 && remaining.nonEmpty) {
          if (remaining.startsWith(tableSwitchIdVec) || remaining.startsWith(lookupSwitchIdVec)) {
            //We add one here to account for the switch OPCode
            val modDecoded = (decodedBytes + 1) % 4
            val padding    = (4 - modDecoded)   % 4
            decodedBytes += padding

            val idToAddBack = if (remaining.startsWith(tableSwitchIdVec)) tableSwitchIdVec else lookupSwitchIdVec

            //We add one here to also drop the switch OpCode. We then add it back
            remaining = idToAddBack ++ remaining.drop((padding + 1) * 8)
          }

          codec.decode(remaining) match {
            case Attempt.Successful(DecodeResult(value, rest)) =>
              bldr += (decodedBytes -> value)
              count += 1

              val decodedAmount = remaining.bytes.length - rest.bytes.length

              decodedBytes += decodedAmount
              remaining = rest
            case Attempt.Failure(err) =>
              error = Some(err.pushContext(count.toString).pushContext(java.lang.Long.toHexString(decodedBytes)))
              remaining = BitVector.empty
          }
        }

        Attempt.fromErrOption(error, DecodeResult(bldr.result(), remaining))
      }
    }.complete

}
