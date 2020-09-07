package scarlet.classfile.raw

import java.io.InputStream

import scarlet.classfile.shared
import scodec._
import scodec.bits._
import scodec.codecs._
import scala.collection.mutable.ArrayBuffer

/**
  * A codec for classfiles.
  */
object ClassfileCodec {

  private lazy val magicNumber = constant(hex"CAFEBABE").withContext("Magic number")

  private lazy val version = (uint16 :: uint16).as[shared.Version].withContext("Version")

  private lazy val constantPoolEntry: Codec[ConstantPoolEntry] =
    uint8
      .narrow[shared.ConstantPoolTag](
        t => Attempt.fromOption(shared.ConstantPoolTag.tagForValue(t), Err.MatchingDiscriminatorNotFound(t, Nil)),
        _.value
      )
      .flatZip(_.codec)
      .xmap[ConstantPoolEntry](_._2, t => t.tag -> t)
      .withContext("Constant pool entry")

  private class ConstantPoolCodec(maxSize: Int) extends Codec[Vector[ConstantPoolEntry]] {
    override def decode(bits: BitVector): Attempt[DecodeResult[Vector[ConstantPoolEntry]]] = {
      //Mostly copied from Decoder.decodeCollect with extra logic to handle big entries like long and double

      val bldr               = ArrayBuffer.empty[ConstantPoolEntry]
      var remaining          = bits
      var count              = 1
      var error: Option[Err] = None
      while (count < maxSize) {
        constantPoolEntry.decode(remaining) match {
          case Attempt.Successful(DecodeResult(value, rest)) =>
            bldr += value
            count += value.tag.size
            remaining = rest
          case Attempt.Failure(err) =>
            error = Some(err.pushContext(count.toString))
            remaining = BitVector.empty
        }
      }

      Attempt.fromErrOption(error, DecodeResult(bldr.toVector, remaining))
    }

    override def encode(value: Vector[ConstantPoolEntry]): Attempt[BitVector] =
      Encoder.encodeSeq(constantPoolEntry)(value)

    override def sizeBound: SizeBound = SizeBound.exact((maxSize - 1) * 8)
  }

  private lazy val constantPool = uint16
    .flatZip(new ConstantPoolCodec(_))
    .xmap[Vector[ConstantPoolEntry]](_._2, v => v.map(_.tag.size).sum - 1 -> v)
    .as[ConstantPool]
    .withContext("Constant pool")

  private lazy val accessFlags = uint16.as[AccessFlags]
  private lazy val thisClass   = uint16.as[ThisClass]
  private lazy val superClass  = uint16.as[SuperClass]

  private lazy val interfaces = vectorOfN(uint16, uint16.as[InterfaceEntry]).as[Interfaces].withContext("Interfaces")

  private lazy val fieldInfo: Codec[FieldInfo] =
    (accessFlags :: uint16 :: uint16 :: attributes).as[FieldInfo].withContext("Field info")
  private lazy val fields = vectorOfN(uint16, fieldInfo).as[Fields].withContext("Fields")

  private lazy val methodInfo: Codec[MethodInfo] =
    (accessFlags :: uint16 :: uint16 :: attributes).as[MethodInfo].withContext("Method info")
  private lazy val methods = vectorOfN(uint16, methodInfo).as[Methods].withContext("Methods")

  private lazy val attributeInfo: Codec[AttributeInfo] =
    (uint16 :: uint32.flatZip(length => bytes(length.toInt)).xmap[ByteVector](_._2, b => (b.length, b)))
      .as[AttributeInfo]
      .withContext("Attribute info")
  lazy val attributes: Codec[Attributes] = vectorOfN(uint16, attributeInfo).as[Attributes].withContext("Attributes")

  private val classFile =
    (magicNumber ~> version :: constantPool :: accessFlags :: thisClass :: superClass :: interfaces :: fields :: methods :: attributes)
      .as[Classfile]
      .complete

  /**
    * Tries to parse a class given an input stream.
    */
  def parseClass(input: InputStream): Attempt[Classfile] =
    classFile.decodeValue(scodec.bits.BitVector.fromInputStream(input))
}
