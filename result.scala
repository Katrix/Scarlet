@scala.reflect.ScalaSignature(
  bytes =
    "\\u0006\\u0001\\r3A!\\u0001\\u0002\\u0001\\u000B\\t)!)Y:jG*\\t1!A\\u0004=K6\\u0004H/\\u001F \\u0004\\u0001M\\u0011\\u0001A\\u0002\\t\\u0003\\u000F)i\\u0011\\u0001\\u0003\\u0006\\u0002\\u0013\\u0005)1oY1mC&\\u00111\\u0002\\u0003\\u0002\\u0007\\u0003:L(+\\u001A4\\t\\u000B5\\u0001A\\u0011\\u0001\\b\\u0002\\rqJg.\\u001B;?)\\u0005y\\u0001C\\u0001\\t\\u0001\\u001B\\u0005\\u0011\\u0001\\\"\\u0002\\n\\u0001\\t\\u0003\\u0019\\u0012AA32)\\t!r\\u0003\\u0005\\u0002\\b+%\\u0011a\\u0003\\u0003\\u0002\\u0004\\u0013:$\\b\\\"\\u0002\\r\\u0012\\u0001\\u0004!\\u0012!A1\\t\\u000Bi\\u0001A\\u0011A\\u000E\\u0002\\u0005\\u0015\\u0014Dc\\u0001\\u000B\\u001D;!)\\u0001$\\u0007a\\u0001)!)a$\\u0007a\\u0001)\\u0005\\t!\\rC\\u0003!\\u0001\\u0011\\u0005\\u0011%\\u0001\\u0002fgQ!ACI\\u0012%\\u0011\\u0015Ar\\u00041\\u0001\\u0015\\u0011\\u0015qr\\u00041\\u0001\\u0015\\u0011\\u0015)s\\u00041\\u0001\\u0015\\u0003\\u0005\\u0019\\u0007\\\"B\\u0014\\u0001\\t\\u0003A\\u0013a\\u00014bGR\\u0011A#\\u000B\\u0005\\u00061\\u0019\\u0002\\r\\u0001\\u0006\\u0005\\u0006W\\u0001!\\t\\u0001L\\u0001\\no\\\"LG.\\u001A\\\"pIf$\\\"\\u0001F\\u0017\\t\\u000B9R\\u0003\\u0019\\u0001\\u000B\\u0002\\u00075\\f\\u0007\\u0010C\\u00031\\u0001\\u0011\\u0005\\u0011'A\\u0006uC&d'/Z2C_\\u0012LHC\\u0001\\u000B3\\u0011\\u0015qs\\u00061\\u0001\\u0015\\u0011\\u0015!\\u0004\\u0001\\\"\\u00016\\u0003%i\\u0017\\r^2i\\u0013:$\\u0018\\u0007\\u0006\\u0002\\u0015m!)qg\\ra\\u0001)\\u0005\\t\\u0011\\u000EC\\u0003:\\u0001\\u0011\\u0005!(A\\u0005nCR\\u001C\\u0007.\\u00138ueQ\\u0011Ac\\u000F\\u0005\\u0006oa\\u0002\\r\\u0001\\u0006\\u0005\\u0006{\\u0001!\\tAP\\u0001\\f[\\u0006$8\\r[(qi&|g\\u000E\\u0006\\u0002\\u0015!)q\\u0007\\u0010a\\u0001\\u0001B\\u0019q!\\u0011\\u000B\\n\\u0005\\tC!AB(qi&|g\\u000E"
)
class Basic extends java.lang.Object {
  def e1(x0: scala.Int): scala.Int = {
    """
      |digraph e1 {
      |}"""
    ???
  }
  def e2(x0: scala.Int, x1: scala.Int): scala.Int = {
    """
      |digraph e2 {
      |}"""
    ???
  }
  def e3(x0: scala.Int, x1: scala.Int, x2: scala.Int): scala.Int = {
    """
      |digraph e3 {
      |}"""
    ???
  }
  def fac(x0: scala.Int): scala.Int = {
    """
      |digraph fac {
      |   0 -> 3
      |   11 [label = <>]
      |   11 -> <error>
      |   0 [label = <0: var local_0 = a<BR />2: if(((local_0: Int) &lt;= (1: Int))) goto 11>]
      |   3 -> <error>
      |   0 -> 11
      |   <error> [label = <12: Return(Some(Int)) //Wanted one value in the stack, but it was empty at 12>]
      |   3 [label = <3: var local_1 = a<BR />4: var local_2 = var_0<BR />5: var local_3 = a<BR />8: var local_4 = (local_2: AnyRef).fac(((local_3: Int) - (1: Int)))<BR />10: goto 12>]
      |}"""
    ???
  }
  def whileBody(x0: scala.Int): scala.Int = {
    """
      |digraph whileBody {
      |   14 [label = <14: var local_7 = var_2<BR />15: return (local_7: Int)>]
      |   0 -> 2
      |   2 -> 5
      |   0 [label = <1: var var_2 = (0: Int)>]
      |   2 -> 14
      |   5 -> 2
      |   2 [label = <2: var local_0 = var_2<BR />3: var local_1 = max<BR />4: if(((local_0: Int) &gt;= (local_1: Int))) goto 14>]
      |   5 [label = <5: var local_2 = scala/Predef$.MODULE$<BR />6: var local_3 = var_2<BR />7: var local_4 = scala/runtime/BoxesRunTime.boxToInteger()<BR />8: var local_5 = (local_2: scala/Predef$).println((local_4: java/lang/Integer))<BR />9: var local_6 = var_2<BR />12: var var_2 = ((local_6: Int) + (1: Int))<BR />13: goto 2>]
      |}"""
    ???
  }
  def tailrecBody(x0: scala.Int): scala.Int = {
    """
      |digraph tailrecBody {
      |}"""
    ???
  }
  def matchInt1(x0: scala.Int): scala.Int = {
    """
      |digraph matchInt1 {
      |   4 [label = <5: goto 12>]
      |   0 -> 4
      |   0 -> 6
      |   4 -> <error>
      |   10 [label = <11: goto 12>]
      |   10 -> <error>
      |   8 -> <error>
      |   6 -> <error>
      |   6 [label = <7: goto 12>]
      |   0 -> 10
      |   0 [label = <0: var local_0 = i<BR />1: var var_2 = (local_0: Int)<BR />2: var local_1 = var_2<BR />3: switch((local_1: Int)) {<BR /> : case 0: goto 4<BR /> : case 1: goto 6<BR /> : case 2: goto 10<BR /> : case 3: goto 8<BR /> : default: goto 10<BR /> : }>]
      |   8 [label = <9: goto 12>]
      |   <error> [label = <12: Return(Some(Int)) //Wanted one value in the stack, but it was empty at 12>]
      |   0 -> 8
      |}"""
    ???
  }
  def matchInt2(x0: scala.Int): scala.Int = {
    """
      |digraph matchInt2 {
      |   0 -> 4
      |   12 [label = <13: goto 14>]
      |   4 [label = <5: goto 14>]
      |   0 -> 6
      |   4 -> <error>
      |   12 -> <error>
      |   10 [label = <11: goto 14>]
      |   10 -> <error>
      |   8 -> <error>
      |   6 -> <error>
      |   0 -> 10
      |   <error> [label = <14: Return(Some(Int)) //Wanted one value in the stack, but it was empty at 14>]
      |   6 [label = <7: goto 14>]
      |   0 [label = <0: var local_0 = i<BR />1: var var_2 = (local_0: Int)<BR />2: var local_1 = var_2<BR />3: switch((local_1: Int)) {<BR /> : case 0: goto 4<BR /> : case 1: goto 6<BR /> : case 3: goto 8<BR /> : case 1000: goto 10<BR /> : default: goto 12<BR /> : }>]
      |   8 [label = <9: goto 14>]
      |   0 -> 12
      |   0 -> 8
      |}"""
    ???
  }
  def matchOption(x0: scala.Option[java.lang.Object]): scala.Int = {
    """
      |digraph matchOption {
      |   16 -> 20
      |   29 [label = <29: var local_13 = var_2<BR />30: return (local_13: Int)>]
      |   20 -> 29
      |   5 [label = <5: var local_2 = var_3<BR />6: var local_3 = (local_2: AnyRef).asInstanceOf[scala/Some]<BR />7: var var_4 = (local_3: scala/Some)<BR />8: var local_4 = var_4<BR />9: var local_5 = (local_4: AnyRef).value()<BR />10: var local_6 = scala/runtime/BoxesRunTime.unboxToInt()<BR />11: var var_5 = (local_6: Int)<BR />12: var local_7 = var_5<BR />13: var var_2 = (local_7: Int)<BR />14: goto 29>]
      |   0 [label = <0: var local_0 = i //Stack before:   Stack after: GetFakeLocal(0,AnyRef)<BR />1: var var_3 = (local_0: AnyRef) //Stack before: GetFakeLocal(0,AnyRef)  Stack after: <BR />2: var local_1 = var_3 //Stack before:   Stack after: GetFakeLocal(1,AnyRef)<BR />4: IntIfZero(EQ,15) //Wanted Int in stack, but found Boolean instead at 4>]
      |   16 -> 23
      |   20 [label = <21: var var_2 = (0: Int)<BR />22: goto 29>]
      |   23 [label = <23: goto 24>]
      |   15 -> 16
      |   5 -> 29
      |   24 [label = <24: classOf[scala/MatchError]<BR />26: var local_11 = var_3<BR />27: var local_12 = new scala/MatchError((local_11: AnyRef))<BR />28: throw (local_12: scala/MatchError)>]
      |   0 -> 5
      |   23 -> 24
      |   0 -> 15
      |   15 [label = <15: goto 16>]
      |   16 [label = <16: var local_8 = scala/None$.MODULE$ //Stack before:   Stack after: GetFakeLocal(8,Ref(ClassInfo(scala/None$)))<BR />17: var local_9 = var_3 //Stack before: GetFakeLocal(8,Ref(ClassInfo(scala/None$)))  Stack after: GetFakeLocal(9,AnyRef), GetFakeLocal(8,Ref(ClassInfo(scala/None$)))<BR />18: var local_10 = (local_8: scala/None$).equals((local_9: AnyRef)) //Stack before: GetFakeLocal(9,AnyRef), GetFakeLocal(8,Ref(ClassInfo(scala/None$)))  Stack after: GetFakeLocal(10,Boolean)<BR />19: IntIfZero(EQ,23) //Wanted Int in stack, but found Boolean instead at 19>]
      |}"""
    ???
  }
  final private def inner$1(x0: scala.Int, x1: scala.Int): scala.Int = {
    """
      |digraph "inner$1" {
      |   0 -> 3
      |   12 [label = <12: var local_7 = i<BR />13: return (local_7: Int)>]
      |   3 -> 0
      |   0 [label = <0: var local_0 = i<BR />1: var local_1 = max$1<BR />2: if(((local_0: Int) &gt;= (local_1: Int))) goto 12>]
      |   3 [label = <3: var local_2 = scala/Predef$.MODULE$<BR />4: var local_3 = i<BR />5: var local_4 = scala/runtime/BoxesRunTime.boxToInteger()<BR />6: var local_5 = (local_2: scala/Predef$).println((local_4: java/lang/Integer))<BR />7: var local_6 = i<BR />10: var i = ((local_6: Int) + (1: Int))<BR />11: goto 0>]
      |   0 -> 12
      |}"""
    ???
  }
  def `<init>`(): scala.Unit = {
    """
      |digraph <init> {
      |}"""
    ???
  }
}
