class Basic {

  def e1(a: Int): Int = a + 1

  def e2(a: Int, b: Int): Int = a + b

  def e3(a: Int, b: Int, c: Int): Int = e2(a, b) + e2(a, c) + e2(b, c)

  def fac(a: Int): Int = if (a > 1) a * fac(a - 1) else 1

  def whileBody(max: Int): Int = {
    var i = 0
    while (i < max) {
      println(i)
      i = i + 1
    }

    i
  }

  def tailrecBody(max: Int): Int = {
    @scala.annotation.tailrec
    def inner(i: Int): Int =
      if (i < max) {
        println(i)
        inner(i + 1)
      } else i

    inner(0)
  }

  def matchInt1(i: Int): Int = i match {
    case 0 => 5
    case 1 => 10
    case 3 => 15
    case _ => 0
  }

  def matchInt2(i: Int): Int = i match {
    case 0    => 5
    case 1    => 10
    case 3    => 15
    case 1000 => 20
    case _    => 0
  }

  def matchOption(i: Option[Int]): Int = i match {
    case Some(value) => value
    case None        => 0
  }
}
