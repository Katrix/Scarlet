class Basic {

  def e1(a: Int): Int = a + 1

  def e2(a: Int, b: Int): Int = a + b

  def e3(a: Int, b: Int, c: Int): Int = e2(a, b) + e2(a, c) + e2(b, c)

  def fac(a: Int): Int = if(a > 1) a * fac(a - 1) else 1
}