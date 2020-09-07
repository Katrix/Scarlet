class SwitchTest {

  def table(n: Int): Int = n match {
    case 0 => 1
    case 1 => 2
    case 2 => 3
    case 3 => 4
  }

  def lookup(n: Int): Int = n match {
    case 1    => 1
    case 10   => 2
    case 100  => 3
    case 1000 => 4
  }

  def matchInt(n: Int, b1: Boolean, b2: Boolean): Int = {
    lazy val containTest = (0 to 10).contains(n)
    n match {
      case m if m < 0                       => 1
      case 0                                => 2
      case m if (0 to 10).contains(m) && b1 => 3
      case m if b2 && (0 to 10).contains(m) => 4
      case m if (0 to 10).contains(m)       => 5
      case m if m > 10 && containTest       => 6
      case m if containTest                 => 7
      case _                                => 8
    }
  }

  def matchOptInt(n: Option[Int]) = n match {
    case Some(m) if m < 0                 => 1
    case Some(0)                          => 2
    case Some(m) if (0 to 10).contains(m) => 3
    case Some(m)                          => 4
    case None                             => 5
  }
}
