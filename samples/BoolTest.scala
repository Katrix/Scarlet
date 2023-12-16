class BoolTest {

  def returnsTrue: Boolean = true

  def returnsFalse: Boolean = false

  def normal: Boolean  = returnsFalse && returnsTrue
  def bitwise: Boolean = returnsFalse & returnsTrue
}
