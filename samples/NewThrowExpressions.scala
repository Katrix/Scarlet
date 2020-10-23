object NewThrowExpressions {

  def sayHello(): Unit = {
    println("Hello")
  }
  
  def sayWorld(): Unit = {
    println("World")
  }
  
  def boom(): Unit = {
    println("Boom")
  }

  def hello(): Int = {
    sayHello()
	4
  }
  
  def world(): Int = {
    sayWorld()
	5
  }
  
  def baz(): Int = throw new FooException
  
  def tryNew() = try {
    new A(hello(), world(), baz()).result
  } catch {
    case e: FooException => 0
  }
  
  def tryExpressionNew() = new A(hello(), world(), try {baz()} catch { case e: FooException => 0 }).result
  
  def newExpression() = try {
    new A(hello(), world(), {sayHello(); sayHello(); world()}).result
  } catch {
    case e: NoClassDefFoundError => 
	  boom()
	  throw e
  }
  
  class FooException extends Exception

  class A(foo: Int, bar: Int, baz: Int) {
    def result: Int = foo + bar + baz
  }
}