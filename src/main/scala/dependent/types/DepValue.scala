package dependent.types

trait DepValue {
  type V
  val value : V
}

object DepValue{
  def magic(that: DepValue): that.V = that.value

  def mk[T] (x: T) = new DepValue {
    override type V = T
    override val value = x
  }

  def main(args : Array[String]): Unit = {
    val a = mk(1)
    println(magic(a))
  }
}
