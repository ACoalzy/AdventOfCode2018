package util

object Timer {
  def time[A](f: => A): A = {
    val start = System.currentTimeMillis()
    val result = f
    println(System.currentTimeMillis() - start)
    result
  }
}
