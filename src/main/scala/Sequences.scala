import PeriodicSequence._
object Sequences {
  def findPeriod[A](s: Vector[A]): Vector[A] = {

    def go[A](i: Int, p: Vector[A]): Boolean = {
      if(i == s.length) true
      else if((s(i) == p.getPeriodicElement(i)) & (i < s.length) ) go(i + 1, p)
      else false
    }

    def check[A](p: Vector[A], s: Vector[A]): Vector[A] = {
      if(go(p.length, p)) p
      else {
        val nextPeriod = p :+ s(p.length)
        if (nextPeriod.length > s.length / 2) Vector.empty
        else check(nextPeriod, s)
      }
    }

    check(Vector(s(0)), s) match {
      case Vector() => Vector.empty
      case period => period
    }
  }


  def main(args: Array[String]): Unit = {
    println(findPeriod(Vector('a', 2, "car", 'a', 2, "car", 'a')))
  }

}
