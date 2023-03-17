object Sequences {
  def findPeriod(s: Vector[Int]): Vector[Int] = {

    def go(i: Int, p: Vector[Int]): Boolean = {
      if(i == s.length) true
      else if((s(i) == p(i % p.length)) & (i < s.length) ) go(i + 1, p)
      else false
    }

    def check(p: Vector[Int]): Vector[Int] = {
      if(go(p.length, p)) p
      else {
        val nextPeriod = p :+ s(p.length)
        if (nextPeriod.length > s.length / 2) Vector.empty
        else check(nextPeriod)
      }
    }

    check(Vector(s(0))) match {
      case Vector() => Vector.empty
      case period => period
    }
  }


  def main(args: Array[String]): Unit = {
    println(findPeriod(Vector(2,1,1,2,1,1,2,1)))
  }

}
