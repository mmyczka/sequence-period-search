object PeriodicSequence {
  implicit class PeriodicVector[A](vector: Vector[A]) {
    def getPeriodicElement(idx: Int): A = vector(idx % vector.length)
  }
}
