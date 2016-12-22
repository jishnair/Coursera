package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = Signal {
    val bi = b()
    bi * bi - (4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal {
    // (-b ± √Δ) / (2a)

    val negB = -1 * b()
    val sqrtDel = math.sqrt(delta())
    val twoA = 2 * a()

    if (delta() < 0) Set()
    else {
      Set(
        ((negB + sqrtDel) / twoA),
        ((negB - sqrtDel) / twoA)
      )
    }
  }
}
