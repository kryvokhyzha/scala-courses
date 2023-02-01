package calculator

object Polynomial extends PolynomialInterface:
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] =
    Signal(b() * b() - 4 * a() * c())


  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
    Signal {
      var result = Set[Double]()

      if computeDelta(a, b, c)() > 0 then
        result += (-b() + math.sqrt(computeDelta(a, b, c)())) / (2 * a())
        result += (-b() - math.sqrt(computeDelta(a, b, c)())) / (2 * a())
      else if computeDelta(a, b, c)() == 0 then
        result += (-b()) / (2 * a())

      result
    }
