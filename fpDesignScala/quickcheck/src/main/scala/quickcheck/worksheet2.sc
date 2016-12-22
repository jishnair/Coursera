import org.scalacheck._
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._


lazy val genMap: Gen[Map[Int,Int]] = for {
  k <- arbitrary[Int]
  v <- arbitrary[Int]
  m <- oneOf(const(Map.empty[Int,Int]), genMap)
} yield m.updated(k, v)

genMap.sample