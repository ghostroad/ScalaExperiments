package euler

import euler.Euler53._
import org.scalatest.{FreeSpec, Matchers}

class Euler53Spec extends FreeSpec with Matchers {

  "Computing the choose function" - {
    "should work for n = 23 and r = 10" in {
      chooseFactors(23, 10).product shouldBe 1144066
    }
  }

}
