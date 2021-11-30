package com.intenthq.challenge

import org.specs2.mutable.Specification

class SEnigmaSpec extends Specification {

  section("scala")
  section("enigma")
  "SEnigmaSpec" should {
    val map = Map(23 -> 'N', 234 -> ' ', 89 -> 'H', 78 -> 'Q', 37 -> 'A')
    val deciphe = SEnigma.deciphe(map) _

    "(2,3) is 'N'" in {
      deciphe(List(2,3)) must_== "N"
    }
    "(2,3,8,9) is 'NH'" in {
      deciphe(List(2,3,8,9)) must_== "NH"
    }

    "(1,2,3) is '1N'" in {
      deciphe(List(1,2,3)) must_== "1N"
    }

    "(2,3,4) is ' '" in {
      deciphe(List(2,3,4)) must_== " "
    }

    "(1,2,3,7,3,2,3,7,2,3,4,8,9,7,8) is '1N73N7 HQ'" in {
      deciphe(List(1,2,3,7,3,2,3,7,2,3,4,8,9,7,8)) must_== "1N73N7 HQ"
    }

    val overlapMap = Map(25415 -> 'H', 225 -> 'I')
    "(2,2,5,4,1,5) is 'I415'" in {
      SEnigma.deciphe(overlapMap)(List(2,2,5,4,1,5)) must_== "I415"
    }

    val overlapMap2 = Map(25415 -> 'H', 4153 -> 'L')
    "(2,5,4,1,5,3) is 'H'" in {
      SEnigma.deciphe(overlapMap2)(List(2,5,4,1,5,3)) must_== "H3"
    }
  }
  section("enigma")
  section("scala")
}
