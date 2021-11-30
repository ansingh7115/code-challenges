package com.intenthq.challenge

import org.specs2.mutable.Specification

class SNiceStringsSpec extends Specification {

  section("scala")
  section("nice")
  "SNiceStrings" should {
    "ugknbfddgicrmopn is nice" in {
      SNiceStrings.nice(List("ugknbfddgicrmopn")) must_== 1
    }
    "aaa is nice" in {
      SNiceStrings.nice(List("aaa")) must_== 1
    }
    "jchzalrnumimnmhp is naughty" in {
      SNiceStrings.nice(List("jchzalrnumimnmhp")) must_== 0
    }
    "haegwjzuvuyypxyu is naughty" in {
      SNiceStrings.nice(List("haegwjzuvuyypxyu")) must_== 0
    }
    "dvszwmarrgswjxmb is naughty" in {
      SNiceStrings.nice(List("dvszwmarrgswjxmb")) must_== 0
    }
    "' ' is naughty" in {
      SNiceStrings.nice(List(" ")) must_== 0
    }
    "aaa is nice and jchzalrnumimnmhp is naughty" in {
      SNiceStrings.nice(List("aaa", "jchzalrnumimnmhp")) must_== 1
    }
    "spqeggio and esaw are naughty" in {
      SNiceStrings.nice(List("spqeggio", "esaw")) must_== 0
    }
  }
  section("nice")
  section("scala")
}
