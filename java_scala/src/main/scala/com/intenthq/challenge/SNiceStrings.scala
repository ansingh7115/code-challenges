package com.intenthq.challenge

import scala.annotation.tailrec

object SNiceStrings {

// From http://adventofcode.com/day/5
//  --- Day 5: Doesn't He Have Intern-Elves For This? ---
//
//  Santa needs help figuring out which strings in his text file are naughty or nice.
//
//    A nice string is one with all of the following properties:
//
//    It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
//  It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
//    It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements.
//    For example:
//
//    ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...), a double letter (...dd...), and none of the disallowed substrings.
//  aaa is nice because it has at least three vowels and a double letter, even though the letters used by different rules overlap.
//    jchzalrnumimnmhp is naughty because it has no double letter.
//    haegwjzuvuyypxyu is naughty because it contains the string xy.
//    dvszwmarrgswjxmb is naughty because it contains only one vowel.
//    How many strings are nice?



  def nice(xs: List[String]): Int = {
    xs.count(str => containsThreeVowels(str) && containsDoubleLetter(str) && !containsBannedStrings(str))
  }

  def containsThreeVowels(str: String): Boolean = {
    val vowels = List('a', 'e', 'i', 'o', 'u')
    // prioritizing simplicity over efficiency - this solution counts all the vowels instead of returning
    // when it encounters three. Could write a tail recursive solution instead.
    str.count(vowels.contains) >= 3
  }

  def containsDoubleLetter(str: String): Boolean = {
    val strLastIndex: Int = str.length - 1

    @tailrec
    def loop(index: Int): Boolean = {
      if (strLastIndex == index) false
      else if (str.charAt(index) == str.charAt(index + 1)) true
      else loop(index + 1)
    }
    loop(0)
  }

  def containsBannedStrings(str: String): Boolean = {
    val banned = List("ab", "cd", "pq", "xy")
    banned.foldLeft(false)((a, b) => a || str.contains(b) )
  }

}
