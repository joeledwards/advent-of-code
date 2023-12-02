package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.concurrent.{ExecutionContext, Future}

object day1 extends AdventDay(1) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"${digitSums}"
  }
  
  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"${lineSums}"
  }

  def digitSums: Long = digitValues.sum

  def digitValues: List[Int] = {
    lines
      .map({ line =>
        val first = firstDigit(line).map(_._1).getOrElse(0)
        val last = lastDigit(line).map(_._1).getOrElse(0)

        first * 10 + last
      })
  }

  def lineSums: Long = {
    lines
      .map({ line =>
        val value = lineToValue(line)
        //println(s"$line => $value")
        value
      })
      .sum
  }

  def lineToValue(line: String): Int = {
    // Lowest index value is the first
    val first = (firstDigit(line).toList ::: firstDigitWord(line).toList).minBy(_._2)

    // Highest index value is the last
    val last = (lastDigit(line).toList ::: lastDigitWord(line).toList).maxBy(_._2)

    first._1 * 10 + last._1
  }

  def nameToValueMap: Map[String, Int] = Map(
    "zero"  -> 0,
    "one"   -> 1,
    "two"   -> 2,
    "three" -> 3,
    "four"  -> 4,
    "five"  -> 5,
    "six"   -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine"  -> 9,
  )

  /**
   * Return all prefixes of a word.
   *
   * If the word is "advent", this will return
   * Set( "a", "ad", "adv", "adve", "adven", "advent" )
   *
   * @return all prefixes of a word
   */
  def prefixes(text: String): Set[String] = {
    text.foldLeft[(List[String], String)](Nil, "")({ (state, nextChar) =>
      val (prefixes, prefix) = state
      val nextPrefix = s"$prefix$nextChar"
      (nextPrefix :: prefixes, nextPrefix)
    })._1.toSet
  }

  /**
   * Return all suffixes of a word.
   *
   * If the word is "advent", this will return
   * Set( "t", "nt", "ent", "vent", "dvent", "advent" )
   *
   * @return all suffixes of a word
   */
  def suffixes(text: String): Set[String] = {
    text.reverse.foldLeft[(List[String], String)](Nil, "")({ (state, nextChar) =>
      val (suffixes, suffix) = state
      val nextSuffix = s"$nextChar$suffix"
      (nextSuffix :: suffixes, nextSuffix)
    })._1.toSet
  }

  val namePrefixes: Set[String] = nameToValueMap.keySet.toList.flatMap(prefixes).toSet
  val nameSuffixes: Set[String] = nameToValueMap.keySet.toList.flatMap(suffixes).toSet

  def firstDigitWord(text: String): Option[(Int, Int)] = {
    var prefix: String = ""
    var prefixOffset: Option[Int] = None

    for (i <- 0 until text.length) {
      val nextChar = text.charAt(i)

      if (nextChar.isDigit) {
        // If we encountered a digit, we have not successfully consumed a valid
        // word digit, so we just return this as the first encountered value.
        return Some((nextChar.getNumericValue, i))
      } else {
        prefix = s"$prefix$nextChar"
        var tryAnother = true
        while (tryAnother) {
          val wordValue = nameToValueMap.get(prefix)
          if (wordValue.nonEmpty) {
            // If the updated prefix is a digit word, return its value
            return Some((wordValue.getOrElse(0), prefixOffset.getOrElse(i)))
          } else if (namePrefixes.contains(prefix)) {
            // If the updated prefix is a valid digit word prefix, move on to the next character
            prefixOffset = prefixOffset.orElse(Some(i))
            tryAnother = false
          } else {
            prefixOffset = None
            prefix = prefix.substring(1)
            if (prefix.isEmpty) {
              tryAnother = false
            }
          }
        }
      }
    }

    // If we get to the end, there was no initial value
    None
  }

  def lastDigitWord(text: String): Option[(Int, Int)] = {
    var suffix: String = ""
    var suffixOffset: Option[Int] = None

    if (text.length < 0)
      return None

    // Traverse the string backward
    for (i <- Range.inclusive(text.length - 1, 0, -1)){
      val nextChar = text.charAt(i)

      if (nextChar.isDigit) {
        // If we encountered a digit, we have not successfully consumed a valid
        // word digit, so we just return this as the first encountered value.
        return Some((nextChar.getNumericValue, i))
      } else {
        suffix = s"$nextChar$suffix"
        var tryAnother = true
        while (tryAnother) {
          val wordValue = nameToValueMap.get(suffix)
          if (wordValue.nonEmpty) {
            // If the updated prefix is a digit word, return its value
            return Some((wordValue.getOrElse(0), i))
          } else if (nameSuffixes.contains(suffix)) {
            // If the updated suffix is a valid digit word suffix, move on to the next character
            suffixOffset = Some(i)
            tryAnother = false
          } else {
            suffixOffset = None
            suffix = suffix.substring(0, suffix.length - 1)
            if (suffix.isEmpty) {
              tryAnother = false
            }
          }
        }
      }
    }

    // If we get to the end, there was no initial value
    None
  }

  def firstDigit(text: String): Option[(Int, Int)] = {
    text
      .zipWithIndex
      .find(_._1.isDigit)
      .map { case (c, i) => (c.getNumericValue, i) }
  }

  def lastDigit(text: String): Option[(Int, Int)] = {
    firstDigit(text.reverse)
      .map({ case (v, i) =>
        (
          v,
          // Not just a simple inversion since we need the index to be corrected to
          // match the offset in the original text.
          text.length - 1 - i
        )
      })
  }
}
