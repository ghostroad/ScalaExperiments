package euler

import euler.Utils.divides

object Euler19 extends App {
  class Date(val day: Int, val month: Int, val year: Int) {
    private val daysInMonthNonLeapYears = Array(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    override def toString: String = month + "/" + day + "/" + year

    def isLeapYear(): Boolean = {
       divides(year, 400) | (divides(year, 4) & !divides(year, 100))
    }

    def daysInMonth(): Int = month match {
      case 2 => if (isLeapYear()) 29 else 28
      case _ => daysInMonthNonLeapYears(month - 1)
    }

    def next(): Date = {
      if (day < daysInMonth()) new Date(day + 1, month, year) else {
        if (month < 12) new Date(1, month + 1, year) else new Date(1, 1, year + 1)
      }
    }

    override def equals(other: scala.Any): Boolean = other match {
      case other: Date => (other.day == day) & (other.month == month) & (other.year == year)
      case _ => false
    }

  }

  val start = new Date(1, 1, 1901) //Tuesday
  println(Stream.iterate(start)(_.next).takeWhile(_ != new Date(1, 1, 2001)).zipWithIndex.count({ case (date, i) => date.day == 1 & (i % 7 == 5)}))
}
