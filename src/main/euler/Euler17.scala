package euler

object Euler17 extends App {

  def toWords(n: Int):String = {
    if (n < 20) n match {

      case 0 => ""
      case 1 => "one"
      case 2 => "two"
      case 3 => "three"
      case 4 => "four"
      case 5 => "five"
      case 6 => "six"
      case 7 => "seven"
      case 8 => "eight"
      case 9 => "nine"
      case 10 => "ten"
      case 11 => "eleven"
      case 12 => "twelve"
      case 13 => "thirteen"
      case 14 => "fourteen"
      case 15 => "fifteen"
      case 16 => "sixteen"
      case 17 => "seventeen"
      case 18 => "eighteen"
      case 19 => "nineteen"
    } else if (n < 100) {
      val units = n % 10
      val tens = (n - units) /10
      val tensString = tens match {
        case 2 => "twenty"
        case 3 => "thirty"
        case 4 => "forty"
        case 5 => "fifty"
        case 6 => "sixty"
        case 7 => "seventy"
        case 8 => "eighty"
        case 9 => "ninety"
      }

      tensString + toWords(units)
    } else if (n < 1000) {
      val rem = n % 100
      val hundreds = (n - rem)/100
      val hundredsstring = toWords(hundreds) + "hundred"
      if (rem == 0) hundredsstring else hundredsstring + "and" + toWords(rem)
    } else {
      "onethousand"
    }


  }

  println(toWords(20))
  println((1 to 1000).map(toWords).foldLeft(0)(_ + _.length))

}
