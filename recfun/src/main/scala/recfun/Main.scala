package recfun
import common._
import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    assert(r >= 0 && c >= 0 && c <= r)
    if (r == 0 || c == 0 || c == r) {
      1
    } else {
      pascal(c-1, r-1) + pascal(c, r-1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def balanceWithCount(c: Int, chars: List[Char]): Boolean = {
      if (c < 0) {
        false
      } else {
        chars match {
          case List() => c == 0
          case head :: tail =>
            if (head == '(') balanceWithCount(c+1, tail)
            else if (head == ')') balanceWithCount(c-1, tail)
            else balanceWithCount(c, tail)
        }
      }
    }
    balanceWithCount(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0) 0
    else if (money == 0) 1
    else coins match {
      case List() => 0
      case head :: tail => countChange(money-head, coins) + countChange(money, tail)
    }
  }
}
