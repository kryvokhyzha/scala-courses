package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if c == 0 || c == r then 1 else pascal(c , r - 1) + pascal(c - 1 , r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    def balanceIter(chars: List[Char], counter: Int): Int =
      if chars.isEmpty || counter < 0 then
        counter
      else
        balanceIter(
          chars.tail, chars.head match
            case '(' => counter + 1
            case ')' => counter - 1
            case _ => counter
        )

    balanceIter(chars, 0) == 0

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    def countChangeIter(money: Int, coins: List[Int]): Int =
      if money < 0 || coins.isEmpty then 0
      else if money == 0 then 1
      else countChangeIter(money, coins.tail) + countChangeIter(money - coins.head, coins)
    countChangeIter(money, coins)
