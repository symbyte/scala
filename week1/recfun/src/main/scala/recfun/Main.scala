package recfun

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
    def pascal(c: Int, r: Int): Int =
      if(c < 0 || r < 0) 0
      else if (c == 0 && r == 0 ) 1
      else pascal(c -1, r -1) + pascal(c, r - 1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balanceIter(leftCount: Int, rightCount: Int, chars: List[Char]): Boolean =
        if (rightCount > leftCount) false
        else if (chars.isEmpty) rightCount == leftCount
        else if (chars.head == '(') balanceIter(leftCount + 1, rightCount, chars.tail)
        else if (chars.head == ')') balanceIter(leftCount, rightCount + 1, chars.tail)
        else balanceIter(leftCount, rightCount, chars.tail)
      balanceIter(0, 0, chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def currentCoinTooLarge(money: Int) = money < 0

      def weMadeChange(money: Int) = money == 0

      def makeChangeWithCurrentCoin(money: Int, coins: List[Int]) =
        countChangeIter(money - coins.head, coins)

      def makeChangeWithAllOtherCoins(money: Int, coins: List[Int]) =
        countChangeIter(money, coins.tail)

      def countChangeIter(money: Int, coins: List[Int]): Int =
        if (coins.isEmpty) 0
        else if (currentCoinTooLarge(money)) 0
        else if (weMadeChange(money)) 1
        else makeChangeWithCurrentCoin(money, coins) + makeChangeWithAllOtherCoins(money, coins)

      countChangeIter(money, coins)
    }
  }
