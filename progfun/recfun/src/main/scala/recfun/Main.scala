package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    // println(balance("()Example(()".toList))
    // println(countChange(4,List(1,2,3)))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    def loops(c: Int, r: Int): Int = {
      if ((c==0) || (c==r))
        1
      else
        loops(c-1, r-1) + loops(c, r-1)
    }

    loops(c, r)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def isBegin(_char: Char): Boolean = _char == '('
    def isEnd(_char: Char): Boolean = _char == ')'

    def check(_count: Int, _char: Char): Int = {
      if (isBegin(_char))
        _count + 1
      else if (isEnd(_char) && (_count <= 0))
         _count - 2
      else if (isEnd(_char))
        _count -1
      else
        _count
    }

    def loops(_count: Int, _chars: List[Char]): Boolean = {
      if (_chars.isEmpty)
        (_count == 0)
      else
        loops(check(_count, _chars.head),_chars.tail)
    }

    loops(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def loops(_currentMoney: Int, _canUsedCoins: List[Int]): Int = {
      if (_canUsedCoins.isEmpty)
        0
      else {
        val h = _canUsedCoins.head
        val t = _canUsedCoins.tail

        val newMoney = _currentMoney + h

        if (newMoney > money)
          0
        else if (newMoney == money)
          1
        else
          loops(newMoney, _canUsedCoins) + loops(_currentMoney, t)
      }
    }

    loops(0, coins.sorted)
  }
}
