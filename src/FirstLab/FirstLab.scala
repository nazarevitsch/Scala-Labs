package FirstLab

object FirstLab {

  def countChange(money: Int, coins: List[Int]): Int = {
    var f: Int = 0;
    def count(money: Int, coins: List[Int]): Unit = {
      if (coins.isEmpty) {
        return
      } else {
        if (money - coins.head <= 0) {
          if (money - coins.head == 0) {
            f += 1
            return
          } else {
            return
          }
        } else {
          count(money - coins.head, coins)
          count(money, coins.tail)
        }
      }
    }
    count(money, coins)
    f
  }

  def balance(chars: List[Char]): Boolean = {
    def balance2(chars: List[Char], open: Int, close: Int) : Boolean = {
      if (chars.size == 0) {
        if(open == close) true else false
      } else {
        if (chars.head == '(' || chars.head == ')') {
          if (chars.head == '(') {
            balance2(chars.tail, open + 1, close)
          } else {
            if (close + 1 > open) false else balance2(chars.tail, open, close + 1)
          }
        } else
          balance2(chars.tail, open, close)
      }
    }
    balance2(chars, 0, 0)
  }

  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r)
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  def exponent(b : Int, n : Int) : Int = {
    if (b == 0) 1
    else {
      if (n == 0) 1 else b * exponent(b, n - 1)
    }
  }
}
