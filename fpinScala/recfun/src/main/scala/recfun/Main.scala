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
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)

  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def checkBalanced(open: Int, l: List[Char]): Boolean = l match {
      case Nil => open==0
      case x::xs if(x=='(') =>checkBalanced(open+1,xs)
      case x::xs if(x==')') => open >0 && checkBalanced(open-1,xs)
      case x::xs => checkBalanced(open,xs)
    }
    checkBalanced(0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0)
      1
    else if(money > 0 && !coins.isEmpty)
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    else
      0
  }
}
