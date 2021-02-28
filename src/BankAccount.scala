class BankAccount
(
  private var _balance: Int
) {
  def deposit(addVal:Int) = {
    _balance += addVal
  }

  def withdraw(subVal:Int): Unit = {
    if(subVal > _balance) {
      println("no money")
      return
    }
    _balance -= subVal
  }

  def balance = _balance
}


//object Main extends App {
//  var b = new BankAccount(10)
//  b.deposit(100200)
//  b.withdraw(9999)
//  b.withdraw(999999)
//  println(b.balance)
//}