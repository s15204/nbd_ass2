class Functions {
  def dayToWeek (day: String): String = day match {
    case "Monday" | "Tuesday" | "Wednesday" | "Thursday" | "Friday" => "work"
    case "Saturday" | "Sunday" => "weekend"
    case _ => "no such day"
  }

  def multiply (x: Int): Int = {
    x * x
  }

  def apply (x: Int, function: Int => Int): Int = {
    var out = x
    for (i <- 0 to 2)
      out = function.apply(out)

    out
  }
}

class BankAccount(var initialBalance: Int = 0) {
  private var _currentBalance = initialBalance

  def currentBalance: Int = _currentBalance

  def deposit (money: Int): Unit ={
    if (money <= 0) println("Wrong amount")
    _currentBalance += money
    println(s"Deposited $money \nCurrent balance: ${_currentBalance}")
  }

  def withdraw (money: Int): Unit = {
    if (_currentBalance < money) println("Insufficient amount to withdraw")
    else if (money <= 0) println("Wrong amount")
    else _currentBalance -= money
    println(s"Withdrew $money\nCurrent balance: ${_currentBalance}")
  }
}

trait Student extends Person {
  override def taxToPay: Int = 0
}

trait Employee extends Person {
  private var _salary: Int = 0

  def salary: Int = _salary

  def set_salary(sal: Int): Unit = {
    _salary = sal
  }

  override def taxToPay: Int = (0.2 * _salary).toInt
}

trait Teacher extends Employee {
  override def taxToPay: Int = (0.1 * salary).toInt
}

class Person(val firstName: String, val lastName: String, val tax: Int) {
  private val _taxToPay = tax

  def taxToPay: Int = _taxToPay

  def greet(): String = (firstName, lastName) match {
    case ("Greg", _) => s"It's just $firstName" // we all know it's always Greg anyway
    case (_, "Smith") => s"It's ye 'ol $firstName $lastName"
    case _ => s"Hello $firstName $lastName"
  }
}

object Ass2 extends App {
  val functions: Functions = new Functions

  println(functions.dayToWeek("Monday"))
  println(functions.dayToWeek("Sunday"))
  println(functions.dayToWeek("Caturday"))
  // what do you mean no Caturday

  println()

  val myAccount: BankAccount = new BankAccount()
  myAccount.deposit(10000)
  myAccount.withdraw(3849)

  println()

  val people: List[Person] = List(new Person("Greg","Dork", 0), new Person("Sarah","Butt", 0), new Person("Tin","Smith", 0))
  people.foreach(person => println(person.greet()))

  println()

  println(5)
  println(functions.apply(5, functions.multiply))

  println()

  val studyee = new Person("Mr","Smort",0) with Student with Employee
  studyee.set_salary(1000)
  val emplent = new Person("Mr","Rich",0) with Employee with Student
  emplent.set_salary(6000)

  println(studyee.taxToPay)
  println(emplent.taxToPay)
}