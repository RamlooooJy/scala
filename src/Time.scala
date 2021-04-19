import java.util.Calendar

class Time
(
  private var hours: Int,
  private var minutes: Int
){

  def current () = {
    println(Calendar.getInstance().getTime().getHours() * 60 + Calendar.getInstance().getTime().getMinutes() + " минут")
  }

  def before (other: Time): Boolean = {
    if( this.hours >= other.hours && this.minutes >= other.minutes ){
      println("Позже или равно")
      false
    }
    else {
      println("Раньше")
      true
    }
  }
}

object Main extends App {

  var t1 = new Time(14,2)
  t1.current()
  var t2 = new Time(16,2)
  println(t1 before t2)
}
