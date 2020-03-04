// package time2

class Time(private var hours_: Int, private minutes: Int = 0) {
  
  def hours = hours_
  def hours_(new newHours): Unit = {
    if (newHours < 0) throw new Exception("Hours must be >= 0")
    if (newHours >= 24) throw new Exception("Hours must be < 24")
    hours_ = hours
  }
  
  //~ if (hours < 0) throw new Exception("Hours must be non-negative")
  //~ if (minutes < 0) throw new Exception("Minutes muyst be non-negative")
  //~ if (hours > 23) throw new Exception("Hours must be less 23")
  //~ if (minutes > 59) throw new Exception("Minutes must be less than 60")

  //~ def minutesSinceMidNight = hours * 60 + minutes
  //~ def before(other: Time) = this.minutesSinceMidNight < other.minutesSinceMidNight
  //~ override def toString = hours + ": " + minutes

}

object Time {
  def apply(hours: Int, minutes: Int = 0) = new Time(hours, minutes)
}

object TestTime2 extends App {
  try {
    val t1 = Time(22, 5)
    val t2 = Time(20, 10)
    println("t1 = " + t1)
    println("t2 = " + t2)
    println("t1 < t2 = " + t1.before(t2))
    t1.hours = 20
    println("t1 < t2 = " + t1.before(t2))
    t1.minutes = 60
  } catch {
    case e: IllegalArgumentException => println(e)
  }
  
}
