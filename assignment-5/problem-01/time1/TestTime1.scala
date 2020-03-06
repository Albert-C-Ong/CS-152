// package time1

class Time(val hours: Int, val minutes: Int = 0) {
  
  if (hours < 0) throw new Exception("Hours must be non-negative")
  if (minutes < 0) throw new Exception("Minutes muyst be non-negative")
  if (hours > 23) throw new Exception("Hours must be less 23")
  if (minutes > 59) throw new Exception("Minutes must be less than 60")

  def minutesSinceMidNight = hours * 60 + minutes
  def before(other: Time) = this.minutesSinceMidNight < other.minutesSinceMidNight
  override def toString = hours + ": " + minutes

}

object Time {
  def apply(hours: Int, minutes: Int = 0) = new Time(hours, minutes)
}

object TestTime1 extends App {
  
  println("Hello world!")
  
  try {
    val t1 = Time(3, 45)
    val t2 = Time(3)
    println("t1 = " + t1)
    println("t2 = " + t2)
    println("t1 < t2 = " + t1.before(t2))
    println("t1 minutes since midnight = " + t1.minutesSinceMidNight)
    val t3 = Time(14, 60) // oops!
  } catch {
    case e: IllegalArgumentException => println(e)
  }
  
}
