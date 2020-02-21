
object vector {
  
  def sum(v1: (Double, Double, Double), v2: (Double, Double, Double)): (Double, Double, Double) = 
    // v1 + v2
    (v1._1 + v2._1, v1._2 + v2._2, v1._3 + v2._3)
    
  def mul(a: Double,v: (Double, Double, Double)): (Double, Double, Double) = 
    // = a * v
    (a * v._1, a * v._2, a * v._3)
    
  def dot(v1: (Double, Double, Double), v2: (Double, Double, Double)): Double = 
    // = v1 * v2
    v1._1 * v2._1 + v1._2 * v2._2 + v1._3 * v2._3
  
  def length(v: (Double, Double, Double)): Double = 
    // = |v|
    math.sqrt(v._1 * v._1+ v._2 * v._2+ v._3 * v._3)
  
  def theta(v1: (Double, Double, Double), v2: (Double, Double, Double)): Double =
    // = angle (in radians) between v1 and v2
    math.acos(dot(v1, v2) / (length(v1) * length(v2)))
}
