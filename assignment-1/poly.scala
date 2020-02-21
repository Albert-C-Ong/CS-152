
object poly {
  
  def roots(p: (Double, Double, Double)): Option[(Double, Double)] = {
    // = None if p has no real roots
    // = Some((r1, r2)) where p(r1) == p(r2) == 0
    
    var x = math.sqrt(p._2 * p._2 - (4 * p._1 * p._3))
    var root1 = ((-1 * p._2) + x) / (2 * p._1)
    var root2 = ((-1 * p._2) - x) / (2 * p._1)
    
    None
  }
  
  def deriv(p: (Double, Double, Double)): (Double, Double, Double) = 
    // = derivative of p (which should be degree 1
    (0, 0, 0)
    
  def eval(a: Double, p: (Double, Double, Double)): Double = 
    // = p(a)
    (p._1 * a * a) + (p._2 * a) + p._3
}
