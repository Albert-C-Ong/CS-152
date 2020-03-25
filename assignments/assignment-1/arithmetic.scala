
object arithmetic {
  
  def sqrt(n: Int): Option[Int] = {
     // = None if n < 0
     // = largest int m such that m * m <= n
     if (n > 0) {
       
       var output = 0
       
       for (x <- 1 to n) {
         if (x * x <= n) {
           output = x
         }
       }
       
       Some(output)
     }
     else {
       None
     }
  }
  
  def log(n: Int): Option[Int] = {
     // = None if N <= 0
     // = largest m such that 2^m <= n
     
     if (n > 0)
       Some((math.log10(n) / math.log10(2)).toInt)
    else
      None
  }
  
  def isPrime(n: Int): Option[Boolean] = {
    // = true if n has no divisors > 1
    Some((2 to math.sqrt(n).toInt) forall (x => n % x != 0))
  }
  
  def gcd(n: Int, m: Int): Option[Int] = {
    // = None if n or m < 0
    // = gcd(m, n) if n < m
    // = largest k dividing both n and m
    if (n >= 0 && m >= 0) {
      
      var output = 0
      
      for (x <- 1 to math.max(n, m)) {
         
         if (n % x == 0 && m % x == 0) {
           output = x
         }
      }
      
      Some(output)
    }
    else {
      None
    }
  }
  
  def lcm(n: Int, m: Int): Option[Int] = {
    // = None if n < 0 or m < 0
    // = smallest k such that n a,d m divide k
    
    if (n < 0 || m < 0) {
      None
    }
    else {
      Some(0)
    }
  }
  
  def phi(n: Int): Option[Int] = {
    // = None if n < 0
    // = # of k <= n such that gcd(k, n) = 1
    Some(0)
  }
}
