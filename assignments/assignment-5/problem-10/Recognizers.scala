
/* Written by Albert Ong
 * 
 * CS 152 Spring 2020
 * Professor Jon Pearce
 * 
 * Assignment #5: Problem #10 - Recognizers
 * 
 * Revision: 2020.03.05
 */


trait Recognizers {

  // matches(s) = s
  def matches(s: String): String => Boolean = {
    def r(s1: String) = s1.trim.equalsIgnoreCase(s)
    r _
  }

  // opt(r) = r?
  def opt(r: String => Boolean): String => Boolean = {
    def r1(s: String) =  r(s) || s == ""
    r1 _
  }

  // pipe(r1, r2) = r1 | r2
  def pipe(r1: String => Boolean, r2: String => Boolean) : String => Boolean = {
    def r(s: String) = r1(s) || r2(s)
    r _
  }

  // follows(r1, r2) = r1 ~ r2
  def follows(r1: String => Boolean, r2: String => Boolean) : String => Boolean = {
    
    def r(s: String) = {
      
      var found = false
      
      for (i <- 0 to s.length if (!found))
        found = r1(s.take(i)) && r2(s.drop(i))
        
      found
    }
    r _
  }

  // rep(r) = r*
  def rep(r: String => Boolean): String => Boolean = {
    
  //r1 uses recursion and iteration!
    def r1(s: String): Boolean = {
      var result = false
      if (s == "") result = true
      else {
        for(i <- 0 to s.length if !result)
          result = r(s.substring(0, i)) && r1(s.substring(i))
      }
      result
    }
    
    r1 _
  }
}

