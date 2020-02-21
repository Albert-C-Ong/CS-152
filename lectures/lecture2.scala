
/* Written by Albert Ong
 * 
 * CS 152 Spring 2020
 * Professor Jon Pearce
 * 
 * Revision: 2020.02.18
 */

object lecture2 extends App {

  def maxAmp1(score: List[Note]) = {
    
    var result = 0.0
    
  }

  def maxAmp2(score: List[Note]): Double = {
    if (score == Nil) 0.0
    else if (0 < score.head.duration) score.head.duration + duration(score)
    else duration(score.tail)
  } 


  def maxAmp3(score: List[Note]) = {
    
    def helper(result: Double, unseen: List[Note]): Double = {
      if (unseen == Nil) result
      else if (0 < unseen.head.duration)
    }
  }

}

