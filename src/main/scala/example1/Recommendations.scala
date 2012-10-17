package example1

import collection.mutable.ListBuffer
import math._

/**
 * The Class Recommendations.
 *
 * @author Nguyen Duc Dung
 * @since 10/17/12 3:35 PM
 *
 */
object Recommendations {

  /**
   * Returns the Pearson correlation coefficient for p1 and p2
   * @param prefs
   * @param person1
   * @param person2
   * @return
   */
  def sim_pearson(prefs: Map[String, Map[String, Double]], person1: String, person2: String): Double = {

    val si = new ListBuffer[String]

    prefs(person1).foreach(pref => {
      if (!prefs(person2).get(pref._1).isEmpty) si += pref._1
    })

    if (si.size == 0) return 0

    val n = si.size

    //Sums of all the preferences
    var sum1 = 0.0
    si.foreach(item => {
      sum1 += prefs(person1)(item)
    })

    var sum2 = 0.0
    si.foreach(item => {
      sum2 += prefs(person2)(item)
    })

    //Sums of the squares
    var sum1Sq = 0.0
    si.foreach(item => {
      sum1Sq += pow(prefs(person1)(item), 2)
    })

    var sum2Sq = 0.0
    si.foreach(item => {
      sum2Sq += pow(prefs(person2)(item), 2)
    })

    //Sum of the products
    var pSum = 0.0
    si.foreach(item => {
      pSum += prefs(person1)(item) * prefs(person2)(item)
    })

    //Calculate r (Pearson score)
    val num = pSum - (sum1 * sum2 / n)
    val den = sqrt((sum1Sq - pow(sum1, 2) / n) * (sum2Sq - pow(sum2, 2) / n))
    if (den == 0) return 0

    num / den
  }

}
