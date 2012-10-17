package example1

import collection.mutable.ListBuffer
import math._
import collection.mutable

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

    truncateAt(num / den, 3)
  }

  def topMatches(prefs: Map[String, Map[String, Double]], person1: String) = {
    val scores = new ListBuffer[(String, Double)]
    prefs.keys.foreach(person2 => {
      if (person2 != person1) {
        val result = sim_pearson(prefs, person1, person2)
        scores += ((person2, result))
      }
    })

    scores.sortWith(_._2 > _._2)
  }

  def getRecommendations(prefs: Map[String, Map[String, Double]], person1: String) = {

    val totals = new mutable.HashMap[String, Double]
    val simSums = new mutable.HashMap[String, Double]

    prefs.keys.foreach(person2 => {
      if (person1 != person2) {
        val sim = sim_pearson(prefs, person1, person2)
        if (sim > 0) {
          prefs(person2).keys.foreach(item => {
            if (prefs(person1).get(item).isEmpty || prefs(person1)(item) == 0) {
              if (!totals.isDefinedAt(item)) totals += item -> 0
              totals(item) += prefs(person2)(item)

              if (!simSums.isDefinedAt(item)) simSums += item -> 0
              simSums(item) += sim
            }
          })
        }
      }
    })

    val rankings = new ListBuffer[(String, Double)]
    totals.foreach(entry => {
      val score = entry._2 / simSums(entry._1)
      rankings += entry._1 -> score
    })

    rankings.sortWith(_._2 > _._2)
  }

  def truncateAt(n: Double, p: Int): Double = {
    val s = pow(10, p)
    floor(n * s) / s
  }
}
