package chapter2

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
   * Returns a distance-based similarity score for person1 and person2
   */
  def sim_distance(prefs: Map[String, Map[String, Double]], person1: String, person2: String): Double = {
    val si = for (item <- prefs(person1) if (prefs(person2).isDefinedAt(item._1))) yield item._1
    if (si.size == 0) return 0

    val sum_of_squares = si.map(item => pow(prefs(person1)(item) - prefs(person2)(item), 2)).sum

    1 / (1 + sum_of_squares)
  }

  /**
   * Returns the Pearson correlation coefficient for p1 and p2
   * @param prefs
   * @param person1
   * @param person2
   * @return
   */
  def sim_pearson(prefs: Map[String, Map[String, Double]], person1: String, person2: String): Double = {

    val si = for (item <- prefs(person1) if (prefs(person2).isDefinedAt(item._1))) yield item._1
    if (si.size == 0) return 0
    val n = si.size

    //Sums of all the preferences
    val sum1 = si.map(prefs(person1)(_)).sum
    val sum2 = si.map(prefs(person2)(_)).sum

    //Sums of the squares
    val sum1Sq = si.map(item => pow(prefs(person1)(item), 2)).sum
    val sum2Sq = si.map(item => pow(prefs(person2)(item), 2)).sum

    //Sum of the products
    val pSum = si.map(item => prefs(person1)(item) * prefs(person2)(item)).sum

    //Calculate r (Pearson score)
    val num = pSum - (sum1 * sum2 / n)
    val den = sqrt((sum1Sq - pow(sum1, 2) / n) * (sum2Sq - pow(sum2, 2) / n))
    if (den == 0) return 0

    truncateAt(num / den, 3)
  }

  def topMatches(prefs: Map[String, Map[String, Double]], person1: String,
                 distance:(Map[String, Map[String, Double]], String, String) => Double = sim_pearson) = {
    val scores = for (person2 <- prefs.keys.toList if (person1 != person2)) yield {
      person2 -> distance(prefs, person1, person2)
    }
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
              totals(item) += prefs(person2)(item) * sim

              if (!simSums.isDefinedAt(item)) simSums += item -> 0
              simSums(item) += sim
            }
          })
        }
      }
    })

    val rankings = new ListBuffer[(String, Double)]
    totals.foreach(item => {
      val score = item._2 / simSums(item._1)
      rankings += item._1 -> score
    })

    rankings.sortWith(_._2 > _._2)
  }

  def transformPrefs(prefs: Map[String, Map[String, Double]]) = {
    val items = prefs.flatMap(_._2)
    items.flatMap(item => Map(item._1 -> (
      for (person <- prefs.keys if (prefs(person).isDefinedAt(item._1))) yield person -> prefs(person)(item._1)).toMap))
  }

  def loadMovieLens(path: String = "ml-100k") = {
    val movies = io.Source.fromFile(path + "/u.item", "latin1").getLines().map(line => {
      val data = line.split('|')
      val id = data(0)
      val title = data(1)
      id -> title
    }).toMap

    val prefs = new mutable.HashMap[String, Map[String, Double]]()
    io.Source.fromFile(path + "/u.data").getLines().foreach(line => {
      val data = line.split('\t')
      val user = data(0)
      val moviesId = data(1)
      val rating = data(2).toDouble
      if (!prefs.isDefinedAt(user)) prefs += user -> Map(movies(moviesId) -> rating)
      else prefs(user) += movies(moviesId) -> rating
    })

    prefs.toMap
  }

  def getRecommendedItems(prefs: Map[String, Map[String, Double]], itemMatch: Map[String, Map[String, Double]], user: String) = {
    val userRatings = prefs(user)
    val scores = new mutable.HashMap[String, Double]
    val totalSim = new mutable.HashMap[String, Double]

    for ((item, ratting) <- userRatings)
      for ((item2, similarity) <- itemMatch(item) if !userRatings.isDefinedAt(item2) && similarity > 0) {
        if (!scores.isDefinedAt(item2)) scores += item2 -> 0

        //Weighted sum of rating times similarity
        scores(item2) += similarity * ratting

        if (!totalSim.isDefinedAt(item2)) totalSim += item2 -> 0
        //Sum of all the similarities
        totalSim(item2) += similarity
      }
    //Divide each total score by total weighting to get an average
    val rankings = for ((item, score) <- scores) yield {
      item -> score / totalSim(item)
    }

    rankings.toList.sortWith(_._2 > _._2)
  }

  def calculateSimilarItems(prefs: Map[String, Map[String, Double]]) = {
    val itemPrefs = transformPrefs(prefs)
    //Using scala parallel collection for speeding up.
    val result = itemPrefs.par.map(pref => {
      pref._1 -> topMatches(itemPrefs, pref._1).toMap
    })
    result.toList.toMap
  }

  def truncateAt(n: Double, p: Int): Double = {
    val s = pow(10, p)
    floor(n * s) / s
  }
}
