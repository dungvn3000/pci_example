package chapter2

import org.scalatest.FunSuite
import DataSet._
import Recommendations._

/**
 * The Class Chapter2Suite.
 *
 * @author Nguyen Duc Dung
 * @since 10/17/12 3:40 PM
 *
 */
class Chapter2Suite extends FunSuite {

  test("caculate pearson correlation score") {
    val result1 = sim_pearson(critics, "Lisa Rose", "Gene Seymour")
    assert(result1 == 0.396)

    val result2 = sim_pearson(critics, "Lisa Rose", "Toby")
    assert(result2 == 0.991)
  }

  test("top matchers") {
    assert(topMatches(critics, "Toby")(0)._1 == "Lisa Rose")
    assert(topMatches(critics, "Toby")(0)._2 == 0.991)
    assert(topMatches(critics, "Michael Phillips")(0)._1 == "Claudia Puig")
    assert(topMatches(critics, "Michael Phillips")(0)._2 == 1)
  }

  test("get recommendations") {
    assert(getRecommendations(critics, "Michael Phillips")(0)._1 == "Just My Luck")
    assert(getRecommendations(critics, "Toby")(0)._1 == "The Night Listener")
  }

  test("top matcher for a movie") {
    val flipCritics = transformPrefs(critics)
    assert(topMatches(flipCritics, "Superman Returns").head._1 == "You, Me and Dupree")
    assert(topMatches(flipCritics, "Superman Returns").head._2 == 0.657)
  }

  test("test with real data uing user base recommendation") {
    val prefs = loadMovieLens()
    var time = System.currentTimeMillis()
    getRecommendations(prefs, "87").take(10).foreach(println)

    time = System.currentTimeMillis() - time

    println("get recommendatons " + time + " ms")
  }

  test("test with real data using item base recommendation") {
    var time = System.currentTimeMillis()
    val prefs =loadMovieLens()
    val itemsim = calculateSimilarItems(prefs)
    time = System.currentTimeMillis() - time
    println("builing dictionary " + time + " ms")

    time = System.currentTimeMillis()
    getRecommendedItems(prefs, itemsim, "87").take(10).foreach(println)
    time = System.currentTimeMillis() - time
    println("get recommendations " + time + " ms")
  }
}
