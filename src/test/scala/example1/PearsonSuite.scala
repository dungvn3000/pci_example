package example1

import org.scalatest.FunSuite
import DataSet._
import Recommendations._
import math._

/**
 * The Class PearsonSuite.
 *
 * @author Nguyen Duc Dung
 * @since 10/17/12 3:40 PM
 *
 */
class PearsonSuite extends FunSuite {

  test("caculate pearson correlation score") {
    val result1 = sim_pearson(critics, "Lisa Rose", "Gene Seymour")
    assert(result1 == 0.396)

    val result2 = sim_pearson(critics, "Lisa Rose", "Toby")
    assert(result2 == 0.991)
  }

  test("top matchers") {
    assert(topMatches(critics,"Toby")(0)._1 == "Lisa Rose")
    assert(topMatches(critics,"Toby")(0)._2 == 0.991)
    assert(topMatches(critics,"Michael Phillips")(0)._1 == "Claudia Puig")
    assert(topMatches(critics,"Michael Phillips")(0)._2 == 1)
  }
}
