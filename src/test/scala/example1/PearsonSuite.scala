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
    assert(truncateAt(result1, 3) == 0.396)

    val result2 = sim_pearson(critics, "Lisa Rose", "Toby")
    assert(truncateAt(result2, 3) == 0.991)
  }


  def truncateAt(n: Double, p: Int): Double = {
    val s = pow(10, p)
    floor(n * s) / s
  }
}
