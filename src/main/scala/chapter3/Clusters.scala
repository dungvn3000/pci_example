package chapter3

import scala.math._

/**
 * The Class Clusters.
 *
 * @author Nguyen Duc Dung
 * @since 10/20/12 5:33 PM
 *
 */
object Clusters {

  def pearson(v1: List[Double], v2: List[Double]) {
    val sum1 = v1.sum
    val sum2 = v2.sum

    val sum1Sq = v1.reduceLeft(pow(_, 2) + pow(_,2))
    val sum2Sq = v2.reduceLeft(pow(_, 2) + pow(_,2))

  }


}
