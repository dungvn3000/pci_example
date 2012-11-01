package chapter3

import org.scalatest.FunSuite
import Clusters._

/**
 * The Class Chapter3Suite.
 *
 * @author Nguyen Duc Dung
 * @since 10/25/12 12:02 PM
 *
 */
class Chapter3Suite extends FunSuite {

  test("hcluster") {
    val (blogNames, words, data) = readFile()
    val clust = hCluster(data)
    printClust(clust(0), blogNames)
  }

  test("kcluster") {
    val (blogNames, words, data) = readFile()
  }

}
