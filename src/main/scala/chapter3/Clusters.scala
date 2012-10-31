package chapter3

import scala.math._
import collection.mutable.ListBuffer
import collection.mutable

/**
 * The Class Clusters.
 *
 * @author Nguyen Duc Dung
 * @since 10/20/12 5:33 PM
 *
 */
object Clusters {

  def pearson(v1: List[Double], v2: List[Double]): Double = {
    val sum1 = v1.sum
    val sum2 = v2.sum

    val sum1Sq = v1.foldLeft(0.0)(_ + pow(_, 2))
    val sum2Sq = v2.foldLeft(0.0)(_ + pow(_, 2))

    var pSum = 0.0
    val n = v1.length
    for (i <- 0 until n) pSum += v1(i) * v2(i)

    val num = pSum - (sum1 * sum2 / n)
    val den = sqrt((sum1Sq - pow(sum1, 2) / n) * (sum2Sq - pow(sum2, 2) / n))

    if (den == 0) return 0

    1.0 - num / den
  }

  def readFile(filename: String = "blogdata.txt") = {
    val lines = io.Source.fromFile(filename).getLines().toList

    //First line is the column titles
    val colNames = lines(0).stripMargin.split('\t')
    val rowNames = new ListBuffer[String]
    val data = new ListBuffer[List[Double]]

    for (line <- lines.drop(1)) {
      val p = line.stripMargin.split('\t')

      //First column in each row is the rowname
      rowNames += p(0)

      //The data for this row is the remainder of the row
      val row = for (x <- p.drop(1)) yield x.toDouble

      data += row.toList
    }

    (rowNames.toList, colNames, data.toList)
  }

  def hCluster(rows: List[List[Double]]) = {
    val distances = new mutable.HashMap[(Int, Int), Double]
    var currentClusterId = -1

    val n = rows.length
    //Cluster are initially just the rows
    var clust = (for (i <- 0 until n) yield BiCluster(rows(i), id = i)).toList
    while (clust.length > 1) {
      var lowestPair = (0, 1)
      var closest = pearson(clust(0).vec, clust(1).vec)

      //loop through every pair looking for the smallest distance
      for (i <- 0 until clust.length)
        for (j <- i + 1 until clust.length) {
          if (!distances.contains((clust(i).id, clust(j).id))) {
            val d = pearson(clust(i).vec, clust(j).vec)
            distances((clust(i).id, clust(j).id)) = d
          }

          val d = distances((clust(i).id, clust(j).id))
          if (d < closest) {
            closest = d
            lowestPair = (i, j)
          }
        }

      //calculate the average of the two clusters
      val mergeVec = for (i <- 0 until clust(0).vec.length) yield (clust(lowestPair._1).vec(i) + clust(lowestPair._2).vec(i)) / 2

      //Create the new cluster
      val newCluster = BiCluster(mergeVec.toList, left = Some(clust(lowestPair._1)),
        right = Some(clust(lowestPair._2)), distance = closest, id = currentClusterId)

      //Cluster ids that weren't in the original set are negative
      currentClusterId -= 1

      clust = removeAt(lowestPair._1, clust)
      clust = removeAt(lowestPair._2, clust)
      clust = clust ::: List(newCluster)
    }

    clust
  }

  def printClust(clust: BiCluster, labels: List[String], n: Int = 0) {
    for (i <- 0 until n) print(" ")
    if (clust.id < 0) println("-")
    else println(labels(clust.id))

    //now print the right and left branches
    if (!clust.left.isEmpty) printClust(clust.left.get, labels, n +1)
    if (!clust.right.isEmpty) printClust(clust.right.get, labels, n +1)
  }

  def removeAt[A](n: Int, ls: List[A]) = ls.splitAt(n) match {
    case (pre, e :: post) => pre ::: post
    case (Nil, _) if n < 0 => throw new NoSuchElementException
    case (pre, Nil) => pre.take(n - 1)
  }

}

case class BiCluster(vec: List[Double], left: Option[BiCluster] = None, right: Option[BiCluster] = None, distance: Double = 0.0, id: Int = -1)