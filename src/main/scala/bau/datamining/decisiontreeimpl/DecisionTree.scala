package bau.datamining.decisiontreeimpl

import bau.datamining.decisiontreeimpl.io._

import scala.annotation.tailrec
import scala.util.Random

/**
  * Created by sercan on 28.11.2016.
  */
sealed abstract class Tree

case class EmptyLeaf() extends Tree

case class NonEmptyLeaf(targetValues: ValueSet) extends Tree

case class Split(informationGain: Double, attrId: Int, value: Any, set1: DataSet, set2: DataSet)

case class Node(attrId: Int, value: Any, leftBranch: Tree, rightBranch: Tree) extends Tree

class DecisionTree(dataSet: DataSet, metaData: TemplateMetaData, score: (Double, Double) => Double, y: Int) {
  val targetValues = dataSet.map(get(y)).distinct
  val currentScore = _score(dataSet)

  def classify(tree: Tree, testSource: Vector[Row]): Vector[Any] = {
    @tailrec def classifier(t: Tree, observation: Row): Any = t match {
      case Node(attrId: Int, value: String, leftBranch: Tree, rightBranch: Tree) => if (value == observation.getString(attrId)) classifier(leftBranch, observation) else classifier(rightBranch, observation)
      case Node(attrId: Int, value: Double, leftBranch: Tree, rightBranch: Tree) => if (value > observation.getDouble(attrId)) classifier(leftBranch, observation) else classifier(rightBranch, observation)
      case NonEmptyLeaf(targetValues: ValueSet) => targetValues.head
      case EmptyLeaf() => Nil
    }

    testSource.map(classifier(tree, _))
  }

  def get(i: Int): Row => Any = metaData.types(i) match {
    case NUMERIC => _.getDouble(i)
    case CATEGORICAL => _.getString(i)
  }

  def buildTree(): Tree = {
    if (dataSet.isEmpty) {
      EmptyLeaf()
    } else {
      val possibleSplits = for {attributeId <- dataSet.head.indices().withFilter(_ != y)
                                values = dataSet.map(get(attributeId)).distinct
                                value <- values
                                p = partitioner(attributeId, value)
                                (set1, set2) = dataSet.partition(p)
                                if set1.nonEmpty && set2.nonEmpty}
        yield {
          Split(informationGain(set1, set2, values), attributeId, value, set1, set2)
        }

      if (possibleSplits.nonEmpty) {
        val bestSplit = possibleSplits.maxBy(_.informationGain)
        val leftBranch = new DecisionTree(bestSplit.set1, metaData, score, y).buildTree()
        val rightBranch = new DecisionTree(bestSplit.set2, metaData, score, y).buildTree()
        Node(bestSplit.attrId, bestSplit.value, leftBranch, rightBranch)
      } else {
        NonEmptyLeaf(targetValues)
      }
    }
  }

  private def _score(d: DataSet): Double = {
    val (trueBranch, falseBranch) = d.partition(partitioner(y, targetValues.head))
    score(trueBranch.size, falseBranch.size)
  }

  private def partitioner(attributeId: Int, value: Any): Row => Boolean =
    value match {
      case v: Double => _.getDouble(attributeId) > v
      case v: String => _.getString(attributeId) == v
    }

  private def informationGain(set1: DataSet, set2: DataSet, values: ValueSet): Double = {
    val c0 = set1.size.toDouble
    val c1 = set2.size.toDouble
    val total = c0 + c1
    val f0 = c0 / total
    val f1 = c1 / total
    currentScore - f0 * _score(set1) - f1 * _score(set2)
  }


}

object DecisionTree {
  def main(args: Array[String]): Unit = {
    val results = DecisionTree.start("data/bank.csv", Array(NUMERIC, CATEGORICAL, CATEGORICAL, CATEGORICAL, CATEGORICAL, NUMERIC, CATEGORICAL, CATEGORICAL, CATEGORICAL, NUMERIC, CATEGORICAL, NUMERIC, NUMERIC, NUMERIC, NUMERIC, CATEGORICAL, CATEGORICAL))

    val totalTestCount = results.size
    println(s"total test count: $totalTestCount")
    results.foreach(_.printConsole())

    val avgPrecision = results.map(_.precision()).sum/totalTestCount
    println(s"Average Precision $avgPrecision")

    val avgRecall = results.map(_.recall()).sum/totalTestCount
    println(s"Average Recall $avgRecall")

    val avgErrorRate = results.map(_.errorRate()).sum/totalTestCount
    println(s"Average Error Rate $avgErrorRate")

  }

  def start(trainPath: String, testPath: String, types: Array[String]): Metrics = {
    //new TreeGrowth(dataSource.source, dataSource.metaData, Score.entropy, 16)
    val trainSourceSource = ParallelDataSource(trainPath, types)
    val (testSource, templateMetaData) = ReadToVector.fromFile(testPath, types)
    val decisionTree = new DecisionTree(trainSourceSource.source, trainSourceSource.metaData, Score.entropy, 16)
    val tree: Tree = decisionTree.buildTree()

    val pred = decisionTree.classify(tree, testSource).collect { case a: String => a }
    val actual = testSource.map(_.getString(16)).collect { case a: String => a }
    Metrics(actual, pred, "yes", "no")
  }

  def start(path: String, types: Array[String]):Array[Metrics] = {
    val (source, metaData) = ReadToVector.fromFile(path, types)
    new Iterator[Metrics]{
      var cursor = 0

      override def hasNext: Boolean = cursor<10

      override def next(): Metrics = {
        cursor+=1
        val shuffled = Random.shuffle(source)
        val splitAt = Math.floor(source.size*(9d/10)).toInt
        val (train, test) = (shuffled.slice(0, splitAt).par, shuffled.slice(splitAt, source.size))

        val decisionTree = new DecisionTree(train, metaData, Score.entropy, 16)
        val tree: Tree = decisionTree.buildTree()
        val pred = decisionTree.classify(tree,test).collect { case a: String => a }
        val actual = test.map(_.getString(16)).collect { case a: String => a }
        Metrics(actual, pred, "yes", "no")
      }
    }.toArray
  }

  def start(train: Array[Array[String]], test: Array[Array[String]], types: Array[String], header: Array[String]): Metrics = {
    val trainSource = ParallelDataSource(train, types, header)
    val testSource = ReadToVector.fromMemory(test, types, header)
    val decisionTree = new DecisionTree(trainSource.source, trainSource.metaData, Score.entropy, 3)
    val tree: Tree = decisionTree.buildTree()

    val pred = decisionTree.classify(tree, testSource).collect { case a: String => a }
    val actual = testSource.map(_.getString(3)).collect { case a: String => a }
    Metrics(actual, pred, "T", "F")
  }
}

