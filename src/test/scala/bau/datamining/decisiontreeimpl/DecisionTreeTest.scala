package bau.datamining.decisiontreeimpl

import org.scalatest.WordSpec

/**
  * Created by sercan on 4.12.2016.
  */
class DecisionTreeTest extends WordSpec {


  "A Given Train and Test Sets" when {
    val train = Array(Array("0", "0", "0", "T"),
      Array("0", "0", "1", "T"),
      Array("0", "1", "0", "T"),
      Array("0", "1", "1", "F"),
      Array("1", "0", "0", "T"),
      Array("1", "0", "0", "T"),
      Array("1", "1", "0", "F"),
      Array("1", "0", "1", "T"),
      Array("1", "1", "0", "F"),
      Array("1", "1", "0", "F"))

    val test =
      Array(Array("0", "0", "0", "T"),
        Array("0", "1", "1", "T"),
        Array("1", "1", "0", "T"),
        Array("1", "0", "1", "F"),
        Array("1", "0", "0", "T"))

    val types = Array[String](CATEGORICAL, CATEGORICAL, CATEGORICAL, CATEGORICAL)
    val header = Array[String]("A", "B", "C", "y")

    val metrics = DecisionTree.start(train, test,types,header)

    " are non empty" should {
      s"should produce precision as: 0.66" in {
        assert(roundAt2(metrics.precision) == 0.67)
      }

      "should produce recall as: 0.5" in {
        assert(roundAt2(metrics.recall) == 0.50)
      }

      "should produce error rate as: 0.60" in {
        assert(roundAt2(metrics.errorRate) == 0.60)
      }
    }
  }

  def roundAt(p: Int)(n: Double): Double = { val s = math pow (10, p); (math round n * s) / s }
  def roundAt2(p: Double) = roundAt(2)(p)
}
