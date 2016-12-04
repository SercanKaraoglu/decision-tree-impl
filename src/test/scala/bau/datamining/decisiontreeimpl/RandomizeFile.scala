package bau.datamining.decisiontreeimpl

import java.io.{File, InputStream}

import bau.datamining.decisiontreeimpl.io.{Line, TemplateMetaData}
import org.scalatest.WordSpec

import scala.io.Source

/**
  * Created by sercan on 4.12.2016.
  */
class RandomizeFile  extends WordSpec {


    "A Given Train and Test Sets" when {
      val lines: Iterator[String] = Source.fromInputStream(getClass.getResourceAsStream("/lineNumbers.log")).getLines()
      import util.Random
      val shuffled = Random.shuffle(lines)
      val (train, test) = (shuffled.slice(0,8).toVector.par,shuffled.slice(8,10).toVector.par)
      val _lines = lines
      val trainSet = train.mkString(",")
      val testSet = test.mkString(",")
      println(trainSet)
      println(testSet)
      " randomize" should {
        s"train should be randomly splitted" in {
          assert("1,2,3,4,5,6,7,8" != trainSet)
        }

        "test should be randomly splitted" in {
          assert("9,10" != testSet)
        }
      }
    }

}
