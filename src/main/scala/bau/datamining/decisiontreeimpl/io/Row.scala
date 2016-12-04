package bau.datamining.decisiontreeimpl.io

/**
  * Created by sercan on 30.11.2016.
  */
trait Row {
  def getDouble(i: Int): Double

  def getString(i: Int): String

  def indices(): Range
}



