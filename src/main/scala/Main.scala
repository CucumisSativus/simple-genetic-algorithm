/**
 * Created by michal on 21.05.15.
 */
import scala.math._
package SGA {

object Main {
  type IntervalBoundaries = (Double, Double)
  type SignificantDigitsNum = Int


  def calculateBitLength(boundaries: IntervalBoundaries, significantDigitsNum: SignificantDigitsNum): Int = {
    val leftSide = (boundaries._2 - boundaries._1) * pow(10.toDouble, significantDigitsNum.toDouble) + 1
    ceil(log(leftSide.toDouble) / log(2.toDouble)).toInt
  }
  def functionToOptimize(x: Double): Double ={
    (exp(x) * sin(10 * Pi * x) +1)/ (x +5)
  }

  def main(arguments: Array[String]): Unit = {
    val intervalBoundaries = new IntervalBoundaries(0.5, 2.5)
    val significantDigitsNum: SignificantDigitsNum = 6
  }
}

}