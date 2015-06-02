/**
 * Created by michal on 21.05.15.
 */
import scala.math._
package SGA {

object Main {
  type IntervalBoundaries = (Double, Double)
  type SignificantDigitsNum = Int
  type ChromosomeArr = Array[Chromosome]
  type ChromsomeConfiguration = (IntervalBoundaries, SignificantDigitsNum)


  def functionToOptimize(x: Double): Double ={
    (exp(x) * sin(10 * Pi * x) +1)/ (x +5)
  }

  def generateInitialPopulation(initialPopulationSize : Int, configuration: ChromsomeConfiguration) : ChromosomeArr ={
    Array.fill(initialPopulationSize)(new Chromosome(configuration))
  }
  def main(arguments: Array[String]): Unit = {
    val intervalBoundaries = new IntervalBoundaries(0.5, 2.5)
    val significantDigitsNum: SignificantDigitsNum = 6
    val initialPopulationSize = 50
    val initialPopulation = generateInitialPopulation(initialPopulationSize, (intervalBoundaries, significantDigitsNum))
  }
}

}