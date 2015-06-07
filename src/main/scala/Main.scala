/**
 * Created by michal on 21.05.15.
 */
import scala.math._
package SGA {

object Main {
  type IntervalBoundaries = (Double, Double)
  type SignificantDigitsNum = Int
  type ChromosomeArr = Array[Chromosome]
  type ChromsomeConfiguration = (IntervalBoundaries, SignificantDigitsNum, (Double) => Double)

  // algorithm constants
  val intervalBoundaries = new IntervalBoundaries(0.5, 2.5)
  val significantDigitsNum: SignificantDigitsNum = 6
  val initialPopulationSize = 50
  val mutationProbability = 0.3
  val crossingProbability = 0.3
  val reproductionProbability = 1- mutationProbability - crossingProbability
  val maxGenerationsNum = 2500
  val maxRunNum = 10
  val showDebug = false

  def functionToOptimize(x: Double): Double ={
    (exp(x) * sin(10 * Pi * x) +1)/ (x +5)
  }

  def generateInitialPopulation(initialPopulationSize : Int, configuration: ChromsomeConfiguration) : ChromosomeArr ={
    Array.fill(initialPopulationSize)(new Chromosome(configuration))
  }

  def populationFitness(population: ChromosomeArr): Double ={
    var sum : Double = 0
    for(el <- population){
      sum += el.fitness()
    }
    sum
  }

  def findSolutionOnRoulette(currentPopulation: ChromosomeArr): Chromosome ={
    val probabilities = currentPopulation.map(
      chromosome => {
        (chromosome, chromosome.fitness()/populationFitness(currentPopulation))
      }
    )
    var currentSum : Double = 0
    val rouletteRoll = util.Random.nextDouble()
    for(probability <- probabilities){
      currentSum += probability._2
      if(currentSum > rouletteRoll) return probability._1
    }
    probabilities.last._1 //shouldnt land here but anyway
  }
  def performMutation(currentPopulation: ChromosomeArr): ChromosomeArr ={
    if(showDebug) println("Performing mutation")
    Array.fill(1)(findSolutionOnRoulette(currentPopulation).mutate).filter(_.isInDomain())
  }
  def performCrossing(currentPopulation: ChromosomeArr): ChromosomeArr ={
    if(showDebug) println("Performing corssing")
    val (sol1, sol2) = findSolutionOnRoulette(currentPopulation).cross(findSolutionOnRoulette(currentPopulation))
    val chromosomes = new ChromosomeArr(2)
    chromosomes(0) = sol1
    chromosomes(1) = sol2
    chromosomes.filter(_.isInDomain())
  }

  def performReproduction(currrentPopulation: ChromosomeArr): ChromosomeArr ={
    if(showDebug) println("Performing reproduction")
    Array.fill(1)(findSolutionOnRoulette(currrentPopulation)).filter(_.isInDomain())
  }
  def fillNewPopulation(currentPopulation: ChromosomeArr): ChromosomeArr ={
    var newPopulation = new ChromosomeArr(0)
    while (newPopulation.length < currentPopulation.length){
      val operationProb = util.Random.nextDouble()
      if(operationProb < mutationProbability) newPopulation ++= performMutation(currentPopulation)
      else if(operationProb < mutationProbability + crossingProbability) newPopulation ++= performCrossing(currentPopulation)
      else newPopulation ++=performReproduction(currentPopulation)
    }
    newPopulation
  }

  def runSGA(runNumber: Int): Unit ={
    var firstGeneration = generateInitialPopulation(initialPopulationSize,
      (intervalBoundaries, significantDigitsNum, functionToOptimize))
    var runBestX : Double = 0
    var runBestFitness : Double = 0
    for(generation <- 0 until maxGenerationsNum){
      val nextGeneration = fillNewPopulation(firstGeneration)
      val (bestFitness, bestSoultion) = nextGeneration.map(
        x => {
          (x.fitness(), x.value())
        }
      ).sortWith(_._1 > _._1).maxBy(_._1)
      println(s"Generation: ${generation}; bestValue: ${bestFitness}; bestX: ${bestSoultion}}")
      if(bestFitness > runBestFitness){
        runBestX = bestSoultion
        runBestFitness = bestFitness

      }
      firstGeneration = nextGeneration
    }
    println(s"Run ${runNumber}: bestX: ${runBestX}, bestFitness: ${runBestFitness}")


  }
  def main(arguments: Array[String]): Unit = {
    for(runNum <- 0 until maxRunNum){
      runSGA(runNum)
    }
  }
}

}