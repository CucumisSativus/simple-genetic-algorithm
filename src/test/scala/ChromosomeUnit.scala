import java.awt.font.ShapeGraphicAttribute

import SGA.Chromosome
import SGA.Main.{ChromsomeConfiguration, SignificantDigitsNum, IntervalBoundaries}
import org.scalatest._
class ChromosomeUnit extends FlatSpec with Matchers {
  val boundaries = new IntervalBoundaries(0.5, 2.5)
  val significantDigits : SignificantDigitsNum = 6
  def testFunction(x : Double): Double = x
  val chromosomeConfiguration : ChromsomeConfiguration = (boundaries, significantDigits, testFunction)

  it should "mutate properly from 1 to 0" in {
    val chromosome = new Chromosome(chromosomeConfiguration)
    val onePosition = chromosome.bits.indexOf(1)
    chromosome.mutate(onePosition)
    chromosome.bits(onePosition) should be(0)
  }
  it should "mutate properly from 0 to 1" in {
    val chromosome = new Chromosome(chromosomeConfiguration)
    val onePosition = chromosome.bits.indexOf(0)
    chromosome.mutate(onePosition)
    chromosome.bits(onePosition) should be(1)
  }

  it should "cross properly" in {
    val crossIndex = 2
    val chromosome1 = new Chromosome(chromosomeConfiguration)
    val chromosome2 = new Chromosome(chromosomeConfiguration)
    val (chromosome1AfterCrossing, chromosome2AfterCrossing) = chromosome1.cross(crossIndex, chromosome2)
    for(i <- 0 until crossIndex){
      chromosome1AfterCrossing.bits(i) should be(chromosome1.bits(i))
      chromosome2AfterCrossing.bits(i) should be(chromosome2.bits(i))
    }
    for(i <- crossIndex until chromosome1.length){
      chromosome1AfterCrossing.bits(i) should be(chromosome2.bits(i))
      chromosome2AfterCrossing.bits(i) should be(chromosome1.bits(i))
    }
  }
  it should "return lower boundary when all bits are 0" in {
    val chromosome = new Chromosome(chromosomeConfiguration)
    chromosome._bits = Array.fill(chromosome.length)(0)
    chromosome.value() should be(boundaries._1)
  }
  it should "return higher boundary when all bits are 1" in {
    val chromosome = new Chromosome(chromosomeConfiguration)
    chromosome._bits = Array.fill(chromosome.length)(1)
    chromosome.value() should be(boundaries._2 +- scala.math.pow(10.0, (-1)*(significantDigits-1)))
  }
}