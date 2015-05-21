import SGA.Chromosome
import org.scalatest._
class ChromosomeUnit extends FlatSpec with Matchers {
  lazy val bitLength = 5
  it should "mutate properly from 1 to 0" in {
    val chromosome = new Chromosome(bitLength)
    val onePosition = chromosome.bits.indexOf(1)
    chromosome.mutate(onePosition)
    chromosome.bits(onePosition) should be(0)
  }
  it should "mutate properly from 0 to 1" in {
    val chromosome = new Chromosome(bitLength)
    val onePosition = chromosome.bits.indexOf(0)
    chromosome.mutate(onePosition)
    chromosome.bits(onePosition) should be(1)
  }

  it should "cross properly" in {
    val crossIndex = 2
    val chromosome1 = new Chromosome(bitLength)
    val chromosome2 = new Chromosome(bitLength)
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
}