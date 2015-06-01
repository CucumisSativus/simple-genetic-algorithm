/**
 * Created by michal on 21.05.15.
 */
import scala.math._
package SGA {

import SGA.Main.{SignificantDigitsNum, IntervalBoundaries}

class Chromosome(bitNumber: Int, boundaries: IntervalBoundaries = new IntervalBoundaries(0.5, 2.5), significantDigitsNum: SignificantDigitsNum = 6) {

  private var _bits = (generateBits)
  private val _length = bitNumber
  private val _intervalBegin = boundaries._1
  private val _intervalEnd = boundaries._2
  private val _significantDigits = significantDigitsNum


  def bits = _bits
  def length = _length
  def intervalBegin = _intervalBegin
  def intervalEnd = _intervalEnd
  def significantDigits = _significantDigits

  def cross(index: Int, other: Chromosome): (Chromosome, Chromosome) = {
    val thisDivided = divide(index)
    val otherDivided = other.divide(index)
    val newThis = new Chromosome(length)
    val newOther = new Chromosome(other.length)
    newThis._bits = thisDivided._1.++(otherDivided._2)
    newOther._bits = otherDivided._1.++(thisDivided._2)
    (newThis, newOther)
  }

  def divide(index: Int): (Array[Int], Array[Int]) = (bits.slice(0, index), bits.slice(index, bits.length))

  def mutate(index: Integer): Unit = {
    if (_bits(index).equals(1)) {
      _bits(index) = 0
    }
    else {
      _bits(index) = 1
    }
  }
  def mutate : Unit ={
    val index = scala.util.Random.nextInt(_bits.length)
    mutate(index)
  }

  def generateBits: Array[Int] = {
    (1 to bitNumber).map(
      x => {
        scala.util.Random.nextInt(2)
      }
    ).toArray
  }

  def bitsToDecimal(): Int = {
    var sum = 0
    for (index <- 0 until bits.length) {
      sum += (bits(index) * pow(2.toDouble, index.toDouble)).toInt
    }
    sum
  }

  def value(boundaries: IntervalBoundaries, significantDigitsNum: SignificantDigitsNum): Double = {
    (boundaries._1 + bitsToDecimal() * (boundaries._2 - boundaries._1)) / (pow(2.toDouble, significantDigitsNum.toDouble) + 1)
  }

  def value(): Double ={
    value((intervalBegin, intervalEnd), significantDigits)
  }

}

}