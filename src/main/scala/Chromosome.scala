/**
 * Created by michal on 21.05.15.
 */
import scala.math._
package SGA {

import SGA.Main.{SignificantDigitsNum, IntervalBoundaries, ChromsomeConfiguration}

class Chromosome(boundaries: IntervalBoundaries , significantDigitsNum:  SignificantDigitsNum) {

  def this(chromsomeConfiguration: ChromsomeConfiguration) {
    this(chromsomeConfiguration._1, chromsomeConfiguration._2)
  }

  private val _length = (calculateBitLength(boundaries, significantDigits))
  private var _bits = (generateBits)
  private val _intervalBegin = boundaries._1
  private val _intervalEnd = boundaries._2
  private val _significantDigits = significantDigitsNum


  def length = _length
  def bits = _bits
  def intervalBegin = _intervalBegin
  def intervalEnd = _intervalEnd
  def significantDigits = _significantDigits

  def cross(index: Int, other: Chromosome): (Chromosome, Chromosome) = {
    val thisDivided = divide(index)
    val otherDivided = other.divide(index)
    val newThis = new Chromosome((intervalBegin, intervalEnd), significantDigits)
    val newOther = new Chromosome((other.intervalBegin, other.intervalEnd), other.significantDigits)
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
    (1 to length).map(
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

  private def calculateBitLength(boundaries: IntervalBoundaries, significantDigitsNum: SignificantDigitsNum): Int = {
    val leftSide = (boundaries._2 - boundaries._1) * pow(10.toDouble, significantDigitsNum.toDouble) + 1
    ceil(log(leftSide.toDouble) / log(2.toDouble)).toInt
  }
}

}