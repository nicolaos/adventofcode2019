package aoc2018

import scala.io.Source

object day01 extends App {

  def getInput(): List[Int] = {
    val filename = "resources/day01_input.txt"
    Source.fromFile(filename).getLines().map(in => in.toInt).toList
  }

  val inputList = getInput()

  def fuelSimple(weight: Int): Int = {
    Math.floor(weight / 3).toInt - 2
  }

  // take its mass, divide by three, round down, and subtract 2
  // sum all
  def result1(): Int = {
    val result = inputList.map(n => Math.floor(n / 3).toInt - 2 ).sum
    println(s"RESULT IS = $result")
    result
  }

  //3297626
  result1()

  def result2():Int = {
    def fuelRecursive(weight: Int): Int = {
      val weightFuel = fuelSimple(weight)
      weightFuel match {
        case n if n <= 0 => 0
        case _ =>
          weightFuel + fuelRecursive(weightFuel)
      }
    }

    val fuelWithoutWish = inputList.map(fuelRecursive).sum

    println(s"RESULT 2 IS = $fuelWithoutWish")
    fuelWithoutWish
  }

  //4943578
  result2()

}
