package aoc2019

import scala.collection.mutable
import scala.io.Source

////1
//case class Multiply(firstIndex: Int, secondIndex: Int, destinationIndex: Int, instructions: List[Int])
////2
//case class Add(firstIndex: Int, secondIndex: Int, destinationIndex: Int, instructions: List[Int])
////99
//case class Complete(firstIndex: Int, secondIndex: Int, destinationIndex: Int, instructions: List[Int])

abstract class Action
case class Multiply() extends Action
case class Add() extends Action

object day02 extends App {

  def getInput: List[Int] = {
    val filename = "resources/day02_input.txt"
    val value: String = Source.fromFile(filename).getLines().toList.head
    value.split(',').map(in => in.toInt).toList
//    Source.fromFile(filename).getLines().map(in => in.toInt).toList
  }

  val inputList: scala.collection.mutable.ArrayBuffer[Int] = scala.collection.mutable.ArrayBuffer(getInput: _*)

  var current_index: Int = 0
  var current_value: Int = inputList(current_index)

  def valAt(idx: Int): Int = {
    inputList(idx)
  }

  def actOnList(actionInput: scala.collection.mutable.ArrayBuffer[Int], act: Action, actIdx: Int): scala.collection.mutable.ArrayBuffer[Int] = {
    act match {
      case Multiply() => {
        actionInput(valAt(actIdx+3)) = valAt(valAt(actIdx+1)) * valAt(valAt(actIdx+2))
        actionInput
      }
      case Add() => {
        actionInput(valAt(actIdx+3)) = valAt(valAt(actIdx+1)) + valAt(valAt(actIdx+2))
        actionInput
      }
    }
  }


  //restart
  // replace position 1 with the value 12 and replace position 2 with the value 2
  inputList(1) = 12
  inputList(2) = 2

  while ( valAt(current_index) != 99 ) {
    val action = valAt(current_index)
    val act = action match {
      case 2 => Multiply()
      case 1 => Add()
    }
    actOnList(inputList, act, current_index)

    current_index = current_index + 4
  }

  println("Part1 result="+inputList.head)


}
