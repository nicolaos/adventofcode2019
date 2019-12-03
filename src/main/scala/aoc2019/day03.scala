package aoc2019

import scala.io.Source

object day03 extends App {

  sealed trait Direction
  case object Left extends Direction
  case object Right extends Direction
  case object Up extends Direction
  case object Down extends Direction
  case class Point(x: Int, y: Int)
  lazy val center = Point(0, 0)

  case class Section(direction: Direction, length: Int)

//  val filename = "resources/day03_01_mini_input.txt"
  val filename = "resources/day03_input.txt"
  val lines = Source.fromFile(filename).getLines().toList

  def parseItem(direction: String): Section = {
    direction.charAt(0) match {
      case 'U' => Section(Up, direction.substring(1).toInt)
      case 'D' => Section(Down, direction.substring(1).toInt)
      case 'R' => Section(Right, direction.substring(1).toInt)
      case 'L' => Section(Left, direction.substring(1).toInt)
    }
  }

  val wireOne: List[String] = lines.head.split(",").toList
  val wireTwo: List[String] = lines(1).split(",").toList

  println("wireOne="+wireOne)
  println("wireTwo="+wireTwo)

  println("wireOneMapped"+wireOne.map(parseItem).map(_.length))
  println("wireTwoMapped"+wireTwo.map(parseItem).map(_.length))

  def wireToPoints(wire: List[Section]): List[Point] = {
    def sectionToPoints(start: Point, section: Section): List[Point] = section.direction match {
      case Up    => (start.y + 1 to start.y + section.length).map(y => Point(start.x, y)).toList
      case Down  => (start.y - section.length until start.y).reverse.map(y => Point(start.x, y)).toList
      case Right => (start.x + 1 to start.x + section.length).map(x => Point(x, start.y)).toList
      case Left  => (start.x - section.length until start.x).reverse.map(x => Point(x, start.y)).toList
    }

    wire.foldLeft(List(center))((points, section) => points ++ sectionToPoints(points.last, section))
  }

  def manhattan(origin: Point)(point: Point): Int = {
    Math.abs(origin.x - point.x) + Math.abs(origin.y - point.y)
  }

  private val wireOnePoints: List[Point] = wireToPoints(wireOne.map(parseItem))
  private val wireTwoPoints: List[Point] = wireToPoints(wireTwo.map(parseItem))

  def stepsDistance(intersection: Point): Int = {
    wireOnePoints.indexOf(intersection) + wireTwoPoints.indexOf(intersection)
  }

  private val intersectionPoints: Set[Point] = (wireOnePoints toSet).intersect(wireTwoPoints toSet) - center
  println( "Part1="+intersectionPoints.map(manhattan(center)).min)

  println( "Part2="+intersectionPoints.map(stepsDistance).min)

}
