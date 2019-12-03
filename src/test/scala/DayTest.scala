import aoc2019.day01
import org.scalatest.{FunSuite, Matchers}

/** @version 1.0.0 */
class DayTest extends FunSuite with Matchers {

  test("day test") {
    day01.result1() should be (538)
  }

  test("innerFind should be 77271") {
    day01.result2() should be (77271)
  }

}