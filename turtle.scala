/**
  * Created by am_dev on 6/28/16.
  */

import doodle.core._
import doodle.core.Image._
import doodle.syntax._
import doodle.jvm.Java2DCanvas._
import doodle.backend.StandardInterpreter._
import doodle.turtle._
import doodle.turtle.Instruction._

object turtle extends App {

  //exercise 1
  def polygon(sides: Int, sideLength: Double): Image = {
    val rotation = Angle.degrees(360) / sides

    def iter(n: Int): List[Instruction] = {
      n match {
        case 0 => Nil
        case n => turn(rotation) :: forward(sideLength) :: iter(n - 1)
      }
    }

    Turtle.draw(iter(sides))
  }

  val poly = polygon(10, 100)
  poly.draw

  //exercise 2
  def squareSpiral(): Image = {
    val rotation = Angle.degrees(89)

    def iter(n: Int, len: Int): List[Instruction] = {
      n match {
        case 0 => Nil
        case n => turn(rotation) :: forward(len) :: iter(n - 1, len + 1)
      }
    }

    Turtle.draw(iter(100, 5))
  }

  val sSpiral = squareSpiral()
  sSpiral.draw

  //exercise 3
  // There are 2 parts needed for Turtle instructions

  //exercise 4
  // I don't think we can

  //exercise 5
  def double[T](list: List[T]): List[T] = {
    list.flatMap({
      item => List(item, item)
    })
  }

  //exercise 6
  def nothing[T](list: List[T]): List[T] = {
    list.flatMap({
      item => List()
    })
  }

  //exercise 7
  def rewrite(instructions: List[Instruction], rule: Instruction => List[Instruction]): List[Instruction] = {
    instructions.flatMap({
      item =>
        item match {
          case Branch(item) => List(branch(rewrite(item, rule):_*))
          case other => rule(other)
        }
    })
  }

  //exercise 8
  def iterate(steps: Int, seed: List[Instruction], rule: Instruction => List[Instruction]): List[Instruction] = {
    //TODO
  }

  //exercise 9

}
