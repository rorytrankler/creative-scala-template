/**
  * Created by am_dev on 6/7/16.
  */

import doodle.core._
import doodle.core.Image._
import doodle.core.Point._
import doodle.syntax._
import doodle.jvm.Java2DCanvas._
import doodle.backend.StandardInterpreter._
import doodle.examples._

object week4 extends App {

  // exercise 1
  val triangle =
    List(
      MoveTo(polar(100, 0.degrees)),
      LineTo(polar(100, 120.degrees)),
      LineTo(polar(100, 240.degrees))
    )

  val square =
    List(
      MoveTo(polar(100, 0.degrees)),
      LineTo(polar(100, 90.degrees)),
      LineTo(polar(100, 180.degrees)),
      LineTo(polar(100, 270.degrees))
    )

  val pentagon =
    List(
      MoveTo(polar(100, 0.degrees)),
      LineTo(polar(100, 72.degrees)),
      LineTo(polar(100, 144.degrees)),
      LineTo(polar(100, 216.degrees)),
      LineTo(polar(100, 288.degrees))
    )

  val image =
    closedPath(triangle) beside closedPath(square) beside closedPath(pentagon)

  image.draw

  // exercise 2

  val curve =
    List(BezierCurveTo(cartesian(50, 100), cartesian(100, 100), cartesian(150, 0)))

  val triangle2 =
    List(
      MoveTo(polar(100, 0.degrees)),
      LineTo(polar(100, 120.degrees)),
      LineTo(polar(100, 240.degrees))
    )

  val square2 =
    List(
      MoveTo(polar(100, 0.degrees)),
      LineTo(polar(100, 90.degrees)),
      LineTo(polar(100, 180.degrees)),
      LineTo(polar(100, 270.degrees))
    )

  val pentagon2 =
    List(
      MoveTo(polar(100, 0.degrees)),
      LineTo(polar(100, 72.degrees)),
      LineTo(polar(100, 144.degrees)),
      LineTo(polar(100, 216.degrees)),
      LineTo(polar(100, 288.degrees))
    )

  val image2 =
    closedPath(triangle) beside closedPath(square) beside closedPath(pentagon)

  image2.draw

  //exercise 3
  def ones(n: Int): List[Int] = {
    n match {
      case 0 => Nil
      case n => 1 :: ones(n - 1)
    }
  }

  //exercise 4
  def descending(n: Int): List[Int] = {
    n match {
      case 0 => Nil
      case n => n :: descending(n -1)
    }
  }

  //exercise 5
  def ascending(n: Int): List[Int] = {
    n match {
      case 0 => Nil
      case n => n :: ascending(n - 1)
    }
  }

  //exercise 6
  def fill[A](n: Int, a: A): List[A] = {
    n match {
      case 0 => Nil
      case n => a :: fill(n - 1, a)
    }
  }

  //exercise 7
  def double(list: List[Int]): List[Int] = {
    list match {
      case Nil => Nil
      case hd :: tl => hd * 2 :: tl
    }
  }

  //exercise 8
  def product(list: List[Int]): Int = {
    list match {
      case Nil => 0
      case hd :: tl => hd * product(tl)
    }
  }

  //exercise 9
  def contains[A](list: List[A], a: A): Boolean = {
    list match {
      case Nil => false
      case hd :: tl => hd == a || contains(tl, a)
    }
  }

  //exercise 10
  def first[A](list: List[A], a: A): A = {
    list match {
      case Nil => a
      case hd :: tl => hd
    }
  }

  //exercise 11
  def reverse[A](list: List[A]): List[A] = {
    def iter(list: List[A], result: List[A]): List[A] = {
      list match {
        case Nil => result
        case hd :: tl => iter(tl, hd :: result)
      }

      iter(list, Nil)
    }
  }

  //exercise 12
  def polygon(sides: Int, size: Int, rot1: Angle): Image = {
    def iter(n: Int, rot2: Angle): List[PathElement] = {
      n match {
        case 0 => Nil
        case n => LineTo(polar(size, (rot2 * n) + rot1)) :: iter(n - 1, rot2)
      }
    }

    closedPath(MoveTo(polar(size, rot1)) :: iter(sides, 360.degrees/sides))
  }

  // 9.4 exercise 1
  def star(p: Int, skip: Int, radius: Double): Image = {
    def iter(lines: Int): List[PathElement] = {
      lines match {
        case x if x == p => Nil
        case lines => LineTo(polar(radius, ((360.degrees/p) * (lines + 1) * skip) )) :: iter(lines + 1)
      }
    }

    closedPath(MoveTo(polar(radius, 0.degrees)) :: iter(0))
  }

  val img = star(11, 5, 100)
  img.draw
}
