package com.gregghz.ds

import org.specs2.mutable._
import scalaz.State

class GapBufferSpec extends Specification {
  "GapBuffer" should {
    

    "insert text correctly" in {
      val g = new GapBuffer[Char]()
      val machine = for {
        _ <- g.insert('h')
        _ <- g.insert('e')
        _ <- g.insert('l')
        _ <- g.insert('l')
        _ <- g.insert('o')
        result <- g.print
      } yield(result)

      g.go(machine)._2 mustEqual "hello"
    }

    "move point left and insert text in middle" in {
      val g = new GapBuffer[Char]()
      val machine = for {
        _ <- g.insert('h')
        _ <- g.insert('l')
        _ <- g.insert('l')
        _ <- g.insert('o')
        _ <- g.left
        _ <- g.left
        _ <- g.left
        _ <- g.insert('e')
        result <- g.print
      } yield(result)

      g.go(machine)._2 mustEqual "hello"
    }

    "move point left nad right and then insert text" in {
      val g = new GapBuffer[Char]()
      val machine = for {
        _ <- g.insert('h')
        _ <- g.insert('e')
        _ <- g.insert('l')
        _ <- g.insert('o')
        _ <- g.left
        _ <- g.insert('l')
        _ <- g.right
        _ <- g.insert(' ')
        _ <- g.insert('y')
        _ <- g.insert('o')
        _ <- g.insert('u')
        result <- g.print
      } yield(result)

      g.go(machine)._2 mustEqual "hello you"
    }

    def checkBuffer[A](str: String, expected: String) = State[GapState[A], Unit] { case state =>
      str mustEqual expected
      (state, ())
    }

    "move the point correctly" in {

      val size = 30
      val g = new GapBuffer[Char](size)
      val machine = for {
        _ <- g.print.flatMap(checkBuffer(_, ""))

        _ <- g.insert('e')
        _ <- g.print.flatMap(checkBuffer(_, "e"))

        _ <- g.insert('l')
        _ <- g.print.flatMap(checkBuffer(_, "el"))

        _ <- g.insert('l')
        _ <- g.print.flatMap(checkBuffer(_, "ell"))

        _ <- g.insert('o')
        _ <- g.print.flatMap(checkBuffer(_, "ello"))

        _ <- g.left
        _ <- g.print.flatMap(checkBuffer(_, "ello"))

        _ <- g.left
        _ <- g.print.flatMap(checkBuffer(_, "ello"))

        _ <- g.left
        _ <- g.print.flatMap(checkBuffer(_, "ello"))

        _ <- g.left
        _ <- g.print.flatMap(checkBuffer(_, "ello"))

        _ <- g.left
        _ <- g.print.flatMap(checkBuffer(_, "ello"))

        _ <- g.left
        _ <- g.print.flatMap(checkBuffer(_, "ello"))

        _ <- g.left
        _ <- g.print.flatMap(checkBuffer(_, "ello"))

        _ <- g.left
        _ <- g.print.flatMap(checkBuffer(_, "ello"))

        _ <- g.insert('h')
        _ <- g.print.flatMap(checkBuffer(_, "hello"))

        _ <- g.right
        _ <- g.print.flatMap(checkBuffer(_, "hello"))

        _ <- g.right
        _ <- g.print.flatMap(checkBuffer(_, "hello"))

        _ <- g.right
        _ <- g.print.flatMap(checkBuffer(_, "hello"))

        _ <- g.right
        _ <- g.print.flatMap(checkBuffer(_, "hello"))

        _ <- g.right
        _ <- g.print.flatMap(checkBuffer(_, "hello"))

        _ <- g.right
        _ <- g.print.flatMap(checkBuffer(_, "hello"))

        _ <- g.right
        _ <- g.print.flatMap(checkBuffer(_, "hello"))

        _ <- g.right
        _ <- g.print.flatMap(checkBuffer(_, "hello"))

        _ <- g.right
        _ <- g.print.flatMap(checkBuffer(_, "hello"))

        _ <- g.insert('!')
        _ <- g.print.flatMap(checkBuffer(_, "hello!"))
        result <- g.print
      } yield(result)

      g.go(machine)._2 mustEqual "hello!"
    }

    "delete correctly" in {
      val g = new GapBuffer[Char](30)
      val machine = for {
        _ <- g.insert('y')
        _ <- g.insert('u')
        _ <- g.delete
        _ <- g.delete
        _ <- g.delete
        _ <- g.delete
        _ <- g.insert('h')
        _ <- g.insert('e')
        _ <- g.insert('o')
        _ <- g.delete
        _ <- g.insert('l')
        _ <- g.insert('l')
        _ <- g.insert('o')
        str <- g.print
      } yield(str)

      g.go(machine)._2 mustEqual "hello"
    }

    "expand when needed" in {
      val g = new GapBuffer[Char](2)
      val machine = for {
        _ <- g.insert('w')
        _ <- g.insert('o')
        _ <- g.insert('r')
        _ <- g.insert('l')
        _ <- g.insert('d')
        _ <- g.left
        _ <- g.left
        _ <- g.left
        _ <- g.left
        _ <- g.left
        _ <- g.insert('h')
        _ <- g.insert('e')
        _ <- g.insert('l')
        _ <- g.insert('l')
        _ <- g.insert('o')
        _ <- g.insert(' ')
        str <- g.print
      } yield(str)

      g.go(machine)._2 mustEqual "hello world"
    }
  }
}