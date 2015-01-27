package com.gregghz.ds

import scala.reflect.ClassTag
import scalaz.State

case class GapState[A](
  gapStart: Int, // first non-character element of gap
  gapEnd: Int, // first valid char in right set
  private[ds] buffer: Array[A]
)

class GapBuffer[A](val state: GapState[A])(implicit t: ClassTag[A]) {

  def this()(implicit t: ClassTag[A]) = {
    this(GapState(0, 30, new Array[A](30)))
  }

  def this(size: Int)(implicit t: ClassTag[A]) = {
    this(GapState(0, size, new Array[A](size)))
  }

  def go(machine: State[GapState[A], String]) = {
    machine(state)
  }

  def insert(c: A) = State[GapState[A], Unit] { case state =>
    val newGapStart = state.gapStart + 1
    val realState = if (newGapStart == state.gapEnd) {
      expand(state)
    } else {
      state
    }
    realState.buffer(realState.gapStart) = c
    (realState.copy(
      gapStart = realState.gapStart + 1
    ), ())
  }

  private def expand(state: GapState[A])(implicit t: ClassTag[A]): GapState[A] = {
    val newBuffer = new Array[A](state.buffer.size * 2)
    val newGapSize = state.buffer.size + 1
    // copy left set into newBuffer
    (0 until state.gapStart).foreach { i =>
      newBuffer(i) = state.buffer(i)
    }
    (state.gapEnd until state.buffer.size).foreach { i =>
      newBuffer(i + newGapSize - 1) = state.buffer(i)
    }

    GapState(state.gapStart, state.gapStart + newGapSize, newBuffer)
  }

  val delete = State[GapState[A], Unit] { case state =>
    val newGapStart = state.gapStart - 1
    if (newGapStart < 0)
      (state, ())
    else
      (state.copy(newGapStart), ())
  }

  val left = State[GapState[A], Unit] { case state =>
    val newGapEnd = state.gapEnd - 1
    val newGapStart = state.gapStart - 1
    
    if (newGapStart < 0) {
      (state, ())
    } else {
      state.buffer(newGapEnd) = state.buffer(newGapStart)
      (state.copy(newGapStart, newGapEnd), ())
    }
  }

  val right = State[GapState[A], Unit] { case state =>
    val newGapEnd = state.gapEnd + 1
    val newGapStart = state.gapStart + 1

    if (newGapEnd > state.buffer.size) {
      (state, ())
    } else {
      state.buffer(state.gapStart) = state.buffer(state.gapEnd)
      (state.copy(newGapStart, newGapEnd), ())
    }
  }

  val print = State[GapState[A], String] { case state =>
    val leftText = (0 until state.gapStart).map(state.buffer(_))
    val rightText = (state.gapEnd until state.buffer.size).map(state.buffer(_))
    (state, (leftText ++ rightText).mkString)
  }

}
