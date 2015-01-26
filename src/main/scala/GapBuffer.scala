package com.gregghz.ds

import scalaz.State

case class GapState(
  point: Int,
  gapStart: Int, // first non-character element of gap
  gapEnd: Int // first valid char in right set
)

class GapBuffer(size: Int = 30) {

  // make it possible to pass these two in so that a new GapBuffer instance
  // can be used on subsequent iterations
  private val state = GapState(0, 0, size)
  private[ds] var buffer = new Array[Char](size)

  def go(machine: State[GapState, String]) = {
    machine(state)
  }

  def insert(c: Char) = State[GapState, Unit] { case state =>
    val newGapStart = state.point + 1
    val realState = if (newGapStart == state.gapEnd) {
      expand(state)
    } else {
      state
    }
    buffer(realState.point) = c
    (realState.copy(
      point = realState.point + 1,
      gapStart = realState.point + 1
    ), ())
  }

  private def expand(state: GapState): GapState = {
    val newBuffer = new Array[Char](buffer.size * 2)
    val newGapSize = buffer.size + 1
    // copy left set into newBuffer
    (0 until state.gapStart).foreach { i =>
      newBuffer(i) = buffer(i)
    }
    (state.gapEnd until buffer.size).foreach { i =>
      newBuffer(i + newGapSize - 1) = buffer(i)
    }
    buffer = newBuffer
    GapState(state.gapStart, state.gapStart, state.gapStart + newGapSize)
  }

  val delete = State[GapState, Unit] { case state =>
    val newGapStart = state.gapStart - 1
    if (newGapStart < 0)
      (state, ())
    else
      (state.copy(newGapStart, newGapStart), ())
  }

  val left = State[GapState, Unit] { case state =>
    val newGapEnd = state.gapEnd - 1
    val newGapStart = state.gapStart - 1
    
    if (newGapStart < 0) {
      (state, ())
    } else {
      buffer(newGapEnd) = buffer(newGapStart)
      (state.copy(newGapStart, newGapStart, newGapEnd), ())
    }
  }

  val right = State[GapState, Unit] { case state =>
    val newGapEnd = state.gapEnd + 1
    val newGapStart = state.gapStart + 1

    if (newGapEnd > buffer.size) {
      (state, ())
    } else {
      buffer(state.gapStart) = buffer(state.gapEnd)
      (state.copy(state.point + 1, newGapStart, newGapEnd), ())
    }
  }

  val print = State[GapState, String] { case state =>
    val leftText = (0 until state.gapStart).map(buffer(_))
    val rightText = (state.gapEnd until buffer.size).map(buffer(_))
    (state, (leftText ++ rightText).mkString)
  }

}
