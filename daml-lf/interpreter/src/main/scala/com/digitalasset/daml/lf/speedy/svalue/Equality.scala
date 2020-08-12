// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.lf.speedy
package svalue
import com.daml.lf.speedy.SError.SErrorCrash

import scala.collection.JavaConverters._

private[lf] object Equality {

  // Equality between two SValues of same type.
  // This follows the equality defined in the daml-lf spec.
  @throws[SErrorCrash]
  def areEqual(x: SValue, y: SValue): Boolean = {
    import SValue._

    var success = true
    var stackX = List(Iterator(x))
    var stackY = List(Iterator(y))
    // invariant: stackX.length == stackY.length

    @inline
    def push(xs: Iterator[SValue], ys: Iterator[SValue]) = {
      stackX = xs :: stackX
      stackY = ys :: stackY
    }

    @inline
    def pop() = {
      stackX = stackX.tail
      stackY = stackY.tail
    }

    @inline
    def compareStep(tuple: (SValue, SValue)) =
      tuple match {
        case (x: SPrimLit, y: SPrimLit) =>
          success = x == y
        case (SEnum(_, _, xRank), SEnum(_, _, yRank)) =>
          success = xRank == yRank
        case (SRecord(_, _, xs), SRecord(_, _, ys)) =>
          push(xs.iterator().asScala, ys.iterator().asScala)
        case (SVariant(_, _, xRank, x), SVariant(_, _, yRank, y)) =>
          push(Iterator(x), Iterator(y))
          success = xRank == yRank
        case (SList(xs), SList(ys)) =>
          push(xs.iterator, ys.iterator)
        case (SOptional(xOpt), SOptional(yOpt)) =>
          push(xOpt.iterator, yOpt.iterator)
        case (STextMap(xMap), STextMap(yMap)) =>
          val xKeys = xMap.keys.toSeq.sorted
          val yKeys = yMap.keys.toSeq.sorted
          push(
            new Interlace(xKeys.iterator.map(SText), xKeys.iterator.map(xMap)),
            new Interlace(yKeys.iterator.map(SText), yKeys.iterator.map(yMap)),
          )
        case (SGenMap(xMap), SGenMap(yMap)) =>
          push(
            new Interlace(xMap.keys.iterator, xMap.values.iterator),
            new Interlace(yMap.keys.iterator, yMap.values.iterator),
          )
        case (SStruct(_, xs), SStruct(_, ys)) =>
          push(xs.iterator().asScala, ys.iterator().asScala)
        case (SAny(xType, x), SAny(yType, y)) =>
          push(Iterator(x), Iterator(y))
          success = xType == yType
        case (STypeRep(xType), STypeRep(yType)) =>
          success = xType == yType
        case _ =>
          throw SErrorCrash("try to compare incomparable type")
      }

    while (success && stackX.nonEmpty) {
      if (stackX.head.hasNext != stackY.head.hasNext)
        success = false
      else if (stackX.head.hasNext)
        compareStep((stackX.head.next(), stackY.head.next()))
      else
        pop()
    }

    success
  }

  // Assumes iterLeft.size == iterRight.size at initialization
  private[this] class Interlace[X](iterLeft: Iterator[X], iterRight: Iterator[X])
      extends Iterator[X] {
    private[this] var left = true

    override def hasNext: Boolean = iterRight.hasNext

    override def next(): X = {
      if (left) {
        left = false
        iterLeft.next()
      } else {
        left = true
        iterRight.next()
      }
    }
  }

}
