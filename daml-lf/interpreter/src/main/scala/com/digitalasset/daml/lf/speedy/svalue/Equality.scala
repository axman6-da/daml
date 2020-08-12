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
    var xs = List(Iterator(x))
    var ys = List(Iterator(y))

    while (success && xs.nonEmpty) {
      // invariant: xs.length == ys.length

      if (xs.head.hasNext) {
        if (ys.head.hasNext) {
          (xs.head.next(), ys.head.next()) match {
            case (x: SPrimLit, y: SPrimLit) =>
              success = x == y
            case (SEnum(_, _, rank1), SEnum(_, _, rank2)) =>
              success = rank1 == rank2
            case (SRecord(_, _, args1), SRecord(_, _, args2)) =>
              xs = args1.iterator().asScala :: xs
              ys = args2.iterator().asScala :: ys
            case (SVariant(_, _, rank1, arg1), SVariant(_, _, rank2, arg2)) =>
              xs = Iterator(arg1) :: xs
              ys = Iterator(arg2) :: ys
              success = rank1 == rank2
            case (SList(lst1), SList(lst2)) =>
              xs = lst1.iterator :: xs
              ys = lst2.iterator :: ys
            case (SOptional(x1), SOptional(x2)) =>
              xs = x1.iterator :: xs
              ys = x2.iterator :: ys
            case (STextMap(map1), STextMap(map2)) =>
              val keys1 = map1.keys.toSeq.sorted
              val keys2 = map2.keys.toSeq.sorted
              xs = new Interlace(keys1.iterator.map(SText), keys1.iterator.map(map1)) :: xs
              ys = new Interlace(keys2.iterator.map(SText), keys2.iterator.map(map2)) :: ys
            case (SGenMap(map1), SGenMap(map2)) =>
              xs = new Interlace(map1.keys.iterator, map1.values.iterator) :: xs
              ys = new Interlace(map2.keys.iterator, map2.values.iterator) :: ys
            case (SStruct(_, args1), SStruct(_, args2)) =>
              xs = args1.iterator().asScala :: xs
              ys = args2.iterator().asScala :: ys
            case (SAny(t1, v1), SAny(t2, v2)) =>
              xs = Iterator(v1) :: xs
              ys = Iterator(v2) :: ys
              success = t1 == t2
            case (STypeRep(t1), STypeRep(t2)) =>
              success = t1 == t2
            case _ =>
              throw SErrorCrash("try to compare incomparable type")
          }
        } else {
          success = false
        }
      } else {
        if (ys.head.hasNext) {
          success = false
        } else {
          xs = xs.tail
          ys = ys.tail
        }
      }
    }

    success

  }

  // Assumes iterLeft.size == iterRight.size
  private[this] class Interlace[X](iterLeft: Iterator[X], iterRight: Iterator[X])
      extends Iterator[X] {
    private[this] var left = true

    override def hasNext: Boolean =
      if (left) iterLeft.hasNext else iterRight.hasNext

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
