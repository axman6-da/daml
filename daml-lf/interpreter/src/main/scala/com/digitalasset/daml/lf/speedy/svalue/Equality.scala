// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.lf.speedy
package svalue

import scala.collection.JavaConverters._

private[lf] object Equality {

  // Equality between two SValues of same type.
  // Note it is not reflexive, in other words there is some value `v`
  // such that `areEqual(v, v)` returns `False`).
  // This follows the equality defined in the daml-lf spec.
  def areEqual(x: SValue, y: SValue): Boolean = {
    import SValue._

    var success = true
    val stack = new Stack
    stack.push(x, y)

    while (success && stack.nonEmpty) success = stack.pop() match {
      case (x: SPrimLit, y: SPrimLit) =>
        x == y
      case (SEnum(_, _, rank1), SEnum(_, _, rank2)) =>
        rank1 == rank2
      case (SRecord(_, _, args1), SRecord(_, _, args2)) =>
        stack.push(args1.iterator().asScala, args2.iterator().asScala)
        true
      case (SVariant(_, _, rank1, arg1), SVariant(_, _, rank2, arg2)) =>
        stack.push(arg1, arg2)
        rank1 == rank2
      case (SList(lst1), SList(lst2)) =>
        stack.push(lst1.iterator, lst2.iterator)
        lst1.length == lst2.length
      case (SOptional(xs), SOptional(ys)) =>
        stack.push(xs.iterator, ys.iterator)
        xs.nonEmpty == ys.nonEmpty
      case (STextMap(map1), STextMap(map2)) =>
        val keys = map1.keys
        stack.push(keys.iterator.map(map1), keys.iterator.map(map2))
        map1.size == map2.size && keys.forall(map2.keySet.contains)
      case (SGenMap(map1), SGenMap(map2)) =>
        stack.push(map1.values.iterator, map2.values.iterator)
        stack.push(map1.keys.iterator, map2.keys.iterator)
        map1.size == map2.size
      case (SStruct(fields1, args1), SStruct(fields2, args2)) =>
        stack.push(args1.iterator().asScala, args2.iterator().asScala)
        fields1 sameElements fields2
      case (SAny(t1, v1), SAny(t2, v2)) =>
        stack.push(v1, v2)
        t1 == t2
      case (STypeRep(t1), STypeRep(t2)) =>
        t1 == t2
      case _ =>
        false
    }
    success
  }

  private[this] class Stack {
    // Invariants:
    // - xs.length == ys.length
    // - xs.indices.forall(i => xs(i).size == ys(i).size)
    // - xs.forall(_.nonEmpty)
    private[this] var xs = List.empty[Iterator[SValue]]
    private[this] var ys = List.empty[Iterator[SValue]]

    def nonEmpty: Boolean = xs.nonEmpty

    def pop(): (SValue, SValue) = {
      val tuple = (xs.head.next(), ys.head.next())
      if (xs.head.isEmpty) {
        xs = xs.tail
        ys = ys.tail
      }
      tuple
    }

    def push(x: SValue, y: SValue): Unit =
      push(Iterator(x), Iterator(y))

    // Assume xs and ys have the same size.
    def push(xs: Iterator[SValue], ys: Iterator[SValue]): Unit =
      if (xs.nonEmpty) {
        this.xs = xs :: this.xs
        this.ys = ys :: this.ys
      }

  }

}
