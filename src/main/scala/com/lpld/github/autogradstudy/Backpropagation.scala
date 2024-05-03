package com.lpld.github.autogradstudy

object Backpropagation:
  def computeGradient(value: Value): Map[String, Double] =
    sortTopologically(value).foldLeft(Map(value.id -> 1.0)) { (grads, v) =>

      def addGrad(grads: Map[String, Double], v: Value, grad: Double): Map[String, Double] =
        val oldGrad = grads.getOrElse(v.id, 0.0)
        grads + (v.id -> (oldGrad + grad))

      val grad = grads.getOrElse(v.id, sys.error(s"no grad for ${v.id} - ${v.label}"))

      v.children match

        case NodeChildren.Empty => grads

        case NodeChildren.One(child, op) =>
          val childGrad = op.derivatie(child.data, value.data) * grad
          addGrad(grads, child, childGrad)

        case NodeChildren.Two(child1, child2, op) =>
          val childGrad1 = op.partialDerivative1(child1.data, child2.data) * grad
          val childGrad2 = op.partialDerivative2(child1.data, child2.data) * grad

          addGrad(addGrad(grads, child1, childGrad1), child2, childGrad2)
    }

  def sortTopologically(value: Value): List[Value] =

    // todo: rewrite this in a tail-recursive way
    def go(
      value: Value,
      acc: List[Value],
      visited: Set[String],
    ): (List[Value], Set[String]) =
      if visited.contains(value.id) then (acc, visited)
      else
        val (newAcc, newVisited) = value.children match
          case NodeChildren.Empty => (acc, visited + value.id)
          case NodeChildren.One(child, _) => go(child, acc, visited + value.id)
          case NodeChildren.Two(child1, child2, _) =>
            val (acc1, visited1) = go(child1, acc, visited + value.id)
            go(child2, acc1, visited1)

        (value :: newAcc, newVisited)

    go(value, Nil, Set.empty)._1
