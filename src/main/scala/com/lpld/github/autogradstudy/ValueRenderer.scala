package com.lpld.github.autogradstudy

import scala.collection.mutable
import org.graphstream.graph.*
import org.graphstream.graph.implementations.SingleGraph
import java.util.UUID

object ValueRenderer:
  def renderGraph(value: Value, grads: Map[String, Double]): Unit =

    System.setProperty("org.graphstream.ui", "swing");

    val graph: Graph = new SingleGraph("TEst")

    graph.setAttribute(
      "ui.stylesheet",
      """
     |node {
     | shape: box;
     | fill-color: white;
     | stroke-mode: plain;
     | stroke-color: black;
     | size: 250px, 40px;
     | text-size: 15;
     |}
     |""".stripMargin,
    );
    println("Rendering graph")

    val nodes = mutable.Set.empty[String]
    val edges = mutable.Set.empty[(String, String)]

    def traverseValues(value: Value, parent: Option[Value]): Unit =

      if !nodes.contains(value.id) then
        val label1 =
          if value.label.isEmpty() then value.data
          else s"${value.label}: ${value.data}"

        val label2 = value.children match
          case NodeChildren.Empty => ""
          case _ => " = " + value.describeChildren

        val grad = grads(value.id)
        val label3 = s" | grad: $grad"
        nodes.add(value.id)
        graph
          .addNode(value.id)
          .nn
          .setAttribute("ui.label", s"$label1$label2$label3")

      parent.foreach { p =>
        if !edges.contains((p.id, value.id)) then
          edges.add((p.id, value.id))
          graph.addEdge(
            UUID.randomUUID().nn.toString,
            value.id,
            p.id,
            true,
          )
      }

      value.children match
        case NodeChildren.Empty => ()
        case NodeChildren.One(child, _) =>
          traverseValues(child, Some(value))
        case NodeChildren.Two(child1, child2, _) =>
          traverseValues(child1, Some(value))
          traverseValues(child2, Some(value))

    traverseValues(value, None)
    graph.display()
