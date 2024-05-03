package com.lpld.github
package autogradstudy

@main def Main(args: String*): Unit =
  val a = Value(-4.0) @@ "a"
  val b = Value(2.0) @@ "b"
  var c = a + b
  var d = a * b + b**3
  c = c + c + Value(1.0)
  c = (c + Value(1.0) + c + (-a)) @@ "c"
  d = d + d * Value(2) + (b + a).relu()
  d = (d + Value(3.0) * d + (b - a).relu()) @@ "d"
  val e = (c - d) @@ "e"
  val f = (e**2) @@ "f"
  var g = f / Value(2.0)
  g = (g + Value(10.0) / f) @@ "g"

  val grads = Backpropagation.computeGradient(g)

  println(grads(c.id))
  println(grads(a.id))
  println(grads(b.id))

  ValueRenderer.renderGraph(g, grads)
