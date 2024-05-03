package com.lpld.github.autogradstudy

case class Value(
  data: Double,
  children: NodeChildren = NodeChildren.Empty,
  label: String = "",
  id: String = java.util.UUID.randomUUID().toString
):


  def describeChildren: String = children match
    case NodeChildren.Empty => "<>"
    case NodeChildren.One(child, op) => op.desc(child.data)
    case NodeChildren.Two(child1, child2, op) => op.desc(child1.data, child2.data)

  def @@(label: String) = this.copy(label = label)

  def applyUnaray(op: UnaryOp, label: String = ""): Value = 
    Value(
      data = op.applyOp(this.data),
      children = NodeChildren.One(this, op),
      label = label
    )

  def applyBinary(other: Value, op: BinaryOp, label: String = ""): Value = 
    Value(
      data = op.applyOp(this.data, other.data),
      children = NodeChildren.Two(this, other, op),
      label = label
    )

  def +(other: Value): Value = this.applyBinary(other, BinaryOp.plus)

  def -(other: Value): Value = this + (-other)

  def *(other: Value): Value = this.applyBinary(other, BinaryOp.mult)

  def /(other: Value): Value = this * (other ** -1)

  def **(exponent: Double): Value = this.applyUnaray(UnaryOp.pow(exponent))

  def relu(): Value = this.applyUnaray(UnaryOp.relu)

  def tanh(): Value = this.applyUnaray(UnaryOp.tanh)

  def unary_- : Value = this * Value(-1.0)

enum NodeChildren:
  case Empty
  case One(child: Value, op: UnaryOp)
  case Two(child1: Value, child2: Value, op: BinaryOp)

abstract class Op(name: String):
  override def toString(): String = s"<$name>"

abstract class UnaryOp(name: String) extends Op(name):
  def applyOp(data: Double): Double
  def desc(data: Double): String
  def derivatie(data: Double, applied: Double): Double

object UnaryOp:
  val neg = new UnaryOp("neg"):
    def applyOp(data: Double): Double = -data
    def desc(data: Double): String = s"-$data"
    def derivatie(data: Double, applied: Double): Double = -1.0

  val exp = new UnaryOp("exp"):
    def applyOp(data: Double): Double = math.exp(data)
    def desc(data: Double): String = s"exp($data)"
    def derivatie(data: Double, applied: Double): Double = applied

  def pow(exponent: Double) = new UnaryOp(s"pow($exponent)"):
    def applyOp(data: Double): Double = math.pow(data, exponent)
    def desc(data: Double): String = s"$data^$exponent"
    def derivatie(data: Double, applied: Double): Double = exponent * math.pow(data, exponent - 1)

  val sqr = new UnaryOp("sqr"):
    def applyOp(data: Double): Double = data * data
    def desc(data: Double): String = s"$data^2"
    def derivatie(data: Double, applied: Double): Double = 2 * data

  val tanh = new UnaryOp("tanh"):
    def applyOp(data: Double): Double = math.tanh(data)
    def desc(data: Double): String = s"tanh($data)"
    def derivatie(data: Double, applied: Double): Double = 1 - applied * applied

  val relu = new UnaryOp("relu"):
    def applyOp(data: Double): Double = math.max(0, data)
    def desc(data: Double): String = s"relu($data)"
    def derivatie(data: Double, applied: Double): Double = if data > 0 then 1.0 else 0.0

abstract class BinaryOp(name: String, label: String) extends Op(name):

  def applyOp(data1: Double, data2: Double): Double
  def desc(data1: Double, data2: Double): String = s"$data1 $label $data2"

  def partialDerivative1(data1: Double, data2: Double): Double
  def partialDerivative2(data1: Double, data2: Double): Double

object BinaryOp:
  val plus = new BinaryOp("plus", "+"):
    def applyOp(data1: Double, data2: Double): Double = data1 + data2
    def partialDerivative1(data1: Double, data2: Double): Double = 1.0
    def partialDerivative2(data1: Double, data2: Double): Double = 1.0

  val mult = new BinaryOp("mult", "*"):
    def applyOp(data1: Double, data2: Double): Double = data1 * data2
    def partialDerivative1(data1: Double, data2: Double): Double = data2
    def partialDerivative2(data1: Double, data2: Double): Double = data1
