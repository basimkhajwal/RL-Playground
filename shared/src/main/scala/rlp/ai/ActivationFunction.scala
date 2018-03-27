package rlp.ai

import rlp.math.Matrix

/**
  * Represents a the activation function and it's derivative
  * applied to each neuron's summed input in a layer
  */
sealed trait ActivationFunction {
  def apply(m: Matrix): Matrix = m map apply
  def apply(x: Double): Double

  def derivative(m: Matrix): Matrix = m map derivative
  def derivative(x: Double): Double
}

object ActivationFunction {

  /**
    * Implementations of the most popular
    * activation functions
    */

  object Linear extends ActivationFunction {
    override def apply(x: Double): Double = x
    override def derivative(x: Double): Double = 1
  }

  object ReLU extends ActivationFunction {
    override def apply(x: Double) = if (x < 0) 0 else x
    override def derivative(x: Double) = if (x < 0) 0 else 1
  }

  object Sigmoid extends ActivationFunction {
    override def apply(x: Double) = 1.0 / (1.0 + math.exp(-x))
    override def derivative(x: Double) = {
      val sigmoid = apply(x)
      sigmoid * (1 - sigmoid)
    }
  }

  object TanH extends ActivationFunction {
    override def apply(x: Double) = math.tanh(x)
    override def derivative(x: Double) = {
      val t = math.tanh(x)
      1 - t*t
    }
  }
}
