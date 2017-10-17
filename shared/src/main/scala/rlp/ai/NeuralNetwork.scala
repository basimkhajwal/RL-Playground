package rlp.ai

import rlp.math.Matrix

class NeuralNetwork(
  val layerSizes: Array[Int],
  val activationFunctions: Array[ActivationFunction],
  val useSoftMax: Boolean = false
) {

  val numLayers = layerSizes.size

  assert(numLayers >= 2, "Neural network requires input and output layer")
  assert(layerSizes.length-1 == activationFunctions.length, "Invalid number of activation functions provided")

  val weights: Array[Matrix] =
    for ((n1, n2) <- layerSizes.zip(layerSizes.tail)) yield new Matrix(n1+1, n2)

  def forward(activations: Array[Double]): Array[Double] = {
    forward(new Matrix(1, activations.length, activations)).getData()
  }

  def forward(activations: Matrix): Matrix = {
    var act = activations
    val ones = new Matrix(activations.getRows(), 1) fill 1

    for ((w, phi) <- weights.zip(activationFunctions)) {
      act = Matrix.concatCols(ones, act)
      act = (act*w).each(phi.apply)
    }

    act
  }
}

sealed trait ActivationFunction {
  def apply(x: Double): Double
  def derivative(x: Double): Double
}

object Linear extends ActivationFunction {
  override def apply(x: Double): Double = x
  override def derivative(x: Double): Double = 1
}

object ReLU extends ActivationFunction {
  override def apply(x: Double) = if (x < 0) 0 else x
  override def derivative(x: Double) = if (x < 0) 0 else 1
}

