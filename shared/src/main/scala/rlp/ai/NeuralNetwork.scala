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

  def forwardProp(activations: Array[Double]): Array[Double] = {
    forwardProp(new Matrix(1, activations.length, activations)).getData()
  }

  def forwardProp(inputActivations: Matrix): Matrix = {
    var act = inputActivations
    val ones = new Matrix(inputActivations.getRows(), 1) fill 1
    for ((w, phi) <- weights.zip(activationFunctions)) {
      act = (Matrix.concatCols(ones, act) * w) each phi.apply
    }
    if (useSoftMax) softMax(act) else act
  }

  def softMax(activations: Matrix): Matrix = {
    val res = new Matrix(activations.getRows(), activations.getCols())

    for (r <- 0 until activations.getRows()) {
      var maxElem = 0.0
      for (c <- 0 until activations.getCols()) maxElem = math.max(maxElem, activations(r,c))

      var total = 0.0
      for (c <- 0 until activations.getCols()) total += math.exp(activations(r, c) - maxElem)

      for (c <- 0 until activations.getCols()) {
        res(r,c) = math.exp(activations(r, c) - maxElem) / total
      }
    }

    res
  }

  def backProp(inputActivations: Matrix, outputActivations: Matrix): Array[Matrix] = {
    val ones = new Matrix(inputActivations.getRows(), 1) fill 1

    val activations = new Array[Matrix](numLayers)
    val deltas = new Array[Matrix](numLayers)
    val gradients = new Array[Matrix](numLayers)

    activations(0) = inputActivations
    for (i <- 1 until numLayers) {
      activations(1) =
        (Matrix.concatCols(ones, activations(i-1)) * weights(i-1))
          .each(activationFunctions(i-1).apply)
    }

    // TODO: Check for softmax and apply its derivatives
    deltas(numLayers-1) = activations(numLayers-1) - outputActivations

    for (i <- (numLayers-2) to (0) by (-1) ) {
      // TODO: complete backprop
    }

    gradients
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

