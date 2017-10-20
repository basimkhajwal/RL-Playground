package rlp.ai

import rlp.math.Matrix

/**
  *
  *
  * @param layerSizes
  * @param activationFunctions
  * @param useSoftMax
  */
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
    var alpha = inputActivations
    val ones = new Matrix(inputActivations.getRows(), 1) fill 1

    for ((w, phi) <- weights.zip(activationFunctions)) {
      alpha = (Matrix.concatCols(ones, alpha) *= w) each phi.apply
    }

    if (useSoftMax) softMax(alpha) else alpha
  }

  def loss(input: Matrix, target: Matrix): Array[Double] = {
    val m = input.getRows()
    val output = forwardProp(input)

    (0 until m)
      .map { i =>
        val losses =
          for (k <- 0 until output.getCols()) yield {
            if (useSoftMax) {
              target(i, k) * -math.log(output(i, k))
            } else {
              math.pow(target(i, k) - output(i, k), 2)
            }
          }

        losses.sum
      }
      .toArray
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

  def backProp(input: Matrix, target: Matrix): Array[Matrix] = {
    val m = input.getRows()
    val ones = new Matrix(m, 1) fill 1

    val netInputs = new Array[Matrix](numLayers)
    val activations = new Array[Matrix](numLayers)
    val gradients = new Array[Matrix](numLayers-1)

    activations(0) = input
    for (i <- 1 until numLayers) {
      netInputs(i) = Matrix.concatCols(ones, activations(i-1)) * weights(i-1)
      activations(i) = activationFunctions(i-1)(netInputs(i))
    }

    if (useSoftMax) {
      activations(numLayers-1) = softMax(activations(numLayers-1))
    }

    var delta = (activations(numLayers-1) - target) transposeSelf()

    for (i <- (numLayers-2) to 0 by (-1)) {

      delta elemProductSelf activationFunctions(i)(netInputs(i+1)).transposeSelf()

      gradients(i) = (delta * Matrix.concatCols(ones, activations(i))) transposeSelf()

      delta = (weights(i) * delta) subMatrix (rowStart = 1)
    }

    gradients
  }

  def numericalGradient(input: Matrix, target: Matrix, epsilon: Double = 1e-5): Array[Matrix] = {

    val gradients = new Array[Matrix](numLayers-1)
    val initialLoss = loss(input, target).sum

    for (i <- gradients.indices) {
      for (r <- 0 until weights(i).getRows()) {
        for (c <- 0 until weights(i).getCols()) {
          weights(i)(r, c) += epsilon
          val newLoss = loss(input, target).sum
          gradients(i)(r, c) = (newLoss - initialLoss) / epsilon
          weights(i)(r, c) -= epsilon
        }
      }
    }

    gradients
  }
}

sealed trait ActivationFunction {
  def apply(m: Matrix): Matrix = m map apply
  def apply(x: Double): Double

  def derivative(m: Matrix): Matrix = m map derivative
  def derivative(x: Double): Double
}

object ActivationFunction {

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
}
