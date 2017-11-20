package rlp.ai

import rlp.math.Matrix

/**
  * A dense, feed-forward neural network representation
  * with N layers
  *
  * @param layerSizes N integers specifying nodes per layer
  * @param activationFunctions N-1 activation functions
  * @param useSoftMax Whether to use squared error or soft-max with cross entropy loss
  */
class NeuralNetwork(
  val layerSizes: Array[Int],
  val activationFunctions: Array[ActivationFunction],
  val useSoftMax: Boolean = false
) {

  val numLayers = layerSizes.size

  require(numLayers >= 2, "Neural network requires input and output layer")
  require(activationFunctions.length == numLayers-1, "Invalid number of activation functions provided")

  val weights: Array[Matrix] =
    for ((n1, n2) <- layerSizes.zip(layerSizes.tail)) yield new Matrix(n1+1, n2)

  def forwardProp(activations: Array[Double]): Array[Double] = {
    forwardProp(new Matrix(1, activations.length, activations)).getData()
  }

  def forwardProp(inputActivations: Matrix): Matrix = {
    var alpha = inputActivations
    val ones = new Matrix(inputActivations.getRows(), 1) fillWith 1

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
              0.5 * math.pow(target(i, k) - output(i, k), 2)
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
    val ones = new Matrix(m, 1) fillWith 1

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

      delta elemProductSelf activationFunctions(i).derivative(netInputs(i+1)).transposeSelf()

      gradients(i) = (delta * Matrix.concatCols(ones, activations(i))) transposeSelf()

      delta = (weights(i) * delta) subMatrix (rowStart = 1)
    }

    gradients
  }

  def numericalGradient(input: Matrix, target: Matrix, epsilon: Double = 1e-5): Array[Matrix] = {

    val gradients = new Array[Matrix](weights.length)
    val initialLoss = loss(input, target).sum

    for (i <- weights.indices) {
      gradients(i) = new Matrix(weights(i).getRows(), weights(i).getCols())
      for (r <- 0 until weights(i).getRows()) {
        for (c <- 0 until weights(i).getCols()) {
          weights(i)(r, c) += epsilon
          gradients(i)(r, c) = (loss(input, target).sum - initialLoss) / epsilon
          weights(i)(r, c) -= epsilon
        }
      }
    }

    gradients
  }

  def randomiseWeights(min: Double = 0, max: Double = 1): Unit = {
    for (w <- weights.indices) {
      for (r <- 0 until weights(w).getRows()) {
        for (c <- 0 until weights(w).getCols()) {
          weights(w)(r,c) = math.random() * (max - min) + min
        }
      }
    }
  }

  // Very simple training method for now
  // TODO: Implement RMSProp and proper batching
  def train(inputs: Matrix, targets: Matrix, alpha: Double, numEpochs: Int): Unit ={
    for (_ <- 0 until numEpochs) {
      val grad = backProp(inputs, targets)
      for (i <- grad.indices) {
        weights(i) -= (grad(i) *= alpha)
      }
    }
  }
}

