package rlp.ai

import rlp.math.Matrix

/**
  * A dense, feed-forward neural network representation
  * with N layers
  *
  * @param layerSizes N integers specifying nodes per layer
  * @param activationFunctions N-1 activation functions
  * @param useSoftMax Whether to use squared error or soft-max with cross entropy loss
  * @param lambda The regularisation rate using the L2 regularisation method
  */
class NeuralNetwork(
  val layerSizes: Array[Int],
  val activationFunctions: Array[ActivationFunction],
  val useSoftMax: Boolean = false,
  val lambda: Double = 0
) {

  val numLayers = layerSizes.size

  require(numLayers >= 2, "Neural network requires input and output layer")
  require(activationFunctions.length == numLayers-1, "Invalid number of activation functions provided")
  require(lambda >= 0, "Regularisation rate must be non-negative")

  /* Create weight matrices representing each pair of consecutive layer connections */
  val weights: Array[Matrix] =
    for ((n1, n2) <- layerSizes.zip(layerSizes.tail)) yield new Matrix(n1+1, n2)

  def forwardProp(activations: Array[Double]): Array[Double] = {
    forwardProp(new Matrix(1, activations.length, activations)).getData()
  }

  /**
    * Forward propagate the neural network to calculate output activations
    * @param inputActivations The input data, with rows representing different data values
    * @return The output activations, in a similar layout to input data
    */
  def forwardProp(inputActivations: Matrix): Matrix = {
    var alpha = inputActivations
    val ones = new Matrix(inputActivations.getRows(), 1) fillWith 1

    // Iterate, keeping track of current activations by applying each consecutive
    // weight then the corresponding activation function
    for ((w, phi) <- weights.zip(activationFunctions)) {
      alpha = (Matrix.concatCols(ones, alpha) *= w) each phi.apply
    }

    if (useSoftMax) softMax(alpha) else alpha
  }

  /**
    * Compute the losses with respect to each input/output pair
    * given in the input
    *
    * @param input
    * @param target
    * @return
    */
  def loss(input: Matrix, target: Matrix): Array[Double] = {
    val m = input.getRows()
    val output = forwardProp(input)

    val reg: Double =
      if (lambda == 0) 0
      else {
        0.5 * lambda * weights.map(W => W map (w => w*w) getData() sum).sum
      }

    (0 until m)
      .map { i =>
        val losses =
          for (k <- 0 until output.getCols()) yield {
            if (useSoftMax) {
              -(target(i, k)*math.log(output(i, k)) + (1-target(i,k))*math.log(1-output(i,k)) )
            } else {
              0.5 * math.pow(target(i, k) - output(i, k), 2)
            }
          }

        losses.sum + reg
      }
      .toArray
  }

  // Utility function to apply the  softMax activation function
  def softMax(activations: Matrix): Matrix = {
    val res = new Matrix(activations.getRows(), activations.getCols())

    for (r <- 0 until activations.getRows()) {

      // Extract the maximum element before using math.exp for better numerical stability
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

  /**
    * Compute the average gradients of the weights with respect to the loss on
    * the inputs and targets specified
    *
    * @param input Input data with elements given across rows
    * @param target Target outputs with elements across rows
    * @return An array representing the ordered gradients of weights in each layer
    */
  def backProp(input: Matrix, target: Matrix): Array[Matrix] = {

    // Number of data samples
    val m = input.getRows()
    val ones = new Matrix(m, 1) fillWith 1

    val netInputs = new Array[Matrix](numLayers)
    val activations = new Array[Matrix](numLayers)
    val gradients = new Array[Matrix](numLayers-1)

    // Initially, do a forward pass whilst keeping track of inputs and activations in each layer
    activations(0) = input
    for (i <- 1 until numLayers) {
      netInputs(i) = Matrix.concatCols(ones, activations(i-1)) * weights(i-1)
      activations(i) = activationFunctions(i-1)(netInputs(i))
    }

    if (useSoftMax) activations(numLayers-1) = softMax(activations(numLayers-1))

    // Delta represents the partial derivatives of the current layer's total input
    // with respect to the losses across all input data
    // Initial delta is the same for the squared error and softmax with cross-entropy loss
    var delta = (activations(numLayers-1) - target) transposeSelf()

    // Iterate backwards over layers
    for (i <- (numLayers-2) to 0 by (-1)) {

      // Compute Hammard product with the activation function derivatives
      delta elemProductSelf activationFunctions(i).derivative(netInputs(i+1)).transposeSelf()

      // Calculate gradients, applying regularisation if needed
      gradients(i) = (delta * Matrix.concatCols(ones, activations(i))).transposeSelf() * (1.0 / m)
      if (lambda != 0) gradients(i) += weights(i) * lambda

      // Compute activation partial derivatives for the layer below
      delta = (weights(i) * delta) subMatrix (rowStart = 1)
    }

    gradients
  }

  /**
    * Numerical (slow) gradient calculation with respect to inputs and targets. Useful for
    * testing the derivatives calculated by back-propagation.
    *
    * @param input The input data in row order
    * @param target Output data in row order
    * @param epsilon The deviation used to estimate partial derivatives
    * @return Gradients for each weight, ordered by layer
    */
  def numericalGradient(input: Matrix, target: Matrix, epsilon: Double = 1e-5): Array[Matrix] = {

    val m = input.getRows()
    val gradients = new Array[Matrix](weights.length)
    val initialLoss = loss(input, target).sum / m

    for (i <- weights.indices) {
      gradients(i) = new Matrix(weights(i).getRows(), weights(i).getCols())
      // Iterate over each single weight
      for (r <- 0 until weights(i).getRows(); c <- 0 until weights(i).getCols()) {

        // Deviate weight, and thus estimate how the loss changes with respect to current weight
        weights(i)(r, c) += epsilon
        gradients(i)(r, c) = (loss(input, target).sum / m - initialLoss) / epsilon
        weights(i)(r, c) -= epsilon
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
