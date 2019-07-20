package rlp.ai

import rlp.math.Matrix
import rlp.storage.Storable
import ujson.Js

import scala.util.Random

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
) extends Storable {

  val numLayers = layerSizes.size

  require(numLayers >= 2, "Neural network requires input and output layer")
  require(activationFunctions.length == numLayers-1, "Invalid number of activation functions provided")
  require(lambda >= 0, "Regularisation rate must be non-negative")

  /* Create weight matrices representing each pair of consecutive layer connections */
  val weights: Array[Matrix] =
    for ((n1, n2) <- layerSizes.zip(layerSizes.tail)) yield new Matrix(n1+1, n2)

  /**
    * Clone the Neural Network, keeping the same architecture
    * but new set of weights
    *
    * @param cloneWeights Whether to copy weights across or reset to zero (default)
    *
    * @return The cloned network
    */
  def clone(cloneWeights: Boolean): NeuralNetwork = {
    val cloned = new NeuralNetwork(layerSizes, activationFunctions, useSoftMax, lambda)

    if (cloneWeights) {
      for (i <- weights.indices) cloned.weights(i) = weights(i).clone()
    }

    cloned
  }

  override def clone(): NeuralNetwork = clone(true)

  def forwardProp(activations: Array[Double]): Array[Double] = {
    forwardProp(new Matrix(1, activations.length, activations)).data
  }

  /**
    * Forward propagate the neural network to calculate output activations
    * @param inputActivations The input data, with rows representing different data values
    * @return The output activations, in a similar layout to input data
    */
  def forwardProp(inputActivations: Matrix): Matrix = {
    var alpha = inputActivations.clone()
    val ones = new Matrix(inputActivations.rows, 1) fillWith 1

    // Iterate, keeping track of current activations by applying each consecutive
    // weight then the corresponding activation function
    for ((w, phi) <- weights.zip(activationFunctions)) {
      alpha = (Matrix.concatCols(ones, alpha) *= w) each phi.apply
    }

    if (useSoftMax) Matrix.softMax(alpha) else alpha
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
    val m = input.rows
    val output = forwardProp(input)

    val reg: Double =
      if (lambda == 0) 0
      else {
        0.5 * lambda * weights.map(W => W.map(w => w*w).data.sum).sum
      }

    (0 until m)
      .map { i =>
        val losses =
          for (k <- 0 until output.cols) yield {
            if (useSoftMax) {
              -target(i, k)*math.log(output(i, k)) //+ (1-target(i,k))*math.log(1-output(i,k)) )
            } else {
              0.5 * math.pow(target(i, k) - output(i, k), 2)
            }
          }

        losses.sum + reg
      }
      .toArray
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
    val m = input.rows
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

    if (useSoftMax) activations(numLayers-1) = Matrix.softMax(activations(numLayers-1))

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

    val m = input.rows
    val gradients = new Array[Matrix](weights.length)
    val initialLoss = loss(input, target).sum / m

    for (i <- weights.indices) {
      gradients(i) = new Matrix(weights(i).rows, weights(i).cols)
      // Iterate over each single weight
      for (r <- 0 until weights(i).rows; c <- 0 until weights(i).cols) {

        // Deviate weight, and thus estimate how the loss changes with respect to current weight
        weights(i)(r, c) += epsilon
        gradients(i)(r, c) = (loss(input, target).sum / m - initialLoss) / epsilon
        weights(i)(r, c) -= epsilon
      }
    }

    gradients
  }

  /**
    * Reset the weights uniformly at random
    * @param min The minimum weight (default 0)
    * @param max The maximum weight (default 1)
    */
  def randomiseWeights(min: Double = 0, max: Double = 1): Unit = {
    for (w <- weights.indices) {
      for (r <- 0 until weights(w).rows) {
        for (c <- 0 until weights(w).cols) {
          weights(w)(r,c) = math.random() * (max - min) + min
        }
      }
    }
  }

  /** Initialise the weights of the layers using a random
    * heuristic for better convergence properties
    * @param seed An optional seed to uniquely define the initialisation
    */
  def initialiseWeights(seed: Long = System.currentTimeMillis()): Unit = {
    val rand = new Random(seed)
    for (w <- weights.indices) {
      val stdDev = math.sqrt(2.0 / layerSizes(w))
      for (r <- 0 until weights(w).rows) {
        for (c <- 0 until weights(w).cols) {
          weights(w)(r,c) = rand.nextGaussian() * stdDev
        }
      }
    }
  }

  def store(): Js.Value = {
    Js.Arr(weights.map(_.store()) :_* )
  }

  def load(data: Js.Value): Unit = {
    val xs = data.arr
    for (w <- weights.indices) {
      weights(w).load(xs(w))
    }
  }

  /**
    * Do simple gradient descent, not recommended
    * for real problems since an optimiser works much better
    *
    * @param inputs
    * @param targets
    * @param alpha
    * @param numEpochs
    */
  def train(inputs: Matrix, targets: Matrix, alpha: Double, numEpochs: Int): Unit ={
    for (_ <- 0 until numEpochs) {
      val grad = backProp(inputs, targets)
      for (i <- grad.indices) {
        weights(i) -= (grad(i) *= alpha)
      }
    }
  }
}
