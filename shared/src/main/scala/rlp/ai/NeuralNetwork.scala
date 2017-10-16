package rlp.ai

class NeuralNetwork(
  val layerSizes: List[Int],
  val activationFunctions: List[ActivationFunction],
  val useSoftMax: Boolean = false
) {

  assert(layerSizes.length >= 2, "Neural network requires input and output layer")
  assert(layerSizes.length-1 == activationFunctions.length, "Invalid number of activation functions provided")

  
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

