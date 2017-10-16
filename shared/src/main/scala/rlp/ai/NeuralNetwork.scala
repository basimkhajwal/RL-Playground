package rlp.ai

class NeuralNetwork {

}

trait NetworkLayer {
  def forward(activations: Array[Double]): Array[Double]
  def backward(deltas: Array[Double]): Array[Double]
}

sealed trait ActivationFunction {
  def apply(x: Double): Double
  def derivative(x: Double): Double
}
object ReLU extends ActivationFunction {
  override def apply(x: Double) = if (x < 0) 0 else x
  override def derivative(x: Double) = if (x < 0) 0 else 1
}

class DenseLayer(val activationFunction: ActivationFunction) extends NetworkLayer {

  override def forward(activations: Array[Double]) = ???

  override def backward(deltas: Array[Double]) = ???
}