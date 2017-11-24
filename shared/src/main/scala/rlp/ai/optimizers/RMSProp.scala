package rlp.ai.optimizers

import rlp.ai.NeuralNetwork
import rlp.math.Matrix

class RMSProp(
  network: NeuralNetwork,
  learningRate: Double = 0.001,
  rho: Double = 0.9,
  epsilon: Double = 1e-8,
  decay: Double = 0,
) extends NetworkOptimizer(network) {

  val v: Array[Array[Double]] =
    network.weights.map(w => new Array[Double](w.rows * w.cols))

  var lr: Double = learningRate

  override def step(gradient: Array[Matrix]): Unit = {
    for (i <- v.indices) {
      for (j <- v(i).indices) {
        val g = gradient(i)(j)

        v(i)(j) = rho * v(i)(j) + (1 - rho) * g * g

        network.weights(i)(j) -= lr * g / (math.sqrt(v(i)(j)) + epsilon)
      }
    }

    lr *= (1 - decay)
  }
}

