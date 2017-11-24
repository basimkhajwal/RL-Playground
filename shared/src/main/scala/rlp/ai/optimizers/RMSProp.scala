package rlp.ai.optimizers

import rlp.ai.NeuralNetwork
import rlp.math.Matrix

class RMSProp(
  network: NeuralNetwork,
  learningRate: Double,
  rho: Double,
) extends NetworkOptimizer(network) {

  val v: Array[Matrix] =
    network.weights.map(w => new Matrix(w.getRows, w.getCols))

  override def step(gradient: Array[Matrix]): Unit = {
    for (i <- v.indices) {
      for (j <- gradient(i).getData().indices) {

        val g = gradient(i)(j)

        v(i)(j) = rho * v(i)(j) + (1 - rho) * g * g

        network.weights(i)(j) -= learningRate * g / math.sqrt(v(i)(j))
      }
    }
  }
}

