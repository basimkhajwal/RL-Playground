package rlp.ai.optimizers

import rlp.ai.NeuralNetwork
import rlp.math.Matrix

class SGDMomentum(
  network: NeuralNetwork,
  learningRate: Double = 0.01,
  momentum: Double = 0,
  decay: Double = 0,
  nesterov: Boolean = false
) extends NetworkOptimizer(network) {

  val v: Array[Matrix] = network.weights.map(w => new Matrix(w.getRows, w.getCols))
  val vPrev: Array[Matrix] = new Array[Matrix](v.length)

  override def step(gradient: Array[Matrix]): Unit = {
    for (i <- v.indices) {

      if (nesterov) {
        vPrev(i) = v(i)
      }

      v(i) *= momentum
      v(i) -= (gradient(i) * learningRate)

      if (nesterov) {
        network.weights(i) += v(i) * (1 + momentum) - vPrev(i) * momentum
      } else {
        network.weights(i) += v(i)
      }
    }
  }
}
