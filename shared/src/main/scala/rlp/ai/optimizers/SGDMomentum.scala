package rlp.ai.optimizers

import rlp.ai.NeuralNetwork
import rlp.math.Matrix

class SGDMomentum(
  network: NeuralNetwork,
  val learningRate: Double = 0.01,
  val momentum: Double = 0,
  val decay: Double = 0,
  val nesterov: Boolean = false
) extends NetworkOptimizer(network) {

  val v: Array[Matrix] = network.weights.map(w => new Matrix(w.rows, w.cols))
  val vPrev: Array[Matrix] = new Array[Matrix](v.length)

  var lr: Double = learningRate

  override def step(gradient: Array[Matrix]): Unit = {
    for (i <- v.indices) {

      if (nesterov) {
        vPrev(i) = v(i)
      }

      v(i) *= momentum
      v(i) -= (gradient(i) * lr)

      if (nesterov) {
        network.weights(i) += v(i) * (1 + momentum) - vPrev(i) * momentum
      } else {
        network.weights(i) += v(i)
      }
    }

    lr *= (1 - decay)
  }
}
