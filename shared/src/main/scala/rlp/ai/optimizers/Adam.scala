package rlp.ai.optimizers

import rlp.ai.NeuralNetwork
import rlp.math.Matrix

class Adam(
  network: NeuralNetwork,
  learningRate: Double = 0.001,
  beta1: Double = 0.9,
  beta2: Double = 0.999,
  epsilon: Double = 1e-8,
  decay: Double = 0
) extends NetworkOptimizer(network) {

  val m: Array[Matrix] = network.weights.map(w => new Matrix(w.rows, w.cols))
  val v: Array[Matrix] = network.weights.map(w => new Matrix(w.rows, w.cols))

  var lr: Double = learningRate
  var t: Int = 1

  override def step(gradient: Array[Matrix]): Unit = {

    for (i <- gradient.indices) {
      for (j <- gradient(i).data.indices) {
        val g = gradient(i)(j)

        m(i)(j) = beta1 * m(i)(j) + (1 - beta1) * g
        v(i)(j) = beta2 * v(i)(j) + (1 - beta2) * g * g

        val mt = m(i)(j) / (1 - math.pow(beta1, t))
        val vt = v(i)(j) / (1 - math.pow(beta2, t))

        network.weights(i)(j) -= lr * mt / math.sqrt(vt + epsilon)
      }
    }

    lr *= (1 - decay)
    t += 1
  }

}
