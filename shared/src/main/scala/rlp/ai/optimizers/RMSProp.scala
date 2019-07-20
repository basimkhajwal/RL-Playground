package rlp.ai.optimizers

import rlp.ai.NeuralNetwork
import rlp.math.Matrix
import rlp.storage.Storable
import ujson.Js

class RMSProp(
  network: NeuralNetwork,
  val learningRate: Double = 0.001,
  val rho: Double = 0.9,
  val epsilon: Double = 1e-8,
  val decay: Double = 0,
) extends NetworkOptimizer(network) {

  val v: Array[Matrix] = network.weights.map(w => new Matrix(w.rows, w.cols))

  var lr: Double = learningRate

  override def step(gradient: Array[Matrix]): Unit = {
    for (i <- v.indices) {
      for (j <- v(i).data.indices) {
        val g = gradient(i)(j)

        v(i)(j) = rho * v(i)(j) + (1 - rho) * g * g

        network.weights(i)(j) -= lr * g / (math.sqrt(v(i)(j)) + epsilon)
      }
    }

    lr *= (1 - decay)
  }

  override def store(): Js.Value = {
    Js.Obj(
      "lr" -> Js.Num(learningRate),
      "v" -> Storable.store(v)
    )
  }

  override def load(json: Js.Value): Unit = {
    val keyMap = json.obj
    lr = keyMap("lr").num
    Storable.load(v, keyMap("v"))
  }
}

