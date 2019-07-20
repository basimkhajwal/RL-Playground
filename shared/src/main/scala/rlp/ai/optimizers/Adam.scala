package rlp.ai.optimizers

import rlp.ai.NeuralNetwork
import rlp.math.Matrix
import rlp.storage.Storable
import ujson.Js

class Adam(
  network: NeuralNetwork,
  val learningRate: Double = 0.001,
  val beta1: Double = 0.9,
  val beta2: Double = 0.999,
  val epsilon: Double = 1e-8,
  val decay: Double = 0
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

  override def store(): Js.Value = {
    Js.Obj(
      "t" -> Js.Num(t),
      "lr" -> Js.Num(lr),
      "m" -> Storable.store(m),
      "v" -> Storable.store(v)
    )
  }

  override def load(json: Js.Value): Unit = {
    val keyMap = json.obj

    t = keyMap("t").num.toInt
    lr = keyMap("lr").num

    Storable.load(m, keyMap("m"))
    Storable.load(v, keyMap("v"))
  }
}
