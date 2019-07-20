package rlp.ai.optimizers

import rlp.ai.NeuralNetwork
import rlp.math.Matrix
import rlp.storage.Storable
import ujson.Js

class SGDMomentum(
  network: NeuralNetwork,
  val learningRate: Double = 0.01,
  val momentum: Double = 0,
  val decay: Double = 0,
  val nesterov: Boolean = false
) extends NetworkOptimizer(network) {

  val v: Array[Matrix] = network.weights.map(w => new Matrix(w.rows, w.cols))
  val vPrev: Array[Matrix] = v map (_.clone())

  var lr: Double = learningRate

  override def step(gradient: Array[Matrix]): Unit = {
    for (i <- v.indices) {

      if (nesterov) {
        vPrev(i) = v(i).clone()
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

  override def store(): Js.Value = {
    Js.Obj(
      "lr" -> Js.Num(lr),
      "v" -> Storable.store(v),
      "vPrev" -> Storable.store(vPrev)
    )
  }

  override def load(json: Js.Value): Unit = {
    val keyMap = json.obj
    lr = keyMap("lr").num
    Storable.load(v, keyMap("v"))
    Storable.load(vPrev, keyMap("vPrev"))
  }
}
