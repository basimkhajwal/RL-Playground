package rlp.ai.optimizers

import rlp.ai.NeuralNetwork
import rlp.math.Matrix
import rlp.storage.Storable
import ujson.Js

abstract class NetworkOptimizer(val network: NeuralNetwork) extends Storable {

  def step(input: Matrix, target: Matrix): Unit = {
    val gradients = network.backProp(input, target)
    step(gradients)
  }

  def step(gradient: Array[Matrix]): Unit

  def store(): Js.Value

  def load(json: Js.Value): Unit
}

object NetworkOptimizer {

  def store(opt: NetworkOptimizer): Js.Value = {
    opt match {
      case a : Adam =>
        Js.Obj(
          "type" -> Js.Str("adam"),
          "learningRate" -> Js.Num(a.learningRate),
          "beta1" -> Js.Num(a.beta1),
          "beta2" -> Js.Num(a.beta2),
          "epsilon" -> Js.Num(a.epsilon),
          "decay" -> Js.Num(a.decay),
          "data" -> a.store()
        )

      case r : RMSProp =>
        Js.Obj(
          "type" -> Js.Str("rmsprop"),
          "learningRate" -> Js.Num(r.learningRate),
          "rho" -> Js.Num(r.rho),
          "epsilon" -> Js.Num(r.epsilon),
          "decay" -> Js.Num(r.decay),
          "data" -> r.store()
        )

      case s : SGDMomentum =>
        Js.Obj(
          "type" -> Js.Str("sgd"),
          "learningRate" -> Js.Num(s.learningRate),
          "momentum" -> Js.Num(s.momentum),
          "decay" -> Js.Num(s.decay),
          "nesterov" -> (if (s.nesterov) Js.True else Js.False),
          "data" -> s.store()
        )

      case _ => throw new NotImplementedError("Undefined optimiser type!")
    }
  }

  def create(network: NeuralNetwork, optData: Js.Value): NetworkOptimizer = {
    val keyMap = optData.obj

    val opt = keyMap("type").str match {
      case "adam" =>
        new Adam(network,
          keyMap("learningRate").num,
          keyMap("beta1").num,
          keyMap("beta2").num,
          keyMap("epsilon").num,
          keyMap("decay").num,
        )

      case "rmsprop" =>
        new RMSProp(network,
          keyMap("learningRate").num,
          keyMap("rho").num,
          keyMap("epsilon").num,
          keyMap("decay").num,
        )

      case "sgd" =>
        new SGDMomentum(network,
          keyMap("learningRate").num,
          keyMap("momentum").num,
          keyMap("decay").num,
          keyMap("nesterov") == Js.True,
        )

      case t => throw new NotImplementedError(s"Undefined optimiser type $t")
    }

    opt.load(keyMap("data"))

    opt
  }
}