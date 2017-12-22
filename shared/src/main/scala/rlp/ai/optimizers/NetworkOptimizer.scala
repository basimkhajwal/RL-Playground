package rlp.ai.optimizers

import rlp.ai.NeuralNetwork
import rlp.math.Matrix

abstract class NetworkOptimizer(val network: NeuralNetwork) {

  def step(input: Matrix, target: Matrix): Unit = {
    val gradients = network.backProp(input, target)
    step(gradients)
  }

  def step(gradient: Array[Matrix]): Unit
}

object NetworkOptimizer {



}