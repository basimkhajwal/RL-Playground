package rlp.ai

import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.PropertyChecks
import rlp.ai.ActivationFunction.{Linear, ReLU}
import rlp.math.Matrix

class NeuralNetworkSuite extends FunSuite with PropertyChecks with Matchers {

  test("Multi-layer ReLU network converges properly") {
    val nn = new NeuralNetwork(Array(1, 3, 3, 3, 3, 1), Array(ReLU, ReLU, ReLU, ReLU, Linear))
    nn.randomiseWeights()

    val input = new Matrix(100, 1, (0 until 100) map (_ / 100.0) toArray)
    val output = input map { 2*_ + 3}

    nn.train(input, output, 5e-6, 10000)

    nn.forwardProp(Array(0.5))(0) shouldEqual (4.0 +- 0.1)
  }
}
