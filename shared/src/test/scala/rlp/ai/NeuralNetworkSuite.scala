package rlp.ai

import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.PropertyChecks
import rlp.ai.ActivationFunction.{Linear, ReLU, Sigmoid}
import rlp.math.Matrix

class NeuralNetworkSuite extends FunSuite with PropertyChecks with Matchers {

  import rlp._

  test("Multi-layer ReLU network converges properly") {
    val nn = new NeuralNetwork(Array(1, 3, 3, 3, 3, 1), Array(ReLU, ReLU, ReLU, ReLU, Linear))
    nn.randomiseWeights(-0.4,0.4)

    val input = new Matrix(50, 1, (0 until 50) map (_ / 50.0) toArray)
    val output = input map { 2*_ + 3}

    nn.train(input, output, 1e-5, 10000)

    nn.forwardProp(Array(0.5))(0) shouldEqual (4.0 +- 0.05)
  }

  test("Backprop works roughly correctly") {
    val nn = new NeuralNetwork(Array(10, 10, 4, 1), Array(ReLU, Sigmoid, Linear))
    nn.randomiseWeights(-0.4,0.4)

    val input = new Matrix(20, 10) each { _ => Math.random() }
    val output = new Matrix(20, 1) each { _ => Math.random() }

    val backPropGrad = nn.backProp(input, output)
    val numericalGrad = nn.numericalGradient(input, output)

    for ((b, n) <- (backPropGrad, numericalGrad).zipped) {
      for ((d1, d2) <- (b.getData(), n.getData()).zipped) {
        require(d1 === (d2 +- Math.max(Math.abs(d2 * 1e-3), 1e-4)), "Backprop gradient does not match numerical gradient")
      }
    }
  }
}
