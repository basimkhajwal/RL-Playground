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

    nn.train(input, output, 1e-3, 10000)

    nn.forwardProp(Array(0.5))(0) shouldEqual (4.0 +- 0.05)
  }

  test("Backprop works roughly correctly") {

    val networks = List(
      new NeuralNetwork(Array(10, 11, 5), Array(Sigmoid, ReLU)),
      new NeuralNetwork(Array(10, 10, 3, 5), Array(ReLU, Sigmoid, Linear)),
      new NeuralNetwork(Array(10, 9, 3, 5), Array(ReLU, Sigmoid, Linear), true),
      //new NeuralNetwork(Array(10, 8, 2, 5), Array(ReLU, Sigmoid, Linear), false, 10000),
      //new NeuralNetwork(Array(10, 7, 5), Array(Sigmoid, Sigmoid), true, 100),
      //new NeuralNetwork(Array(10, 6, 5), Array(ReLU, ReLU), false, 0.01),
    )

    for (_ <- 0 until 10) {

      val input = new Matrix(20, 10) each { _ => Math.random() }
      val output = new Matrix(20, 5) each { _ => Math.random() }

      for (n <- networks) {
        n.randomiseWeights(-0.4,0.4)

        val backPropGrad = n.backProp(input, output)
        val numericalGrad = n.numericalGradient(input, output, 1e-9)

        for ((b, n) <- (backPropGrad, numericalGrad).zipped) {
          for ((d1, d2) <- (b.getData(), n.getData()).zipped) {
            d1 shouldEqual (d2 +- Math.max(Math.abs(d2 * 0.01), 0.1))
          }
        }
      }

    }
  }
}
