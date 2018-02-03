package rlp.ai

import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.PropertyChecks
import rlp.ai.ActivationFunction.{Linear, ReLU, Sigmoid}
import rlp.math.Matrix

class NeuralNetworkSuite extends FunSuite with PropertyChecks with Matchers {

  import rlp._

  test("Multi-layer ReLU network converges properly") {
    val nn = new NeuralNetwork(Array(1, 3, 3, 3, 3, 1), Array(ReLU, ReLU, ReLU, ReLU, Linear))
    nn.initialiseWeights()

    val input = new Matrix(50, 1, (0 until 50) map (_ / 50.0) toArray)
    val output = input map { 2*_ + 3}

    nn.train(input, output, 1e-3, 5000)

    nn.forwardProp(Array(0.5))(0) shouldEqual (4.0 +- 0.05)
  }

  test("Backprop works roughly correctly") {

    val networks = List(
      new NeuralNetwork(Array(10, 11, 5), Array(Sigmoid, ReLU)),
      new NeuralNetwork(Array(2, 10, 3, 5), Array(ReLU, Sigmoid, Linear), false, 10000),
      new NeuralNetwork(Array(5, 9, 3, 3), Array(ReLU, Sigmoid, Linear), false, 100),
      new NeuralNetwork(Array(3, 8, 2, 7), Array(Sigmoid, ReLU, Linear), true, 10000),
      new NeuralNetwork(Array(1, 7, 5), Array(Sigmoid, Sigmoid), true, 100),
      new NeuralNetwork(Array(9, 6, 5), Array(ReLU, ReLU), true, 0.01),
    )

    for (_ <- 0 until 10) {
      for (n <- networks) {

        val input = new Matrix(20, n.layerSizes(0)) each { _ => Math.random() }

        val outputMat = new Matrix(20, n.layerSizes.last) each { _ => Math.random() }
        val output = if (n.useSoftMax) Matrix.softMax(outputMat) else outputMat

        n.randomiseWeights(-0.4,0.4)

        val backPropGrads = n.backProp(input, output)
        val numericalGrads = n.numericalGradient(input, output, 1e-9)

        for ((a, b) <- (backPropGrads, numericalGrads).zipped) {
          a shouldEqual b
        }
      }

    }
  }
}
