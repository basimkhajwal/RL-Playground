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

    val input = new Matrix(100, 1, (0 until 100) map (_ / 100.0) toArray)
    val output = input map { 2*_ + 3}

    nn.train(input, output, 5e-7, 10000)

    nn.forwardProp(Array(0.5))(0) shouldEqual (4.0 +- 0.001)
  }

  test("Backprop works roughly correctly") {
    val nn = new NeuralNetwork(Array(10, 10, 4, 1), Array(ReLU, Sigmoid, Linear))
    nn.randomiseWeights(-0.4,0.4)

    val input = new Matrix(1, 10) each { _ => Math.random() }
    val output = new Matrix(1, 1) each { _ => Math.random() }

    val backPropGrad = nn.backProp(input, output)
    val numericalGrad = nn.numericalGradient(input, output)
    val pairedGrad = (backPropGrad, numericalGrad).zipped

    val a1 = pairedGrad.map((a,b) => (a - b).getData().map(x=>x*x).sum).sum
    val a2 = pairedGrad.map((a,b) => (a + b).getData().map(x=>x*x).sum).sum

    println(a1)
    println(a2)

    val f1 = backPropGrad(0)
    val f2 = numericalGrad(0)

    println(f1)
    println(f2)
    println(f2 elemProduct (f1 map (1.0 / _)))

    require(pairedGrad.forall((a,b) => (a elemProduct b).getData().forall(_ >= 0)), "All gradients must have the same sign!")

    require((a1/a2) < 1, "Backprop should match signs of numerical gradient")
  }
}
