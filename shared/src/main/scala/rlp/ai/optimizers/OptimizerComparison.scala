package rlp.ai.optimizers

import rlp.ai.NeuralNetwork
import rlp.math.Matrix

object OptimizerComparison {

  def main(args: Array[String]): Unit = {
    val networkA, networkB, networkC = new NeuralNetwork(Array(2, 5, 5, 3, 1), Array(Sigmoid, Sigmoid, Sigmoid, ReLU))

    networkA.randomiseWeights()
    for (i <- networkA.weights.indices) {
      networkB.weights(i) = new Matrix(networkA.weights(i))
      networkC.weights(i) = new Matrix(networkA.weights(i))
    }

    val learningRate = 0.1
    val momentumRate = 0.7

    val sgd = new SGDMomentum(networkA, learningRate, 0)
    val sgdMomentum = new SGDMomentum(networkB, learningRate, momentumRate)
    val rmsProp = new RMSProp(networkC, 0.9, 0.005)

    val dataSamples = 200
    val function = (x:Double, y:Double) => 4*x+2*y

    val inputData = new Matrix(dataSamples, 2) each (_ => math.random())
    val targets = new Matrix(dataSamples, 1,
      (0 until dataSamples) map (i => function(inputData(i,0),inputData(i,1))) toArray)

    println("Epoch\t\tSGD\t\t\t\tSGD+Momentum\t\t\t\tRMS Prop")
    for (i <- 0 until 2000) {
      sgd.step(inputData, targets)
      sgdMomentum.step(inputData, targets)
      rmsProp.step(inputData, targets)

      if (i % 20 == 0) {
        val lossA = networkA.loss(inputData, targets).sum
        val lossB = networkB.loss(inputData, targets).sum
        val lossC = networkC.loss(inputData, targets).sum
        println(s"$i\t\t$lossA\t\t$lossB\t\t$lossC")
      }
    }
  }
}
