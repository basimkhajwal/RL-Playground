package rlp.ai.optimizers

import rlp.ai.ActivationFunction.{ReLU, Sigmoid}
import rlp.ai.NeuralNetwork
import rlp.math.Matrix

abstract class NetworkOptimizer(val network: NeuralNetwork) {

  def step(input: Matrix, target: Matrix): Unit = {
    val gradients = network.backProp(input, target)
    step(gradients)
  }

  def step(gradient: Array[Matrix]): Unit
}

class SGDMomentum(network: NeuralNetwork,
                   learningRate: Double, momentumRate: Double
                  ) extends NetworkOptimizer(network) {

  val velocity: Array[Matrix] = network.weights.map(w => new Matrix(w.getRows, w.getCols))

  override def step(gradient: Array[Matrix]): Unit = {
    for (i <- velocity.indices) {
      velocity(i) *= momentumRate
      velocity(i) -= (gradient(i) * learningRate)

      network.weights(i) += velocity(i)
    }
  }
}

class RMSProp(network: NeuralNetwork, forgettingFactor: Double, learningRate: Double) extends NetworkOptimizer(network) {

  val v: Array[Matrix] = network.weights.map(w => new Matrix(w.getRows, w.getCols))

  override def step(gradient: Array[Matrix]): Unit = {
    for (i <- v.indices) {
      for (j <- gradient(i).getData().indices) {

        // Update rms properties
        v(i)(j) = forgettingFactor*v(i)(j) + (1-forgettingFactor)*gradient(i)(j)*gradient(i)(j)

        // Update weight
        network.weights(i)(j) -= learningRate * gradient(i)(j) / math.sqrt(v(i)(j))
      }
    }
  }
}

object Test {

  def main(args: Array[String]): Unit = {
    val networkA, networkB, networkC = new NeuralNetwork(Array(2, 5, 1), Array(Sigmoid, ReLU))

    networkA.randomiseWeights()
    for (i <- networkA.weights.indices) {
      networkB.weights(i) = new Matrix(networkA.weights(i))
      networkC.weights(i) = new Matrix(networkA.weights(i))
    }

    val learningRate = 0.1
    val momentumRate = 0.7

    val sgd = new SGDMomentum(networkA, learningRate, 0)
    val sgdMomentum = new SGDMomentum(networkB, learningRate, momentumRate)
    val rmsProp = new RMSProp(networkC, 0.9, 0.008)

    val dataSamples = 200
    val function = (x:Double, y:Double) => 4*x+2*y

    val inputData = new Matrix(dataSamples, 2) each (_ => math.random())
    val targets = new Matrix(dataSamples, 1,
      (0 until dataSamples) map (i => function(inputData(i,0),inputData(i,1))) toArray)

    println("Epoch\t\tSGD\t\t\t\tSGD+Momentum\t\t\t\tRMS Prop")
    for (i <- 0 until 500) {
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