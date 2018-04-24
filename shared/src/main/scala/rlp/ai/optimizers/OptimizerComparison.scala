package rlp.ai.optimizers

import rlp.ai.ActivationFunction.{Linear, ReLU, Sigmoid}
import rlp.ai.NeuralNetwork
import rlp.math.Matrix

/**
  * Integration test to compare the effectiveness and correctness
  * of various optimisation strategies
  */
object OptimizerComparison {

  def main(args: Array[String]): Unit = {

    val epochs = 10000
    val dataSamples = 100
    val displayEpochs: List[Int] =
      List(0) ++
      (1 until 10 by 2).toList ++
      (10 until 100 by 20).toList ++
      (100 until 1000 by 200).toList ++
      (1000 until epochs by 2000)

    val testFunction = (x: Double, y: Double) => if (x*x + y*y < 0.25) 1.0 else -1.0

    val networkA = new NeuralNetwork(Array(2, 5, 5, 1), Array(Sigmoid, ReLU, Linear))
    val networkB = networkA.clone(false)

    networkA.randomiseWeights(-3, 3)
    networkB.initialiseWeights()

    val optimizers: Map[String, NetworkOptimizer] = Map(
      "Momentum" -> new SGDMomentum(networkA.clone(), 0.01, 0.1),
      // "Momentum+WeightInit" -> new SGDMomentum(networkB.clone(), 0.01, 0.1),
      "RMSProp" -> new RMSProp(networkA.clone(), 0.003),
      // "RMSProp+WeightInit" -> new RMSProp(networkB.clone(), 0.003),
      "ADAM" -> new Adam(networkA.clone(), 0.005),
      // "ADAM+WeightInit" -> new Adam(networkB.clone(), 0.003)
    )

    val input = new Matrix(dataSamples, 2) each (_ => math.random())

    val target = new Matrix(
      dataSamples, 1,
      (0 until dataSamples) map (i => testFunction(input(i,0), input(i,1))) toArray
    )

    printf("%-10s", "Episodes")
    for (name <- optimizers.keys) printf("%-15s", name)
    println()

    var displayIdx = 0
    for (e <- 0 until epochs) {

      if (displayIdx < displayEpochs.length && e == displayEpochs(displayIdx)) {

        printf("%-10d", e)

        for (opt <- optimizers.values) {
          val loss: Double = opt.network.loss(input, target).sum
          printf("%-15f", loss)
        }

        println()

        displayIdx += 1
      }

      for (opt <- optimizers.values) opt.step(input, target)
    }
  }
}
