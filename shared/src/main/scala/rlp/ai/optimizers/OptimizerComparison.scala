package rlp.ai.optimizers

import rlp.ai.ActivationFunction.{Linear, ReLU, Sigmoid}
import rlp.ai.NeuralNetwork
import rlp.math.Matrix

object OptimizerComparison {

  def main(args: Array[String]): Unit = {

    val epochs = 15000
    val dataSamples = 100
    val displayEpochs: List[Int] =
      (0 until 10).toList ++
      (10 until 100 by 10).toList ++
      (100 until 1000 by 100).toList ++
      (1000 until epochs by 1000)

    val network = new NeuralNetwork(Array(2, 5, 1), Array(Sigmoid, Linear))

    val testFunction = (x: Double, y: Double) => y + math.sin(10*x) + 4

    network.randomiseWeights(-3, 3)

    val optimizers: Map[String, NetworkOptimizer] = Map(
      "SGD" -> new SGDMomentum(network.clone()),
      "SGD+Momentum" -> new SGDMomentum(network.clone(), 0.01, 0.9),
      "RMSProp" -> new RMSProp(network.clone()),
      "ADAM" -> new Adam(network.clone())
    )

    val input = new Matrix(dataSamples, 2) each (_ => math.random())

    val target = new Matrix(
      dataSamples, 1,
      (0 until dataSamples) map (i => testFunction(input(i,0), input(i,1))) toArray
    )

    printf("%-10s", "Name")
    for (name <- optimizers.keys) printf("%-25s", name)
    println()

    var displayIdx = 0
    for (e <- 0 until epochs) {

      for (opt <- optimizers.values) opt.step(input, target)

      if (displayIdx < displayEpochs.length && e == displayEpochs(displayIdx)) {

        printf("%-10d", e)

        for (opt <- optimizers.values) {
          val loss: Double = opt.network.loss(input, target).sum
          printf("%-25f", loss)
        }

        println()

        displayIdx += 1
      }
    }

  }
}
