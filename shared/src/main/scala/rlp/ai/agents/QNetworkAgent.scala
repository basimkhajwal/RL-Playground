package rlp.ai.agents

import rlp.environment.SteppedAgent

class QNetworkAgent() extends SteppedAgent[Array[Double], Array[Double]]{



  override def step(prevState: Array[Double], action: Array[Double], reward: Double, newState: Array[Double]): Array[Double] = {
    ???
  }
}
