package rlp.ai.agents

import rlp.environment.SARSAAgent

class QNetworkAgent() extends SARSAAgent[Array[Double], Array[Double]]{



  override def sarsa(prevState: Array[Double], action: Array[Double], reward: Double, newState: Array[Double]): Array[Double] = {
    ???
  }
}
