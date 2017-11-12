package rlp.controllers

import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.html.Div
import rlp.ai.agents.QTableAgent
import rlp.environment.{Agent, MappedAgent}

class QTableController[O, A](
  numStates: Int, stateMap: (O) => Int,
  numActions: Int, actionMap: (Int) => A,
) extends ModelController[Agent[O, A]] {

  override val name: String = "Tabular Q Learning"

  @dom
  override lazy val modelOptions: Binding[Div] = {
    <div>
      Q Table Options! (to do at a later date)
    </div>
  }

  override def buildAgent(): Agent[O, A] = {
    new MappedAgent(new QTableAgent(numStates, numActions), stateMap, actionMap)
  }
}
