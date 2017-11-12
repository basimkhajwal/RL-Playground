package rlp.pages


import com.thoughtworks.binding.Binding.Var
import rlp._
import rlp.environment.{Agent, Environment, NaivePongAgent, Pong}
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.{CanvasRenderingContext2D, document, html}
import org.scalajs.dom.html.{Canvas, Div}
import org.scalajs.dom.raw.Event
import rlp.controllers.{ModelController, QTableController, QStateSpace}

import scala.scalajs.js.{Date, timers}

class PongPage extends GamePage[Agent[Pong.AgentState, Pong.Action]] {

  import Pong._
  import QStateSpace._

  override val modelControllers = List(
    new QTableController(
      2, { a => if (a == 0) UpAction else DownAction },
      boxed[AgentState]("Ball X", 0, SCREEN_WIDTH, 10, _.ballPos.x),
      boxed[AgentState]("Ball Y", 0, SCREEN_HEIGHT, 10, _.ballPos.y),
      boxed[AgentState]("Paddle Y", 0, SCREEN_HEIGHT, 10, _.currentPaddle)
    )
  )

}

