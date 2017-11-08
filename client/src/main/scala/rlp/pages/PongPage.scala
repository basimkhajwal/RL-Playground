package rlp.pages


import com.thoughtworks.binding.Binding.Var
import rlp._
import rlp.environment.{Environment, NaivePongAgent, Pong}
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.{CanvasRenderingContext2D, document, html}
import org.scalajs.dom.html.{Canvas, Div}
import org.scalajs.dom.raw.Event

import scala.scalajs.js.{Date, timers}

class PongPage extends GamePage {

  @dom
  override lazy val gameOptions: Binding[Div] =  {
    <div>
      PONG!
    </div>
  }

}

