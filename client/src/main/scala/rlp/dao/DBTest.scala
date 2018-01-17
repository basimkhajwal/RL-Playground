package rlp.dao

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

import scala.concurrent.ExecutionContext.Implicits.global

@JSExportTopLevel("DBTest")
object DBTest {

  import IndexedDB._

  @JSExport
  def runTest(): Unit = {
    for {
      _ <- create(MODEL_STORE, js.Dynamic.literal("id" -> 1, "modelStore" -> "Lel, some model here..."))
      _ <- create(MODEL_STORE, js.Dynamic.literal("id" -> 2, "modelStore" -> "Some other model here"))
      _ <- create(MODEL_STORE, js.Dynamic.literal("id" -> 3, "modelStore" -> "Some other model here"))
      _ <- create(MODEL_STORE, js.Dynamic.literal("id" -> 4, "modelStore" -> "Some other model here"))
      _ <- create(MODEL_STORE, js.Dynamic.literal("id" -> 5, "modelStore" -> "Some other model here"))

      _ <- update(MODEL_STORE, js.Dynamic.literal("id" -> 2, "modelStore" -> "CHANGED THE GUY!"))

      _ <- delete(MODEL_STORE, 3)
      _ <- delete(MODEL_STORE, 4)

      items <- getAll[LocalModelDAO.ModelStoreItem](MODEL_STORE)
    } {
      items foreach (x => println(x.id + " - " + x.modelStore))
    }
  }
}
