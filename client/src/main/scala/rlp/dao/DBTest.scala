package rlp.dao

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Integration test for database APIs
  */
@JSExportTopLevel("DBTest")
object DBTest {

  import IndexedDB._

  @JSExport
  def runTest(): Unit = {
    for {
      _ <- create(MODEL_STORE, js.Dynamic.literal("id" -> 1, "modelStore" -> "Test 1"))
      _ <- create(MODEL_STORE, js.Dynamic.literal("id" -> 2, "modelStore" -> "Test 2"))
      _ <- create(MODEL_STORE, js.Dynamic.literal("id" -> 3, "modelStore" -> "Test 3"))
      _ <- create(MODEL_STORE, js.Dynamic.literal("id" -> 4, "modelStore" -> "Test 4"))
      _ <- create(MODEL_STORE, js.Dynamic.literal("id" -> 5, "modelStore" -> "Test 5"))

      _ <- update(MODEL_STORE, js.Dynamic.literal("id" -> 2, "modelStore" -> "Changed model"))

      _ <- delete(MODEL_STORE, 3)
      _ <- delete(MODEL_STORE, 4)

      items <- getAll[LocalAgentDAO.AgentStoreItem](MODEL_STORE)
    } {
      items foreach (x => println(x.id + " - " + x.modelStore))
    }
  }
}
