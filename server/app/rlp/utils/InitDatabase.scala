package rlp.utils

import play.api._
import rlp.dao.{LeaderboardDAO, UserDAO}

object InitDatabase {

  def loadApp(): Application = {
    val env = Environment(new java.io.File("."), this.getClass.getClassLoader, Mode.Dev)
    val context = ApplicationLoader.createContext(env)
    val loader = ApplicationLoader(context)
    val app = loader.load(context)
    Play.start(app)

    app
  }

  def main(): Unit = {
    val app = loadApp()

    /* Get schema instances */
    val userDAO = app.injector.instanceOf[UserDAO]
    val leaderboardDAO = app.injector.instanceOf[LeaderboardDAO]

    // Init schema
    userDAO.createSchema()
    leaderboardDAO.createSchema()
  }
}
