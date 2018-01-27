package rlp.dao

import javax.inject._

import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import rlp.models.LeaderboardEntry
import slick.jdbc.JdbcProfile
import slick.jdbc.meta.MTable

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class LeaderboardDAO @Inject()(protected val dbConfigProvider: DatabaseConfigProvider)(implicit ec: ExecutionContext)
  extends HasDatabaseConfigProvider[JdbcProfile] {

  import profile.api._

  val entries = TableQuery[LeaderboardTable]

  def createSchema(): Future[Boolean] = {
    db run MTable.getTables flatMap { tables =>
      val tableExists = tables.exists(_.name.name == entries.baseTableRow.tableName)

      if (tableExists) Future.successful(false)
      else {
        db.run(entries.schema.create) map { _ => true}
      }
    }
  }

  def insert(newEntry: LeaderboardEntry): Future[LeaderboardEntry] = {
    val query = (entries returning entries.map(_.id)) += newEntry
    db.run(query).map { newId => newEntry.copy(id = newId) }
  }

  class LeaderboardTable(tag: Tag) extends Table[LeaderboardEntry](tag, "leaderboard") {

    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def userId = column[Long]("userId")
    def timeStamp = column[Long]("timeStamp")

    def environmentName = column[String]("environmentName")
    def agentName = column[String]("agentName")
    def modelName = column[String]("modelName")

    def gamesPlayed = column[Int]("gamesPlayed")
    def score = column[Double]("score")

    override def * = (
      id, userId, timeStamp, environmentName,
      agentName, modelName, gamesPlayed, score
    ) <> (LeaderboardEntry.tupled, LeaderboardEntry.unapply)
  }
}
