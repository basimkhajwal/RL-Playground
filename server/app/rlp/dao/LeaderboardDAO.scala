package rlp.dao

import java.sql.Blob
import javax.inject._

import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import rlp.models.{EmailAccount, GoogleAccount, LeaderboardEntry, User}
import rlp.storage.Base64
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
    def performanceStep = column[Int]("performanceStep")
    def performanceHistory = column[Array[Byte]]("performanceHistory")

    def buildData = column[String]("buildData")
    def agentData = column[String]("agentData")

    def score = column[Double]("score")

    type TableColumns = (Long, Long, Long, String, String, String, Int, Int, Array[Byte], String, String, Double)

    def buildLeaderboard(columns: TableColumns): LeaderboardEntry = {

      val (
        id, userId, timeStamp, environmentName,
        agentName, modelName, gamesPlayed,
        performanceStep, performanceHistory,
        buildData, agentData, score
      ) = columns

      LeaderboardEntry(
        id, userId, timeStamp, environmentName,
        agentName, modelName, gamesPlayed,
        performanceStep, Base64.fromByteArray(performanceHistory),
        buildData, agentData, score
      )
    }

    def extractColumns(e: LeaderboardEntry): Option[TableColumns] = {
      Some((
        e.id, e.userId, e.timeStamp, e.environmentName,
        e.agentName, e.modelName, e.gamesPlayed,
        e.performanceStep, Base64.toByteArray(e.performanceHistory),
        e.buildData, e.agentData, e.score
      ))
    }

    override def * = (
      id, userId, timeStamp, environmentName,
      agentName, modelName, gamesPlayed,
      performanceStep, performanceHistory,
      buildData, agentData, score
    ) <> (buildLeaderboard, extractColumns)
  }
}
