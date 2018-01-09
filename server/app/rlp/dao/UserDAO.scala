package rlp.dao

import javax.inject._

import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import rlp.models.{EmailAccount, GoogleAccount, LoginInfo, User}
import slick.jdbc.JdbcProfile
import slick.jdbc.meta.MTable

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class UserDAO @Inject()(protected val dbConfigProvider: DatabaseConfigProvider)(implicit ec: ExecutionContext)
  extends HasDatabaseConfigProvider[JdbcProfile] {

  import profile.api._

  val users = TableQuery[UserTable]

  def createSchema(): Future[Boolean] = {
    db run MTable.getTables flatMap { tables =>
      val tableExists = tables.exists(_.name.name == users.baseTableRow.tableName)

      if (tableExists) Future.successful(false)
      else {
        db.run(users.schema.create) map { _ => true}
      }
    }
  }

  def insert(user: User): Future[User] = {
    val query = (users returning users.map(_.id)) += user
    db.run(query).map { newId => user.copy(id = newId) }
  }

  class UserTable(tag: Tag) extends Table[User](tag, "user") {

    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def username = column[String]("username")
    def googleId = column[Long]("googleId", O.Default(-1))
    def email = column[String]("email")
    def passwordHash = column[String]("password")
    def passwordSalt = column[String]("salt")

    type TableColumns = (Long, String, Long, String, String, String)

    def buildUser(columns: TableColumns): User = {

      val (id, username, googleId, email, passwordHash, passwordSalt) = columns

      val loginInfo =
        if (googleId > 0) GoogleAccount(googleId)
        else EmailAccount(email, passwordHash, passwordSalt)

      User(id, username, loginInfo)
    }

    def extractColumns(user: User): Option[TableColumns] = {

      user.loginInfo match {

        case GoogleAccount(googleId) =>
          Some((user.id, user.username, googleId, null, null, null))

        case EmailAccount(email, passwordHash, passwordSalt) =>
          Some((user.id, user.username, -1, email, passwordHash, passwordSalt))

        case _ => None
      }
    }

    override def * = (id, username, googleId, email, passwordHash, passwordSalt) <> (buildUser, extractColumns)
  }
}
