package rlp.service

import java.security.SecureRandom
import javax.inject._

import org.apache.commons.codec.digest.Crypt
import rlp.dao.UserDAO
import rlp.forms.{LoginForm, SignUpForm}
import rlp.models.{EmailAccount, User}

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class UserService @Inject()(userDAO: UserDAO)(implicit ec: ExecutionContext) {

  private def calculateHash(password: String, salt: String): String = {
    Crypt.crypt(password, salt)
  }

  def createUser(signUpForm: SignUpForm): Future[User] = {

    val random = new SecureRandom()
    val salt = "$6$" + Math.abs(random.nextInt()).toString
    val hash = calculateHash(signUpForm.password, salt)

    val preInitUser = User(
      0,
      signUpForm.username,
      EmailAccount(signUpForm.email, hash, salt)
    )

    userDAO.insert(preInitUser)
  }

  def authenticate(loginForm: LoginForm): Future[Option[User]] = {
    userDAO.findByEmail(loginForm.email) map { emailMatched =>
      if (emailMatched.isEmpty) None
      else {
        val testUser = emailMatched.head
        testUser.loginInfo match {
          case EmailAccount(_, passwordHash, passwordSalt) =>
            if (calculateHash(loginForm.password, passwordSalt) == passwordHash) {
              Some(testUser)
            } else {
              None
            }

          case _ => None
        }
      }
    }
  }

  def findByUsername(username: String): Future[Seq[User]] = userDAO.findByUsername(username)
}
