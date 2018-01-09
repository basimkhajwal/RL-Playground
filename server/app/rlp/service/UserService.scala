package rlp.service

import java.security.SecureRandom
import javax.inject._

import org.apache.commons.codec.digest.Crypt
import rlp.dao.UserDAO
import rlp.forms.SignUpForm
import rlp.models.{EmailAccount, User}

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class UserService @Inject()(userDAO: UserDAO)(implicit ec: ExecutionContext) {

  def createUser(signUpForm: SignUpForm): Future[User] = {

    val random = new SecureRandom()
    val salt = "$6$" + Math.abs(random.nextInt()).toString
    val hash = Crypt.crypt(signUpForm.password, salt)

    val preInitUser = User(
      0,
      signUpForm.username,
      EmailAccount(signUpForm.email, hash, salt)
    )

    userDAO.insert(preInitUser)
  }
}
