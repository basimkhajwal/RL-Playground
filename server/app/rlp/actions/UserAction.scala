package rlp.actions

import javax.inject._

import play.api.mvc._
import rlp.models.User
import rlp.service.UserService

import scala.concurrent.{ExecutionContext, Future}

class UserRequest[A](val user: Option[User], request: Request[A]) extends WrappedRequest[A](request)

@Singleton
class UserAction @Inject()(val parser: BodyParsers.Default, userService: UserService)(implicit val executionContext: ExecutionContext)
  extends ActionBuilder[UserRequest, AnyContent] with ActionTransformer[Request, UserRequest] {

  override protected def transform[A](request: Request[A]): Future[UserRequest[A]] = {
      request.session.get("username") match {
        case None => Future.successful(new UserRequest(None, request))
        case Some(sessionName) => {
          userService.findByUsername(sessionName) map { users =>
            if (users.isEmpty) new UserRequest(None, request)
            else new UserRequest(Some(users.head), request)
          }
        }
      }
  }
}
