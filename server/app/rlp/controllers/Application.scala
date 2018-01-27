package rlp.controllers

import javax.inject._

import play.api.mvc._
import rlp.actions.UserAction
import rlp.forms.{LoginForm, SignUpForm}
import rlp.models.User
import rlp.service.UserService

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class Application @Inject()(userService: UserService, cc: ControllerComponents, userAction: UserAction)(implicit ec: ExecutionContext) extends AbstractController(cc) {

  def index = userAction { implicit request =>
    Ok(views.html.index(request.user))
  }

  def bindUserSession(user: User)(result: Result): Result = {
    result.withSession("username" -> user.username)
  }

  def login = Action { implicit request =>
    Ok(views.html.login(LoginForm.form))
  }

  def loginPost = Action.async { implicit request =>
    LoginForm.form.bindFromRequest.fold(
      formError => Future.successful { BadRequest(views.html.login(formError)) },
      loginForm => {
        userService.authenticate(loginForm).map {
          case Some(user) => bindUserSession(user) { Redirect(routes.Application.index()) }
          case _ => {
            BadRequest(views.html.login(
              LoginForm.form.fill(loginForm) withGlobalError "Invalid username or password"
            ))
          }
        }
      }
    )
  }

  def logOut = Action { implicit request =>
    Redirect(routes.Application.index()).withNewSession
  }

  def signUp = Action { implicit request =>
    Ok(views.html.signup(SignUpForm.form))
  }

  def signUpPost = Action.async { implicit request =>
    SignUpForm.form.bindFromRequest.fold(
      formError => Future.successful(BadRequest(views.html.signup(formError))),
      signUpForm => {
        userService.createUser(signUpForm) map { user =>
          bindUserSession(user) { Redirect(routes.Application.index()) }
        }
      }
    )
  }

  def addLeaderboardEntry = userAction { implicit request =>
    Ok("TODO")
  }
}
