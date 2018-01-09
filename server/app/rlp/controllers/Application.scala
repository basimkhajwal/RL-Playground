package rlp.controllers

import javax.inject._

import play.api.mvc._
import rlp.forms.{LoginForm, SignUpForm}
import rlp.service.UserService

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class Application @Inject()(userService: UserService, cc: ControllerComponents)(implicit ec: ExecutionContext) extends AbstractController(cc) {

  def index = Action { implicit request => Ok(views.html.index()) }

  def login = Action { implicit request => Ok(views.html.login(LoginForm.form)) }

  def loginPost = Action { implicit request =>
    LoginForm.form.bindFromRequest.fold(
      formError => BadRequest(views.html.login(formError)),
      _ => Ok(views.html.index())
    )
  }

  def signUp = Action { implicit request => Ok(views.html.signup(SignUpForm.form)) }

  def signUpPost = Action.async { implicit request =>
    SignUpForm.form.bindFromRequest.fold(
      formError => Future.successful(BadRequest(views.html.signup(formError))),
      signUpForm => {
        for {
          _ <- userService.createUser(signUpForm)
        } yield Ok(views.html.index())
      }
    )
  }
}
