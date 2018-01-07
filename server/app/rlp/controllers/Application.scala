package rlp.controllers

import javax.inject._

import play.api.mvc._
import rlp.forms.{LoginForm, SignUpForm}

@Singleton
class Application @Inject()(cc: ControllerComponents) extends AbstractController(cc) {

  def index = Action { implicit request => Ok(views.html.index()) }

  def login = Action { implicit request => Ok(views.html.login(LoginForm.form)) }

  def loginPost = Action { implicit request =>
    LoginForm.form.bindFromRequest.fold(
      formError => BadRequest(views.html.login(formError)),
      _ => Ok(views.html.index())
    )
  }

  def signUp = Action { implicit request => Ok(views.html.signup(SignUpForm.form)) }

  def signUpPost = Action { implicit request =>
    SignUpForm.form.bindFromRequest.fold(
      formError => BadRequest(views.html.signup(formError)),
      _ => Ok(views.html.index())
    )
  }
}
