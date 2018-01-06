package rlp.controllers

import javax.inject._

import play.api.mvc._

@Singleton
class Application @Inject()(cc: ControllerComponents) extends AbstractController(cc) {

  def index = Action { Ok(views.html.index()) }

  def login = Action { Ok(views.html.login()) }

  def signUp = Action { Ok(views.html.signup()) }
}
