package rlp.forms

import play.api.data.{Form, Forms}

case class LoginForm(
  email: String,
  password: String
)

object LoginForm {
  val form = Form(
    Forms.mapping(
      "email" -> Forms.email,
      "password" -> Forms.text().verifying("Minimum password length is 8", _.length >= 8)
    )(LoginForm.apply)(LoginForm.unapply)
  )
}