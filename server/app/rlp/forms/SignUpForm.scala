package rlp.forms

import play.api.data.{Form, Forms}

case class SignUpForm(
  email: String,
  username: String,
  password: String
)

object SignUpForm {

  val form = Form(
    Forms.mapping(
      "email" -> Forms.email,
      "username" -> Forms.text,
      "password" -> Forms.text
    )(SignUpForm.apply)(SignUpForm.unapply)
  )
}