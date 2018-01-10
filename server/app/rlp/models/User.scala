package rlp.models

case class User(
  id: Long,
  username: String,
  loginInfo: LoginInfo
)

sealed trait LoginInfo
case class GoogleAccount(id: Long) extends LoginInfo
case class EmailAccount(
  email: String,
  passwordHash: String,
  passwordSalt: String
) extends LoginInfo