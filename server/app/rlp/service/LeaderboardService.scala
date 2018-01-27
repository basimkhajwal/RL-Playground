package rlp.service

import javax.inject._

import rlp.dao.LeaderboardDAO
import rlp.models.LeaderboardEntry

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class LeaderboardService @Inject()(leaderboardDAO: LeaderboardDAO)(implicit ec: ExecutionContext) {

  def addEntry(entry: LeaderboardEntry): Future[LeaderboardEntry] = {
    leaderboardDAO.insert(entry)
  }


}
