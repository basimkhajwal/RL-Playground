package rlp.agent

import ujson.Js

/**
  * Representation of a reinforcement learning agent
  *
  * @tparam S The agent percept state required on each time step
  * @tparam A The action the agent can perform
  */
trait Agent[S, A] {

  /* Control whether or not the agent will update itself when carrying out actions */

  private var _trainEnabled: Boolean = true

  def setTrainEnabled(trainEnabled: Boolean): Unit = {
    _trainEnabled = trainEnabled
  }

  def isTrainEnabled(): Boolean = _trainEnabled

  /**
    * Accept a new state from the environment and return
    * a corresponding action to take
    *
    * @param state
    * @return The agent's action given this percept
    */
  def act(state: S): A

  /**
    * Percept a reward for the previous action taken and whether
    * the episode finished
    * @param reward
    */
  def percept(reward: Double): Unit = {}

  /**
    * Called to reset the agent to the initial condition
    * outside of an episode
    */
  def resetEpisode(): Unit = {}

  /**
    * Reset the agent to it's default state before
    * it learnt anything
    * @return
    */
  def reset(): Unit = {}

  /**
    * Replicate this agent so that the duplicated agent
    * has a copy of but a different shared state so it's
    * learning diverges from the parent
    * @return
    */
  def duplicate(): Agent[S,A] = clone()

  /**
    * Replicate this agent, keeping a shared learnt state
    * so the agent and it's clone both learn the same representation
    * @return
    */
  override def clone():Agent[S,A] = this

  def load(data: Js.Value): Unit = {}

  def store(): Js.Value = Js.Null
}

