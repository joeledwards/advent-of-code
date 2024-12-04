package com.buzuli.advent

import java.util.concurrent.TimeUnit

import scala.util.{Failure, Success, Try}
import com.buzuli.util.Env

import scala.concurrent.duration._

object Config {
  lazy val logOutput: Boolean = Env.getToggle("ADVENT_2020_LOG_OUTPUT").getOrElse(false)
  lazy val notificationSlackWebhook: Option[String] = Env.get("ADVENT_2020_NOTIFICATION_SLACK_WEBHOOK")
}
