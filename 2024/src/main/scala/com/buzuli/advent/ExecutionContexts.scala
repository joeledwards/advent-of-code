package com.buzuli.advent

import com.buzuli.util.SysInfo

import java.util.concurrent.{LinkedBlockingQueue, ThreadPoolExecutor, TimeUnit}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.{Duration, DurationInt}
import scala.language.postfixOps

case class CustomExecutionContext(
  ec: ExecutionContext,
  executor: ThreadPoolExecutor
) {
  def halt(): Unit = executor.shutdown()
}

object ExecutionContexts {
  def executor(threadCount: Int): CustomExecutionContext = {
    val coreSize = threadCount
    val maxSize = threadCount
    val keepAlive: Duration = 1 minute
    val workQueue = new LinkedBlockingQueue[Runnable]

    val executor = new ThreadPoolExecutor(coreSize, maxSize, keepAlive.toMillis, TimeUnit.MILLISECONDS, workQueue)
    val ec = ExecutionContext.fromExecutor(executor)
    val context = CustomExecutionContext(ec, executor)
    context
  }

  lazy val _default: CustomExecutionContext = executor(SysInfo.parallelism * 2)
  def default: ExecutionContext = _default.ec

  def halt(): Unit = {
    _default.halt()
  }
}
