package com.github.nabezokodaikon

import com.typesafe.scalalogging.LazyLogging

object Main extends App with LazyLogging {

  def helloWorld(name: String): String = {
    "Hello " + name + "!"
  }

  logger.info("HelloWorld!")

}
