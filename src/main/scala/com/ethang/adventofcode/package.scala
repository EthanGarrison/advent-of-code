package com.ethang

import scala.io.Source

package object adventofcode {

  def readResourceFile(file: String): Source = Source.fromResource(file)

}
