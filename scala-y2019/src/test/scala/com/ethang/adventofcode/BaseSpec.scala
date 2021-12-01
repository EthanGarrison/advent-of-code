package com.ethang.adventofcode

import org.scalatest.Inspectors
import org.scalatest.matchers.must.{Matchers => MustMatchers}
import org.scalatest.flatspec.{AnyFlatSpec => FlatSpec}

trait BaseSpec extends FlatSpec with MustMatchers with Inspectors
