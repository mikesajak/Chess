package com.mikesajak.chess

import org.scalatest._
import org.scalatest.flatspec._
import matchers._

class UnitTestSpec extends AnyFlatSpec with should.Matchers with OptionValues 
    with Inside with Inspectors
