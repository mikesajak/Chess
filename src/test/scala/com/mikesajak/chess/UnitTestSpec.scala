package com.mikesajak.chess

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.{Inside, Inspectors, OptionValues}

trait UnitTestSpec extends AnyFlatSpec with should.Matchers with OptionValues
    with Inside with Inspectors
