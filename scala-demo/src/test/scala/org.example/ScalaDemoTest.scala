package org.example

import org.scalatest.FunSuite


class ScalaDemoTest extends FunSuite {


  test("the given number is 5"){
    ScalaDemo.countDate(5)
//    assert(a==b)
  }

  test("the given number is 7") {
    ScalaDemo.countDate(7)
    //    assert(a==b)
  }
}