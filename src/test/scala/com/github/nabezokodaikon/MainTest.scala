package com.github.nabezokodaikon

import org.scalatest.FunSuite                       

class MainTest extends FunSuite {                   
  test("helloWorld") {                              
    assert(Main.helloWorld("nabezokodaikokn") == "Hello nabezokodaikokn!")
  }                                                 
}                                                   
