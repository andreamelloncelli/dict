require("testthat")

###############################################################
context("evalStringVec")

test_that("evalStringVec with 1 var",{
  # variables
  var = 1
  var_vec = c("var")
  valued_var_vec = evalStringVec( var_vec )

  # expectation
  expectedResult = c( 1 )
  expect_equal( valued_var_vec, expectedResult )
})

test_that("evalStringVec with a vector",{
  # variables
  first_var  = "FIRST_LABEL"
  second_var = "SECOND_LABEL"
  var_vec =c("first_var", "second_var")
  valued_var_vec = evalStringVec( var_vec )

  # expectation
  expectedResult = c( "FIRST_LABEL", "SECOND_LABEL" )
  expect_equal( valued_var_vec, expectedResult )
})

###############################################################
context("listToDict")

test_that("listToDict of list of one element",{

  # variables
  a = "aval"
  aval = "AVAL"

  # set up of the dict
  namedList <- list( a = aval )
  namedList <- listToDict( namedList  )

  # expectation
  expectedResult <-
    setNames(list("AVAL"), nm = c("aval"))

  expect_identical(namedList,expectedResult)

})

test_that("listToDict of list of two elements",{

  # variables
  a = "aval"
  aval = "AVAL"

  # set up of dict
  namedList <- list( a = aval, a = aval )
  namedList <- listToDict( namedList  )

  # expectation
  expectedResult <-
    setNames(list("AVAL","AVAL"), nm = c("aval", "aval"))
  expect_identical(namedList,expectedResult)

})

context("dict")

test_that("dict of list of one element",{

  # variables
  a = "aval"
  aval = "AVAL"

  # set up of dict
  namedList <- dict( a = aval )

  # expectation
  expectedResult <-
    setNames(list("AVAL"), nm = c("aval"))
  expect_identical(namedList,expectedResult)

})

test_that("dict of list of two elements",{

  # variables
  a = "aval"
  aval = "AVAL"

  # set up of dict
  namedList <- dict( a = aval, a = aval )

  # expectation
  expectedResult <-
    setNames(list("AVAL","AVAL"), nm = c("aval", "aval"))

  expect_identical(namedList,expectedResult)

})

