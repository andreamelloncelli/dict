
context("dict2")

test_that("dict2 of list of two elements",{

  # variables
  a = "aval"
  aval = "AVAL"

  # set up of dict
  namedList <- dict2( c(a , aval), c( a, aval ) )

  # expectation
  expectedResult <-
    setNames(list("AVAL","AVAL"), nm = c("aval", "aval"))

  expect_identical(namedList,expectedResult)

})
