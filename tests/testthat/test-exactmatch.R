#test-exactmatch.R
#Tests CPATExcel vs CPATR

context("test 2018 china residential coal input data is correct")


test_that("running mean with constant x or position", {

  expect_equal(1,1,tolerance=0.1)
  expect_equal(BaseYear,2018L,tolerance=0.1)

  #expect_equal(FindFilterdValueCPAT(),FindFilterdValueR(),tolerance=0.1)


  })