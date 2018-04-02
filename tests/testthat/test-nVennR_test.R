context("nVennR_test")

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


set1 <- list(set1 = c('a', 'b', 'c'))
set2 <- list(set2 = c('e', 'f', 'c'))
set3 <- list(set3 = c('c', 'b', 'e'))
testSVG1 <- plotVenn(set1, set2, set3, sNames=c("One", "Two", "Three"), showPlot = F)

test_that("plotVenn reads lists as input. List names precede sNames.",{
  expect_that(testSVG1, is_a("nVennObj"))
  expect_that(grep("^nVenn", testSVG1$def), equals(c(1)))
  expect_that(testSVG1$def[2], equals("3"))
})

test_that("plotVenn complains on empty input", {
  expect_that(plotVenn(), throws_error())
})
