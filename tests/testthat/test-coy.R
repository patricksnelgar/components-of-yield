context("Components of yield")

test_that("Components of yield parsing functions", {

  x <- c(",0.0574250.240.6.5.00347.21.6L23", ",1234567.8.9L10")

  expect_equal(WinterBuds(x), c(28,11))
  expect_equal(KingFlowers(x), c(63,45))
  expect_equal(LateralFlowers(x), c(23,10))
  expect_equal(FloralShoots(x), c(15,9))
  expect_equal(VegetShoots(x), c(6,0))

  x <- c("0.0574250.240.6.5.00347.21.623", ",1234;567.8.9L10")


  expect_equal(coyStart(x), c(FALSE, TRUE))
  expect_equal(coyEnd(x), c(FALSE, TRUE))
  expect_equal(checkChars(x),
               list(c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
                      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
                      TRUE, TRUE,  TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
                      TRUE, TRUE, TRUE),
                    c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE,
                      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)))

  expect_equal(okChars(x), c(TRUE, FALSE))
  expect_equal(coyChars(x),
               list(malformed =
                      list(c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE,  TRUE, TRUE,
                             TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)),
                    index.number = 2L, char.pos = list(6L)))

  expect_error(validateCOY(x), "Start comma missing in: 0.0574250.240.6.5.00347.21.623")

})
