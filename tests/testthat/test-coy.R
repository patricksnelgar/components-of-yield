context("Components of yield")

test_that("Components of yield parsing functions", {

	x <- c(",0.0574250.240.6.5.00347.21.6L23", ",1234567.8.9L10", ",.0b220.L10")
	y <- c(",00..00.0..0L0")
	b <- c(",-.97.-33.4L100", ",---19b+29L10")

 	expect_equal(WinterBuds(x), c(28,11,7))
	expect_equal(KingFlowers(x), c(63,45,4))
	expect_equal(LateralFlowers(x), c(23,10,10))
	expect_equal(FloralShoots(x), c(15,9,2))
	expect_equal(VegetShoots(x), c(6,0,2))
	expect_equal(budBreak(x), c(0.75, 0.818, 0.714), tolerance = 0.001)
	
	expect_equal(coyStart(x), c(TRUE, TRUE, TRUE))
	expect_equal(coyEnd(x), c(TRUE, TRUE, TRUE))
	
	expect_equal(extractFloralShoots(x), c("574252465347216", "123456789", "22"))
	expect_equal(extractFloralShoots(y), c(""))
	
	expect_true(isValidCoy(y))
	expect_true(isValidCoy(x))
	
	expect_equal(removeBirdDamage(b), c(",.97.33.4L100", ",1929L10"))
	expect_equal(hasBirdDamage(b), c(TRUE, TRUE))
	expect_equal(hasBirdDamage(x), c(FALSE, FALSE, TRUE))
	
	expect_equal(floralPercentage(x), c(0.5357143, 0.8181818, 0.3333333), tolerance = 1e-7)
	expect_equal(floralPercentage(b), c(0.625, 1.0))
	
	expect_equal(vegetativePercentage(x), c(0.2142857, 0.0, 0.3333333), tolerance = 1e-7)
	expect_equal(vegetativePercentage(b), c(0.0, 0.0))
	
	expect_equal(calculateKFperWB(x), c(2.25, 4.09, 0.6667), tolerance = 0.001)
	expect_equal(calculateLFperWB(x), c(0.821, 0.909, 1.667), tolerance = 0.001)
	
	expect_equal(checkChars(x),
	           list(c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
	                  TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
	                  TRUE, TRUE,  TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
	                  TRUE, TRUE, TRUE, TRUE,  TRUE),
	                c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
	                  TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
	           	 	c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
	           	 	  TRUE, TRUE)))
	
	expect_equal(okChars(x), c(TRUE, TRUE, TRUE))
	expect_null(coyChars(x))
	expect_null(validateCOY(x))
	expect_equivalent(wideCOY(x), structure(list(warnings = c(0, 0, 0),
	                                           WinterBuds = c(28L, 11L, 7L),
	                                           KingFlowers = c(63, 45, 4),
	                                           LateralFlowers = c(23, 10, 10),
	                                           floralShoots = c(15L, 9L, 2L),
	                                           vegetShoots = c(6L, 0L, 2L)),
	                                      class = "data.frame",
	                                      row.names = c(NA,  -2L)))
	
	
	x <- c("0.0574250.240.6.5.00347.21.623", ",1234;567.8.9L10")
	y <- c("000.1.2,")
  
	expect_false(isValidCoy(y))
	
	expect_equal(WinterBuds(x), c(30,12))
	expect_error(KingFlowers(x), "argument of length 0")
	expect_warning(LateralFlowers(x), "NAs introduced by coercion")
	expect_equal(suppressWarnings(LateralFlowers(x)), c(NA, 10))
	expect_error(FloralShoots(x), "argument of length 0")
	expect_error(VegetShoots(x), "argument of length 0")
	
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
	expect_error(wideCOY(x), "argument of length 0")

})
