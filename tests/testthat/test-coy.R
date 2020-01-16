context("Components of yield")

test_that("Components of yield parsing functions", {

	x <- c(",0.0574250.240.6.5.00347.21.6L23", ",1234567.8.9l10", ",.0b220.L10")
	y <- c(",00..00.0..0L0")
	b <- c(",-.97.-33.4L100", ",---19b+29l10")

 	expect_equal(WinterBuds(x), c(28,11,7))
 	expect_equal(WinterBuds(b), c(10,9))
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
	
	expect_equal(removeBirdDamage(b), c(",.97.33.4L100", ",1929l10"))
	expect_equal(hasBirdDamage(b), c(TRUE, TRUE))
	expect_equal(hasBirdDamage(x), c(FALSE, FALSE, TRUE))
	
	expect_equal(floralPercentage(x), c(0.5357143, 0.8181818, 0.3333333), tolerance = 1e-7)
	expect_equal(floralPercentage(b), c(0.625, 1.0))
	
	expect_equal(vegetativePercentage(x), c(0.2142857, 0.0, 0.3333333), tolerance = 1e-7)
	expect_equal(vegetativePercentage(b), c(0.0, 0.0))
	
	expect_equal(KingFlowersPerWinterBud(x), c(2.25, 4.09, 0.6667), tolerance = 0.001)
	expect_equal(LateralFlowersPerWinterBud(x), c(0.821, 0.909, 1.667), tolerance = 0.001)
	
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
	#expect_warning(KingFlowers(x), "NAs introduced by coercion")
	expect_warning(LateralFlowers(x), "NAs introduced by coercion")
	expect_equal(suppressWarnings(LateralFlowers(x)), c(NA, 10))
	
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
	expect_warning(wideCOY(x), "NAs introduced by coercion")
	
	
	dat <- data.frame(CaneID = c(1001, 1002, 1097, 1098, 1099, 1100, 1101, 1102, 1103, 1104, 1105), 
					  coy = c(",+36442....18.6..6.388878.8977.7...L43",
					  		",872.36.2..8.79.8.97.8498.7667.7.8.762.L40",
					  		",--29587.879909.8899.9998.6.L33",
					  		",546.....4.77.8767.567828.99.7.67.5..L24",
					  		",-745557..8.26.7..8.98.9.82986871L18",
					  		",55.8.37.60.6.88.6.87.7..6677.45.4.L16",
					  		",442....555.8.78.99.8...L47",
					  		",77...77.89.9.9.967.8.676b3L14",
					  		",3907..9.9298.98.9.98.99.9.7889.503L36",
					  		",7788888677597976798.989859988887.6L100",
					  		",bb6..6.7....88.58.98.99.99.9.78.99.9488.99.65.L63"), stringsAsFactors = FALSE)
	
	datOut <- data.frame(CaneID = c(1001, 1002, 1097, 1098, 1099, 1100, 1101, 1102, 1103, 1104, 1105), 
							 WinterBuds = c(34, 38, 27, 36, 32, 34, 23, 26, 34, 34, 46),
							 KingFlowers = c(120, 161, 148, 140, 139, 123, 74, 115, 166, 243, 201),
							 LateralFlowers = c(43, 40, 33, 24, 18, 16, 47, 14, 36, 100, 63),
							 BudBreak = c(61.76, 65.78, 81.48, 61.11, 71.87, 61.76, 52.17, 65.38, 70.58, 94.11, 60.86),
							 VegetativeShoots = c(0, 0, 1, 0, 0, 1, 0, 0, 2, 0, 0),
							 FloralShootPercentage = c(60.606, 65.789, 76.000, 61.111, 70.967, 58.823, 52.173, 64.000, 64.705, 94.117, 59.090),
							 KingFlowersPerWinterBud = c(3.64, 4.24, 5.92, 3.89, 4.48, 3.62, 3.22, 4.60, 4.88, 7.15, 4.57),
							 LateralFlowersPerWinterBud = c(1.303, 1.052, 1.320, 0.666, 0.580, 0.470, 2.043, 0.560, 1.058, 2.941, 1.431),
							 KingFlowersPerFloralShoots = c(6.00, 6.44, 7.79, 6.36, 6.32, 6.15, 6.17, 7.19, 7.55, 7.59, 7.73))
						 
	
	expect_equal(class(CoyProcessor(dat)), "data.frame")
	expect_equal(length(CoyProcessor(dat)), length(datOut))
	expect_equal(CoyProcessor(dat), datOut, tolerance = 0.001)
})
