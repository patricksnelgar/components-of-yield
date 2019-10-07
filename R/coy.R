#!/usr/bin/env R

require(testthat)

## Components of yield

coyStart <- function(x){
  return(grepl("^,", x))
}

coyEnd <- function(x){
  return(grepl("L", x, ignore.case = TRUE))
}

splitChars <- function(x) {
  chars <- strsplit(x, "")
  return(chars) 
}

checkChars <- function(x) {
  ok <- lapply(splitChars(x), function(x){x%in%c(",", ".", 0:9, "L")})
  return(ok)
}

okChars <- function(x){
  ok <- sapply(checkChars(x), function(x)all(x))
  return(ok)
}

coyChars <- function(x) {
  ok <- checkChars(x)
  indices <- sapply(ok, function(x){any(!x)} )
  
  if(any(indices)) {
    malformed <- checkChars(x[indices])
    elements       <- lapply(malformed, function(x){which(!x)})
    return(list(malformed=malformed, index.number=which(indices), char.pos=elements))
  }

  return()
}


validateCOY <- function(x){
  checkStart <- coyStart(x)
  if(!all(checkStart)) {
    startMessage <- paste("Start comma missing in:", x[which(!checkStart)])
    stop(startMessage)
  }
  checkEnd <- coyEnd(x)
  if(!all(checkEnd)) {
    endMessage <- paste("End L character missing in:", x[which(!checkEnd)])
    stop(endMessage)
  }
  
  checkChars <- coyChars(x)
  if(!is.null(checkChars)) {
    charMessage <- paste("Illegal characters in x:", checkChars)
    stop(charMessage)
  }

  return()
}

WinterBuds <- function(x){
  nchar(gsub(",(.+)L.+", "\\1", x))
}

KingFlowers <- function(x) {
  
  countKings <- function(y) {
    stInd  <- match(",", y)+1
    ## Remove NAs from upper/lowercase match
    res    <- match(c("L","l"), y)-1
    endInd <- res[!is.na(res)]
    chars <- y[stInd:endInd]
    chars[chars=="."] <- NA
    
    return(sum(as.numeric(chars), na.rm=TRUE))
  }
  
  elements <- splitChars(x)
  kings <- sapply(elements, countKings)
  
  return(kings)
}

FloralShoots <- function(x) {
  
  countFlorals <- function(y) {
    stInd  <- match(",", y)+1
    ## Remove NAs from upper/lowercase match
    res    <- match(c("L","l"), y)-1
    endInd <- res[!is.na(res)]
    chars <- y[stInd:endInd]
    ## exclude 0, '.' for florals
    chars <- chars[!chars%in%c("0", ".")]

    return(length(chars))
  }
  
  elements <- splitChars(x)
  florals <- sapply(elements, countFlorals)
  
  return(florals)
}

LateralFlowers <- function(x){
  lats <- as.numeric(gsub(".+L(.+)$", "\\1", x))
  return(lats)
}

VegetShoots <- function(x){
  
  countVege <- function(y) {
    stInd  <- match(",", y)+1
    ## Remove NAs from upper/lowercase match
    res    <- match(c("L","l"), y)-1
    endInd <- res[!is.na(res)]
    chars <- y[stInd:endInd]
    ## only include 0 for veges
    chars <- chars[chars%in%c("0")]
    
    return(length(chars))
  }
  
  elements <- splitChars(x)
  florals <- sapply(elements, countVege)
  
  return(florals)
}

wideCOY <- function(x, names=NULL) {
  res <- data.frame(warnings = 1*(!(coyStart(x) & coyEnd(x) & okChars(x))),
                    WinterBuds=WinterBuds(x),
                    KingFlowers=KingFlowers(x),
                    LateralFlowers=LateralFlowers(x),
                    floralShoots=FloralShoots(x),
                    vegetShoots=VegetShoots(x)
  )

  return(res)
}

x <- c(",0.0574250.240.6.5.00347.21.6L23", ",1234567.8.9L10")

## Main scripts
WinterBuds(x)
KingFlowers(x)
LateralFlowers(x)
FloralShoots(x)
VegetShoots(x)

expect_equal(WinterBuds(x), c(28,11))
expect_equal(KingFlowers(x), c(63,45))
expect_equal(LateralFlowers(x), c(23,10))
expect_equal(FloralShoots(x), c(15,9))
expect_equal(VegetShoots(x), c(6,0))

x <- c("0.0574250.240.6.5.00347.21.623", ",1234;567.8.9L10")

## Helper scripts
coyStart(x)
coyEnd(x)
checkChars(x)
okChars(x)
coyChars(x)
# try(validateCOY(x))



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



