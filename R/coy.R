#' Is the start of a coy string valid?
#'
#' Pattern match the expected start of a components of yield string
#'
#' @param x A valid coy string
#'
#' @export coyStart
#'
coyStart <- function(x){
  return(grepl("^,", x))
}

#' Is the end of a coy string valid?
#'
#' Patternmatch the expected end of a components of yield string
#'
#' @param x A valid coy string
#'
#' @export coyEnd
#'
coyEnd <- function(x){
  return(grepl("L", x, ignore.case = TRUE))
}


#' Split components of yields characters into a string
#'
#' @param x A valid coy string
#'
#' @export splitChars
#'
splitChars <- function(x) {
  chars <- strsplit(x, "")
  return(chars)
}

#' Check allowed character types
#'
#' Check all allowed character types in components of yield string
#'  [,.0-9L] are all allowed characters
#'
#' @param x A valid coy string
#'
#' @export checkChars
#'
checkChars <- function(x) {
  ok <- lapply(splitChars(x), function(x){x%in%c(",", ".", 0:9, "L")})
  return(ok)
}

#' Test if characters are all ok
#'
#' Uses checkChars(...) and returns a logical if all characters are ok
#'
#' @param x A valid coy string
#'
#' @export okChars
#'
okChars <- function(x){
  ok <- sapply(checkChars(x), function(x)all(x))
  return(ok)
}

#' Find malformed characters in a components of yield string
#'
#' Uses checkChars(...) and returns a list
#'
#' @param x A valid coy string
#'
#' @export coyChars
#'
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

#' Checks and validates a component of yield string
#'
#' @param x A valid coy string
#'
#' @export validateCOY
#'
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

#' Extract WinterBuds component
#'
#' @param x A valid coy string
#'
#' @return numeric vector
#'
#' @export WinterBuds
#'
WinterBuds <- function(x){
  nchar(gsub(",(.+)L.+", "\\1", x))
}

#' Count KingFlowers component
#'
#' @param x A valid coy string
#'
#' @return numeric vector
#'
#' @export KingFlowers
#'
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

#' Extract FloralShoots component
#'
#' @param x A valid coy string
#'
#' @return numeric vector
#'
#' @export FloralShoots
#'
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

#' Extract LateralFlowers  component
#'
#' @param x A valid coy string
#'
#' @return numeric vector
#'
#' @export LateralFlowers
#'
LateralFlowers <- function(x){
  lats <- as.numeric(gsub(".+L(.+)$", "\\1", x))
  return(lats)
}

#' Count VegetShoots component
#'
#' @param x A valid coy string
#'
#' @return numeric vector
#' @export VegetShoots
#'
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

#' Return a dataframe in wide format of components of yield
#'
#' @param x A valid coy string
#'
#' @return numeric vector
#'
#' @export wideCOY
#'
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



