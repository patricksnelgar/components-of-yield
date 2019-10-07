#' export coyStart
#'
coyStart <- function(x){
  return(grepl("^,", x))
}

#' export coyEnd
#'
coyEnd <- function(x){
  return(grepl("L", x, ignore.case = TRUE))
}

#' export splitChars
#'
splitChars <- function(x) {
  chars <- strsplit(x, "")
  return(chars)
}

#' export checkChars
#'
checkChars <- function(x) {
  ok <- lapply(splitChars(x), function(x){x%in%c(",", ".", 0:9, "L")})
  return(ok)
}

#' export okChars
#'
okChars <- function(x){
  ok <- sapply(checkChars(x), function(x)all(x))
  return(ok)
}

#' export coyChars
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

#' export validateCOY
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

#' export WinterBuds
#'
WinterBuds <- function(x){
  nchar(gsub(",(.+)L.+", "\\1", x))
}

#' export KingFlowers
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

#' export FloralShoots
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

#' export LateralFlowers
#'
LateralFlowers <- function(x){
  lats <- as.numeric(gsub(".+L(.+)$", "\\1", x))
  return(lats)
}

#' export VegetShoots
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

#' export wideCOY
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



