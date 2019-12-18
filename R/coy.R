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
  return(grepl("L[0-9]+", x, ignore.case = TRUE))
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
#'  [,.0-9LlbB-+] are all allowed characters
#'
#' @param x A valid coy string
#'
#' @export checkChars
#'
checkChars <- function(x) {
  ok <- lapply(splitChars(x), function(x){x%in%c(",", ".", 0:9, "L", "l", "b", "B", "-", "+")})
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
  	
  	if(coyStart(y)){
  		budsOnly <- gsub(",(.+)L.+$", "\\1", y, ignore.case = TRUE)
  	} else {
  		budsOnly <- gsub("(.+)L.+$", "\\1", y, ignore.case = TRUE)	
  	}
	
  	chars <- strsplit(budsOnly, "")[[1]]
  	
  	# remove all shoots that are 'dead' or have bird damage
    chars[chars%in%c(".", "b", "-", "B", "+")] <- NA

    return(sum(as.numeric(chars), na.rm=TRUE))
  }

  #elements <- splitChars(x)
  kings <- sapply(x, countKings, USE.NAMES = FALSE)

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
  	
  	budsOnly <- gsub(",(.+)L.+$", "\\1", y, ignore.case = TRUE)
  	
  	chars <- strsplit(budsOnly, "")[[1]]
  	
    ## pick out only numbers between 1:9, 0 is vegetative 
    chars <- chars[chars%in%c(1:9)]
    

    return(length(chars))
  }

  
  florals <- sapply(x, countFlorals, USE.NAMES = FALSE)

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
  lats <- as.numeric(gsub(".+L(.+)$", "\\1", x, ignore.case = TRUE))
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
    
  	budsOnly <- gsub(",(.+)L.+$", "\\1", y, ignore.case = TRUE)
  	
  	chars <- strsplit(budsOnly, "")[[1]]
  	
    ## only include 0 for veges
    chars <- chars[chars%in%c("0")]

    return(length(chars))
  }

  vege <- sapply(x, countVege, USE.NAMES = FALSE)

  # return(vege)
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



#' Return a string with only floral shoots
#'
#' @param x a valid COY string
#'
#' @return a string with only numerical values 1-9
#' 
#' @export 
extractFloralShoots <- function(x) {
	noLaterals <- gsub("L([0-9]+)", "", x, ignore.case = TRUE)
	onlyFloral <- gsub("[.,b0]+", "", noLaterals, ignore.case = TRUE)
	
	return(onlyFloral)
}


#' Return a logical vector corresponding to the validity of input string
#'
#' @param x coy string
#' @param checkComma flag to toggle validation of the starting character
#'
#' @return a boolean vector
#' 
#' @export isValidCoy
#' 
isValidCoy <- function(x, checkComma = TRUE) {
	
	valid <- TRUE
	
	if(checkComma){
		checkStart <- coyStart(x)
		if(!all(checkStart)) {
			valid <- FALSE
		}
	}
	
	checkEnd <- coyEnd(x)
	if(!all(checkEnd)) {
		valid <- FALSE
	}
	
	checkChars <- coyChars(x)
	if(!is.null(checkChars)) {
		valid <- FALSE
	}
	
	return(valid)
}

#' Returns a vector of booleans for each coy item in the input
#'
#' @param x list of coy strings
#' @param checkComma flag for toggling validation of coy starting character
#'
#' @return vector of booleans
#' @export batchValidateCoy
#'
batchValidateCoy <- function(x, checkComma = TRUE) {
	return(sapply(x, isValidCoy, USE.NAMES = FALSE, checkComma))
}
	

#' Return the percentage of buds that have shot (floral or vegetative)
#'
#' @param x A valid coy string
#'
#' @return a double
#' @export budBreak
#'
budBreak <- function(x) {
	winterBuds <- WinterBuds(x)
	dormantBuds <- nchar(gsub("[^.]", "", x))
	
	percentBudBreak <- 1 - (dormantBuds / winterBuds)
	
	return(percentBudBreak)
}


#' Return coy string wth all bird damage values removed
#'
#' @param x a valid coy string
#'
#' @return coy string with no bird damage
#' @export removeBirdDamage
#'
removeBirdDamage <- function(x) {
	return(gsub("[-b+]+", "", x, ignore.case = TRUE))
}


#' Returns a boolean value corresponding to whether the coy string contains chaacters for bird damage
#'
#' @param x a valid coy string
#'
#' @return a boolean
#' @export hasBirdDamage
#'
hasBirdDamage <- function(x) {
	return(sapply(splitChars(x), function(c) { any(c %in% c("b", "B", "-", "+"))}))
}


#' Calculate the percentage of floral shoots, automatically excludes bird damage
#'
#' @param x a valid coy string
#'
#' @return numeric vector 
#' @export floralPercentage
#'
floralPercentage <- function(x) {
	calculateFloral <- function(y) {
		if(hasBirdDamage(y))
			y <- removeBirdDamage(y)
		
		winterBuds <- WinterBuds(y)
		floralShoots <- FloralShoots(y)
		
		return(floralShoots / winterBuds) 
	}
	
	return(sapply(x, calculateFloral, USE.NAMES = FALSE))
}


#' Calculate vegetative shoot percentage, automatically excludes bird damage
#'
#' @param x a vlid coy string
#'
#' @return numeric vector
#' @export vegetativePercentage
#'
vegetativePercentage <- function(x) {
	
	calculateVegetative <- function(y) {
		if(hasBirdDamage(y))
			y <- removeBirdDamage(y)
		
		numBuds <- WinterBuds(y)
		vegetativeShoots <- VegetShoots(y)
		
		return(vegetativeShoots / numBuds)
	}
	return(sapply(x, calculateVegetative, USE.NAMES = FALSE))
}


#' Calculate the average king flowers per winter bud metric
#'
#' @param x a valid coy string
#'
#' @return numeric vector
#' @export KingFlowersPerWinterBud
#'
KingFlowersPerWinterBud <- function(x) {
	calculateKFperWB <- function(c) {
		if(hasBirdDamage(c))
			c <- removeBirdDamage(c)
		
		numBuds <- WinterBuds(c)
		numFlowers <- KingFlowers(c)
		
		return(numFlowers / numBuds)
	}
	
	return(sapply(x, calculateKFperWB, USE.NAMES = FALSE))
}


#' Calculate average lateral flowers per winter bud
#'
#' @param x a valid coy string
#'
#' @return a numeric vector
#' @export LateralFlowersPerWinterBud
#'
LateralFlowersPerWinterBud <- function(x) {
	
	calculateLFperWB <- function(c) {
		if(hasBirdDamage(c))
			c <- removeBirdDamage(c)
		
		numBuds <- WinterBuds(c)
		numLaterals <- LateralFlowers(c)
		
		return(numLaterals / numBuds)
	}
	
	return(sapply(x, calculateLFperWB, USE.NAMES = FALSE))
}


#' Returns a data frame with the standard COY analysis
#'
#' @param data data frame containing the IDs and coy strings
#' @param name column containing the coy data, default = "coy"
#' @param id column containing the identifiers, default = "CaneID"
#'
#' @return a data frame
#' @export CoyProcessor
#'
CoyProcessor <- function(data, name="coy", id="CaneID") {
	WB <- WinterBuds(data[[name]])
	KF <- KingFlowers(data[[name]])
	LF <- LateralFlowers(data[[name]])
	FS <- FloralShoots(data[[name]])
	VS <- VegetShoots(data[[name]])
	BB <- budBreak(data[[name]])
	FSP <- floralPercentage(data[[name]])
	KFPWB <- KingFlowersPerWinterBud(data[[name]])
	LFPWB <- LateralFlowersPerWinterBud(data[[name]])
	
	
	return(data.frame(CaneID = data[[id]], 
					  WinterBuds = WB,
					  KingFlowers = KF,
					  LateralFlowers = LF,
					  BudBreak = BB * 100,
					  VegetativeShoots = VS,
					  FloralShootPercentage = FSP * 100,
					  KingFlowersPerWinterBud = KFPWB,
					  LateralFlowersPerWinterBud = LFPWB,
					  KingFlowersPerFloralShoots = KF / FS,
					  row.names = 1:length(data[[name]])))
}
