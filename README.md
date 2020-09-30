Installation:
using remotes:
    remotes::install_github("patricksnelgar/components-of-yield")


Overview
========

Components of Yield
-------------------

There is a requirement to create functions in R (and perl) to parse components of yield information for both current and historical formats.

Kris Kramer-Walter has documented the conversion in a word document.

R code
------

The following script coy.R is a basis of functions to do the conversion. This requires a formalized test suite, and potentially the methods should be part of an S4 object orientated class.

``` r
source("R/coy.R")
ls()
```

    ##  [1] "checkChars"     "coyChars"       "coyEnd"         "coyStart"      
    ##  [5] "FloralShoots"   "gitRevision"    "KingFlowers"    "LateralFlowers"
    ##  [9] "okChars"        "revision"       "splitChars"     "validateCOY"   
    ## [13] "VegetShoots"    "wideCOY"        "WinterBuds"

Example 1
---------

The following example was provided;

``` r
x <- ",0.0574250.240.6.5.00347.21.6L23"

WinterBuds(x)
```

    ## [1] 28

``` r
KingFlowers(x)
```

    ## [1] 63

``` r
LateralFlowers(x)
```

    ## [1] 23

``` r
FloralShoots(x)
```

    ## [1] 15

``` r
VegetShoots(x)
```

    ## [1] 6

Example 2
---------

Vectorized example;

``` r
x <- c(",0.0574250.240.6.5.00347.21.6L23", ",1234567.8.9L10")

WinterBuds(x)
```

    ## [1] 28 11

``` r
KingFlowers(x)
```

    ## [1] 63 45

``` r
LateralFlowers(x)
```

    ## [1] 23 10

``` r
FloralShoots(x)
```

    ## [1] 15  9

``` r
VegetShoots(x)
```

    ## [1] 6 0

Tabular conversion
------------------

Copying the output headers created in excel;

``` r
x <- c(",0.0574250.240.6.5.00347.21.6L23", ",1234567.8.9L10")

wideCOY(x)
```

    ##   warnings WinterBuds KingFlowers LateralFlowers floralShoots vegetShoots
    ## 1        0         28          63             23           15           6
    ## 2        0         11          45             10            9           0

Sanity checking
---------------

The issues appear to be;

1.  All input records start with a "," for historical reasons to enforce character conversion
2.  All records should contain an "L", or potentially "l" before lateral shoot counts
3.  The only valid characters are in the character set \[,0-9.Ll\]

There are several helper functions;

-   coyStart() - Find malformed records not containing a starting ','
-   coyEnd() - Find malformed records not containing a 'L', or 'l'
-   okChars() - Find all records with illegal characters
-   coyChars() - Report illegal characters
-   validateCOY - throw an exception script

``` r
x <- c("0.0574250.240.6.5.00347.21.6L23", ",1234;567.8.9L10")
coyStart(x)
```

    ## [1] FALSE  TRUE

``` r
coyEnd(x)
```

    ## [1] TRUE TRUE

``` r
okChars(x)
```

    ## [1]  TRUE FALSE

``` r
coyChars(x)
```

    ## $malformed
    ## $malformed[[1]]
    ##  [1]  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## [12]  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 
    ## 
    ## $index.number
    ## [1] 2
    ## 
    ## $char.pos
    ## $char.pos[[1]]
    ## [1] 6

``` r
try(validateCOY(x))
```

    ## Error in validateCOY(x) : 
    ##   Start comma missing in: 0.0574250.240.6.5.00347.21.6L23

Todo
----

More information is required to understand and characterize functions for `DeadBuds` and `Blowouts`.

-   Collate all background specifications and existing excel conversion scripts
-   Convert this to a package namespace
-   S4 class with methods?
