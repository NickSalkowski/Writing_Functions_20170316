# Writing Your Own Functions
Nick Salkowski  
March 16, 2017  

## Code Reuse



- Copying and pasting code leads to errors
  - Every time you use your code in a new spot, you might need to tweak some of it -- you'll forget to tweak something important
  - Every time you make your code better (update or fix a bug), you'll need to update it everywhere -- you'll forget to update some spot
  
![Don't be like this ape-guy...](http://i.giphy.com/Btg2Ly7SLkZA4.gif)  

## Functions solve these problems

- Changing function arguments is a lot more straightforward than searching and replacing to tweak code
- Updates / Fixes get applied every time the function is used

<img src="http://i.giphy.com/mh4Ro21ZJssgw.gif" alt="" height="320">

Learn from the MONOLITH!

## Creating your own function

Functions are objects (like almost everyting in R).  You can create one with the `function()` function:


```r
outlier <- function(x) {
  x_median <- median(x, na.rm = TRUE)
  x_mad <- mad(x, na.rm = TRUE)
  out_log <- abs(x - x_median) / x_mad > 4
  out_index <- which(out_log)
  return(out_index)
}
```

## Simulate


```r
test <- c(rpois(98, lambda = 25),
          rpois(2, lambda = 625))
hist(test)
```

![](Writing_Functions_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

## Test!

```r
outlier(test)
```

```
## [1]  99 100
```

```r
test[outlier(test)]
```

```
## [1] 631 610
```

![Lookin' good.](http://www.theshiznit.co.uk/media/2013/February/big-trouble-gifs/3-car.gif)

## Arguments

Let's modify the function to control our outlier criterion:


```r
outlier <- function(x, crit = 4, na.rm = TRUE) {
  x_median <- median(x, na.rm = na.rm)
  x_mad <- mad(x, na.rm = na.rm)
  out_log <- abs(x - x_median) / x_mad > crit
  out_index <- which(out_log)
  return(out_index)
}
```

## Test!


```r
outlier(test)
```

```
## [1]  99 100
```

```r
test[outlier(test)]
```

```
## [1] 631 610
```

```r
outlier(test, crit = 3)
```

```
## [1]  26  57  99 100
```

```r
test[outlier(test, 3)]
```

```
## [1]  41  40 631 610
```

## Complex Returns

Sometimes you want to return more than one thing.  Lists are vectors of things, so return a list!


```r
outlier <- function(x, crit = 4, na.rm = TRUE) {
  x_median <- median(x, na.rm = na.rm)
  x_mad <- mad(x, na.rm = na.rm)
  out_log <- abs(x - x_median) / x_mad > crit
  out_index <- which(out_log)
  return(list(value = x[out_index],
              index = out_index))
}
outlier(test)
```

```
## $value
## [1] 631 610
## 
## $index
## [1]  99 100
```

## Really Complex Returns


```r
return(list(value = x[out_index],
            index = out_index,
            criteria = list(median = x_median,
                            mad_sd = x_mad)))
```

<img src="http://i.giphy.com/U8Fm4k6xZawrm.gif" alt=" " height="320">

The list . . . it's full of lists!

## Add Some Argument Checks


```r
outlier <- function(x, crit = 4, na.rm = TRUE) {
  if (!is.numeric(x)) {
    stop("x must be numeric")
  }
  if (all(is.na(x))) {
    stop("x values are all NA")
  }
  if (!is.numeric(crit)) {
    stop("crit must be numeric")
  }
  if (length(crit) > 1) {
    crit <- crit[1]
    warning("length(crit) > 1, only the first element was used")
  }
  if (is.na(crit)) {
    warning("crit value is NA")
  } else if (crit < 0) {
    crit <- abs(crit)
    warning("crit < 0, abs(crit) used instead")
  }
  x_median <- median(x, na.rm = na.rm)
  x_mad <- mad(x, na.rm = na.rm)
  out_log <- abs(x - x_median) / x_mad > crit
  out_index <- which(out_log)
  return(list(value = x[out_index],
              index = out_index))
}
```

## Test!


```r
outlier(letters)
```

```
## Error in outlier(letters): x must be numeric
```

```r
outlier(test, c(4, 2))
```

```
## Warning in outlier(test, c(4, 2)): length(crit) > 1, only the first element
## was used
```

```
## $value
## [1] 631 610
## 
## $index
## [1]  99 100
```
## Test!

```r
outlier(rep(as.integer(NA), 50))
```

```
## Error in outlier(rep(as.integer(NA), 50)): x values are all NA
```

```r
outlier(test, -4)
```

```
## Warning in outlier(test, -4): crit < 0, abs(crit) used instead
```

```
## $value
## [1] 631 610
## 
## $index
## [1]  99 100
```

## Test!


```r
outlier(test, TRUE)
```

```
## Error in outlier(test, TRUE): crit must be numeric
```

```r
outlier(test, as.integer(NA))
```

```
## Warning in outlier(test, as.integer(NA)): crit value is NA
```

```
## $value
## integer(0)
## 
## $index
## integer(0)
```

## Document!

roxygen2-style -- ready for inclusion in a package!


```r
#' Get Robustly Identified Outliers
#'
#' @param x a numeric vector of data
#' @param crit a nonnegative numeric value
#' @param na.rm logical -- passed to median() and mad()
#' 
#' @details Calculates the median of x and the robustly estimated 
#' standard deviation of x, using the mad() function.  If the 
#' absolute difference between the median and a value of x is 
#' greater than crit robust standard deviations, then the value is 
#' considered an outlier.
#'
#' @return a list with two elements: value is a numeric vector of
#' outliers, and index is an integer vector of outlier indices
#' @export

outlier <- function(x, crit = 4, na.rm = FALSE) {
  if (!is.numeric(x)) {
    stop("x must be numeric")
  }
  if (all(is.na(x))) {
    stop("x values are all NA")
  }
  if (!is.numeric(crit)) {
    stop("crit must be numeric")
  }
  if (length(crit) > 1) {
    crit <- crit[1]
    warning("length(crit) > 1, only the first element was used")
  }
  if (is.na(crit)) {
    warning("crit value is NA")
  } else if (crit < 0) {
    crit <- abs(crit)
    warning("crit < 0, abs(crit) used instead")
  }
  x_median <- median(x, na.rm = na.rm)
  x_mad <- mad(x, na.rm = na.rm)
  out_log <- abs(x - x_median) / x_mad > crit
  out_index <- which(out_log)
  return(out_index)
}
```

## Default Returns

If you don't use the return() function, your function will return the result of the last statement.  


```r
add_five <- function(x) {
  x + 5
}
add_five(pi)
```

```
## [1] 8.141593
```

## Invisible Returns

Use the `invisible()` function, obviously:


```r
add_five <- function(x) {
  invisible(x + 5)
}
add_five(pi)
z <- add_five(sqrt(2))
z
```

```
## [1] 6.414214
```

## Dropping the Braces

If your function code is really short, you **can** skip the braces, and it will still work:


```r
add_pi <- function(x) x + pi
add_pi(cos(pi/2))
```

```
## [1] 3.141593
```

But, it is impossible to write robust, well-documented functions this way.

## Ephemeral Functions

Sometimes you don't write a function to last, and a sloppy function is good enough. 


```r
sapply(mtcars, FUN = function(x) median(abs(x - median(x))))
```

```
##     mpg     cyl    disp      hp    drat      wt    qsec      vs      am 
##  3.6500  2.0000 94.7500 52.0000  0.4750  0.5175  0.9550  0.0000  0.0000 
##    gear    carb 
##  1.0000  1.0000
```

## Watch out!

If you don't pay attention, your function will do things that you don't want.  Try to keep your functions **narrowly focused**:

- It is easier to write a small function that does one simple thing than a big function that does something complex
- So, if you have a complex task, write a bunch of simple functions first, then put them together (perhaps in a bigger function)

![Watch out for the unexpected!](http://i.giphy.com/hUgYyZwFNvr4A.gif)

## Environments

R objects are organized into environments.  Usually your code runs in the global environment.  Your function *can* reference objects in the global environment . . .


```r
z <- 5
double_z <- function() {
  return(z * 2)
}
double_z()
```

```
## [1] 10
```
but it is usually better to pass objects as arguments.


## Function Environments

But, your function code is evaluated in its own environment -- so objects that you create or modify generally have no effect on objects in the global environment:


```r
z <- 5
double_z <- function() {
  z <- z * 2
  return(z)
}
double_z()
```

```
## [1] 10
```

```r
z
```

```
## [1] 5
```

## Black Magic

Modifying or creating objects outside the function environment is **black magic**, and should be avoided...

![because it's real.](http://i.imgur.com/R7OnxlL.gif)

## The Obvious Alternative

Assign the function result to an object in the global environment


```r
z <- 5
double_it <- function(x) {
  return(x * 2)
}
z <- double_it(z)
z
```

```
## [1] 10
```

## Any Questions?

<img src="http://www.theshiznit.co.uk/media/2013/February/big-trouble-gifs/6-sign.gif" alt=" " width="800">

https://github.com/NickSalkowski/Writing_Functions_20170316
