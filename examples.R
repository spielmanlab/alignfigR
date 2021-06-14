# Example code


#' Add together two numbers
#'
#' @param x A number
#' @param y A number
#' @return The sum of x and y
#' @examples
#' add(1, 1)
#' add(10, 1)
add <- function(x, y) {
  x + y
}


#' Brief description of what the function does
#'   (keep me blank)
#' @param x What is this argument? Note it's ACTUALLY x
#' @param y What is this argument? Note it's ACTUALLY y
#' @return Briefly what does the function return
#' @examples
#' at least one example of how one might use the function...IF THE FUNCTION IS INTENDED FOR USE BY USERS!!
add <- function(x, y) {
  x + y
}



# checking if 2 DECIMAL numbers are equal requires TOLERANCE
ZERO <- 1e-8
observed <- 0.99999999999999999
expected <- 1
abs(observed - expected) <= ZERO


