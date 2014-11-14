#' Adds 3.
#' 
#' @param x A numeric quantity.
#' @return The sum of \code{x} 3.
#' @examples
#' tortilla(5)
#' tortilla(1:5)
tortilla <- function(x) {
  return(x + 3)
}

##

cruchade <- function(x) {
  return(x + 1)
}

#' Returns a list of two elements.
#' 
#' @param x Any R object.
#' @param y Any R object.
#' @return A list with first element \code{x} and second element \code{y}.
#' @examples
#' polenta(10, 1)
#' polenta("10", 1:5)
polenta <- function(x, y) {
  return(list(a = x, b = y))
}