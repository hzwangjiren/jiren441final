#' Hypotenuse Length for Right Angle Triangle
#'
#' @param a This is the length of first right-angle side
#' @param b This is the length of second right-angle side
#'
#' @return The function returns the length of hypotenuse in the right angle triangle
#' @export
#'
#' @examples hypotenuse_length(2,2)
hypotenuse_length <- function(a = 1, b = 1){
  return(sqrt(a^2 + b^2))
}
