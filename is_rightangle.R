#' Check if Triangle has a Right Angle
#'
#' @param a Length of 1st side, doesn't need to be the longest or shortest
#' @param b Length of 2nd side, doesn't need to be the longest or shortest
#' @param c Length of 3rd side, doesn't need to be the longest or shortest
#'
#' @return The function returns true if it is right angle triangle, else false.
#' @export
#'
#' @examples is_rightangle(6,8,10)
is_rightangle = function(a = 3, b = 4, c = 5){
  output = FALSE
  if (a^2 + b^2 == c^2){
    output = TRUE
  }
  if (a^2 + c^2 == b^2){
    output = TRUE
  }
  if (b^2 + c^2 == a^2){
    output = TRUE
  }
  return(output)
}
