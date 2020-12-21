#' get_extreme_middle
#'
#' @param data This should be a column or row of data
#'
#' @return Returns the average of the largest and the smallest number inside the data colomn/row
#' @export
#'
#' @examples
#' get_extreme_middle(c(1,2))
get_extreme_middle <- function(data = c(1,2,3,4)){
  highest = max(data)
  lowest = min(data)
  return(0.5*highest + 0.5*lowest)
}
