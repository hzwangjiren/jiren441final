#' compare_response
#'
#' @param res1 This is the first value the user inputs
#' @param res2 This is the second value the user inputs
#'
#' @return This returns a line with the result of comparison of two inputs
#' @export
#'
#' @examples
#' compare_response()
compare_response <- function(res1, res2){
  res1 = readline(prompt("Please input the 1st number  "))
  res2 = readline(prompt("Please input the 2nd number  "))
  if (res1 > res2){
    output <- c("Your 1st input is bigger!")
  }
  if (res1 == res2){
    output <- c("Your inputs are equal.")
  }
  else {
    output <- c("Your 2nd input is bigger!")
  }
  return(output)
}
