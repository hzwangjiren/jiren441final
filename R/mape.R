#' Moving Average Percentage Error
#'
#' @param backtestmodel This is the model used in backtesting
#'
#' @return The function returns the moving average percentage error in the backtesting model
#' @export
#'
#' @examples
mape <- function(backtestmodel){
  colMeans(abs(backtestmodel$error/(backtestmodel$error+backtestmodel$forecast)), na.rm = TRUE)
}
