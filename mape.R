#' mape
#'
#' @param backtestmodel This is the model used in backtesting
#'
#' @return The function returns the moving average percentage error in the backtesting model
#' @export
#'
#' @examples
mape <- function(backtestmodel){
  colMeans(abs(backtestmodel$error/(backtestmodel$error+backtestice$forecast)), na.rm = TRUE)
}
