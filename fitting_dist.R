#' fitting_dist
#'
#' @param data This is the data used in the plot
#' @param color This is the color of the lines
#'
#' @return This function returns a plot of histogram, plus two fitting lines on top
#' @export
#'
#' @examples fitting_dist(c(1,2,3,4,5,6,7,8,9,10,11,12,1,1,1,3,3,3,5,5,5,6,7,4,6,7,8))
fitting_dist = function(data, color="skyblue2"){
  fn <- fitdistrplus::fitdist(data, "norm")
  fln <- fitdistrplus::fitdist(data, "lnorm")
  plot.legend <- c("Normal", "Log-Normal")
  fitdistrplus::denscomp(list(fn, fln), legendtext = plot.legend)
}
