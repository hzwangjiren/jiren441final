#' mean_binomial_sample
#'
#' @param samplesize This is the size of the random sample that you want to generate
#' @param n This is the number of trials in the binomial distribution
#' @param p This is the probability in the binomial distribution
#'
#' @return The function returns the mean of the random sample of binomial distribution generated.
#' @export
#'
#' @examples
#' mean_binomial_sample(50,10,0.5)
mean_binomial_sample <- function(samplesize, n = 2, p = 0.5){
  mean(rbinom(samplesize, n, p))
}
