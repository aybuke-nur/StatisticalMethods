#' Moments of the Binomial Distribution B(n,p)
#' 
#' @description Compute the mean and variance for the binomial 
#' distribution with parameters n and p.
#' 
#' @param n Integer number of trials (must be >= 0).
#' @param p Probability of success (must be between 0 and 1).
#' 
#' @return A list with elements 'mean' and 'variance'.
#' 
#' @examples
#' binomial_moments(10, 0.5)
#' 
#' @export
binomial_moments <- function(n, p) {
  # error control
  if (length(n) != 1 || n < 0 || n %% 1 != 0) {
    stop("'n' must be a non-negative integer.")
  }
  if (length(p) != 1 || p < 0 || p > 1) {
    stop("'p' must be between 0 and 1.")
  }
  
  # Mean: mu = n * p
  mean_val <- n * p
  
  # Variance: sigma^2 = n * p * (1 - p)
  var_val <- n * p * (1 - p)
  
  # output as a list:
  list(mean = mean_val, variance = var_val)
}