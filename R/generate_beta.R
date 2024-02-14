#' Generate a random beta vector
#'
#' Generate a random beta vector for input into the grid function.
#' Specify the number of zeros, and the number of parameters you want.
#'
#' @param length_zero How models you want withOUT that variable as a predictor.
#' @param length_value How models you want WITH that variable as a predictor.
#' @param min The minimum value for generation
#' @param max The maximum value for generation
#'
#' @return A vector of values for the variable.
#'
#' @examples
#' TXGROUP.X <- generate_beta(length_zero = 3, length_value = 9,
#'                            min = -1, max = 1)
#'
#'
#'
#' @export
generate_beta <- function(length_zero, length_value,
                          min, max) {
  beta_value <- runif(1, min = min, max = max)
  beta <- c(rep(0, length_zero), rep(beta_value, length_value))
}
