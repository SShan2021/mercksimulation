#' Simulate Data
#'
#' Takes in the original dataset and the parameter list and outputs a
#' simulated dataset.
#'
#' @param df The original dataset.
#' @param parameter_list A vector of values specifying size and betas for simulated dataset.
#' @param beta_zero The intercept variables (default is beta_zero = 0).
#'
#' @return
#'
#' @examples
#' p <- simulate_data(data, 1000, grid[10,])
#'
#'
#'
#' @export
simulate_data <- function(df, parameter_list, beta_zero = 0) {

  #extract the size of the simulated data
  n = parameter_list$n_total

  #simulate the predictors and outcome
  x <- sample_x(df, n)
  y <- sample_y(x, parameter_list, beta_zero)

  x$AE <- NULL         # remove existing outcome variable

  new <- data.frame(cbind(y, x))                 # create output dataframe with generated y and x
  names(new)[names(new) == 'y'] <- "AE"     # rename generated y to be AE
  new
}
