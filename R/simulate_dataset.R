#' Create simulation data
#'
#' Creates new simulation data depending on the user specification for
#' mean and covariance.
#'
#' @param parameter_list The vector of parameters specifying beta values.
#' @param demographic_list The list of parameters that specify the ratio of
#' different demographic values in your simulated dataset.
#' @param drug_list The names of the drugs that you want to include
#' in your dataset.
#' @param mean_vec Specify your mean vector for the simulated dataset.
#' @param covariance_matrix Specify your covariance matrix for the simulated dataset.
#' @param threshold The value of the threshold which below the values for the
#' drugs are set to 0. Default value is 0.
#'
#' @return A simulated dataset with the prespecified betas.
#'
#' @export

simulate_dataset <- function(parameter_list,
                             demographic_list,
                             drug_list,
                             mean_vec,
                             covariance_matrix,
                             threshold = 0){

  #extract the size of the simulated data
  n <- parameter_list$n_total

  #simulate the predictors
  x <- simulate_x(demographic_list,
                  drug_list,
                  mean_vec,
                  covariance_matrix,
                  threshold,
                  n)

  #scale age
  x$AGE <- scale(x$AGE)


  #simulate the outcome
  y <- sample_y(x, parameter_list)

  #bind the dataset together
  new <- data.frame(cbind(y, x))
  names(new)[names(new) == 'y'] <- 'AE'

  #return the simulated dataset
  new

}
