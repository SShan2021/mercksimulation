#' Describe the MSE of the model
#'
#' This function ....
#'
#' @param beta_sim
#input: vector of covariates of model on simulated
#data, vector

mse_function <- function(beta_sim,
                         beta_real){

  #convert coef data form to be the same for both
  beta_sim$COEF <- as.numeric(beta_sim$COEF)
  beta_real$COEF <- as.numeric(beta_real$COEF)

  #left bind the beta simulated to the beta real
  beta <- beta_real %>%
    left_join(beta_sim, by = "DRUG")

  #fill in the NA with 0
  beta <- beta %>%
    mutate_all(~ifelse(is.na(.),
                       0,
                       .))

  #calculate the MSE (beta - beta_hat)^2
  mse <- mean((beta[,2] - beta[,3])^2)

  #output
  mse
}
