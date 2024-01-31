#' Compute the MSE & Sensitivity & Specificity of the simulation.
#'
#' Takes in the prespecified parameter vector and compares it
#' to the simulated one using mean squared error.
#'
#' @param parameter_list The vector of parameters specifying beta values.
#' @param simulation_list The vector of parameters from the simulation
#' @param beta_num The total number of possible beta in the sample.
#'
#' @return A list object with mse, specificity, and sensitivity.
#'
#' @examples
#' validation_function(parameter_list = grid[7,],
#' simulation_list = output_model7_rep1000_n500.RData$coefficients[[100]])
#'
#'
#'
#' @export
validation_function <- function(parameter_list, simulation_list, beta_num){

  #########################################
  #make the parameter_list into a dataframe
  #########################################
  parameter_list <- as.data.frame(t(subset(parameter_list, select = -n_total)))
  parameter_list$DRUG <- rownames(parameter_list)
  colnames(parameter_list) <- c("COEF", "DRUG")
  rownames(parameter_list) <- 1:dim(parameter_list)[1]

  #########################################
  #convert coef data form to be the same for both
  #########################################
  parameter_list$COEF <- as.numeric(parameter_list$COEF)
  simulation_list$COEF <- as.numeric(simulation_list$COEF)

  #########################################
  #remove the intercept
  #########################################
  simulation_list <- simulation_list[!(simulation_list$DRUG == "(Intercept)"), ]

  #########################################
  #full bind the beta simulated to the beta real
  #########################################
  beta <- parameter_list %>%
    full_join(simulation_list, by = "DRUG")
  colnames(beta) <- c("COEF.PAR", "DRUG", "COEF.SIM")

  #########################################
  #fill in the NA with 0
  #########################################
  beta <- beta %>%
    mutate_all(~ifelse(is.na(.),
                       0,
                       .))

  #########################################
  #calculate the MSE (beta - beta_hat)^2
  #########################################
  mse <- mean((beta[,"COEF.PAR"] - beta[,"COEF.SIM"])^2)

  #########################################
  #calculate specificity
  #########################################
  #how many times does parameter_list != 0
  denom_spec <- beta_num - sum(ifelse(beta[,"COEF.PAR"] != 0, 1, 0))

  #how many times does simulated beta != 0 but parameter_list == 0
  num_spec <- denom_spec - sum(ifelse(beta[,"COEF.PAR"] == 0 & beta[,"COEF.SIM"] != 0, 1, 0))

  #calculate specificity
  if(denom_spec != 0) {
    specificity <- num_spec/denom_spec
  }
  else{
    specificity = 1
  }

  #########################################
  #calculate sensitivity
  #########################################
  #how many times does parameter_list != 0
  denom_sen <- sum(ifelse(beta[,"COEF.PAR"] != 0, 1, 0))

  #how many times do both simulated beta and parameter_list != 0
  num_sen <- sum(ifelse(beta[,"COEF.PAR"] != 0 & beta[,"COEF.SIM"] != 0, 1, 0))

  #calculate sensitivity
  if(denom_sen != 0) {
    sensitivity <- num_sen/denom_sen
  }
  else{
    sensitivity = 1
  }


  #output
  output <- data.frame(mse, specificity, sensitivity)
  output
}
