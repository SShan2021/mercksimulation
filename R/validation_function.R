#' Compute the MSE & Sensitivity & Specificity of the simulation.
#'
#' Takes in the prespecified parameter vector and compares it
#' to the simulated one using mean squared error.
#'
#' @param df The dataset with the predictors and outcome.
#' @param parameter_list The vector of parameters specifying beta values.
#' @param simulation_list The vector of parameters from the simulation
#' @param beta_num The total number of possible beta in the sample.
#' @param cutoff The cutoff to consider coefficients to put into the final model.
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
validation_function <- function(df,
                                parameter_list,
                                simulation_list,
                                beta_num,
                                cutoff = NULL){

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
  #remove the intercept and any coefficient below the cutoff
  #########################################
  simulation_list <- simulation_list[!(simulation_list$DRUG == "(Intercept)"), ]
  if(!is.null(cutoff)){
    simulation_list <- simulation_list[!(simulation_list$COEF < cutoff), ]}

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
  #refit the model using the beta_simulated
  #########################################
  #extract the covariates and outcomes
  x <- subset(df, select = -AE)
  y <- subset(df, select = AE)

  keep_X <- simulation_list$DRUG #extract the column names from the simulation
  keep_X <- keep_X[!keep_X == "(Intercept)"]
  x <- x[,keep_X] #extract the columns from the beta simulated
  new_data <- cbind(x, y)
  new_outcome <- coef(summary(glm(AE ~ ., data = new_data, family = "binomial"))) %>%
    as.data.frame()

  # Filter for significant coefficients (p-value < 0.05)
  significant_coefficients <- new_outcome %>%
    filter(`Pr(>|z|)` < 0.05) %>%
    rownames_to_column(var = "DRUG") %>%
    select(DRUG, COEF = Estimate) %>%
    filter(!(DRUG == "(Intercept)")) #get rid of the intercept

  #########################################
  #full-bind the significant beta to the real beta
  #########################################
  beta_v1 <- parameter_list %>%
    full_join(significant_coefficients, by = "DRUG")
  colnames(beta_v1) <- c("COEF.PAR", "DRUG", "COEF.SIM")

  #########################################
  #fill in the NA with 0
  #########################################
  beta_v1 <- beta_v1 %>%
    mutate_all(~ifelse(is.na(.),
                       0,
                       .))

  #########################################
  #calculate specificity
  #########################################
  #how many times does parameter_list != 0
  denom_spec <- beta_num - sum(ifelse(beta_v1[,"COEF.PAR"] != 0, 1, 0))

  #how many times does simulated beta != 0 but parameter_list == 0
  num_spec <- denom_spec - sum(ifelse(beta_v1[,"COEF.PAR"] == 0 & beta_v1[,"COEF.SIM"] != 0, 1, 0))

  #calculate specificity
  if(denom_spec != 0) {
    specificity <- num_spec/denom_spec
  } else{
    specificity = 1
  }

  #########################################
  #calculate sensitivity
  #########################################
  #how parameters specified in the model
  denom_sen <- sum(ifelse(beta_v1[,"COEF.PAR"] != 0, 1, 0))

  #how many times do both simulated beta and parameter_list != 0
  num_sen <- sum(ifelse(beta_v1[,"COEF.PAR"] != 0 & beta_v1[,"COEF.SIM"] != 0, 1, 0))

  #calculate sensitivity
  if(denom_sen != 0) {
    sensitivity <- num_sen/denom_sen
  } else{
    sensitivity = 0
  }


  #output
  output <- data.frame(mse, specificity, sensitivity)
  output
}
