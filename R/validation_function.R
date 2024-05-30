#' Compute the MSE & Sensitivity & Specificity of the simulation.
#'
#' Takes in the prespecified parameter vector and compares it
#' to the simulated one using mean squared error.
#'
#' @param df The dataset with the predictors and outcome.
#' @param parameter_list The vector of parameters specifying beta values.
#' @param simulation_list The output matrix of parameters from the simulation
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
                                simulation_list){

  #########################################
  #make the parameter_list into a dataframe
  #########################################
  parameter_list <- as.data.frame(t(subset(parameter_list, select = -n_total)))
  parameter_list$DRUG <- rownames(parameter_list)
  colnames(parameter_list) <- c("COEF", "DRUG")
  rownames(parameter_list) <- 1:dim(parameter_list)[1]

  #########################################
  #make the simulation_list into a dataframe
  #########################################
  simulation_list <- as.data.frame(as.matrix(simulation_list))
  simulation_list$DRUG <- rownames(simulation_list)
  colnames(simulation_list) <- c("COEF", "DRUG")
  rownames(simulation_list) <- 1:dim(simulation_list)[1]

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
  #create another beta dataset with only drugs/drug interactions
  #########################################
  beta.subset <- beta %>%
    filter(!(DRUG %in% c("TXGROUP.4",
                         "TXGROUP.8",
                         "TXGROUP.16",
                         "SEX.F",
                         "RACE.Black",
                         "RACE.Hispanic",
                         "RACE.Other",
                         "AGE")))


  #########################################
  #calculate the MSE (beta - beta_hat)^2
  #########################################
  mse_all <- mean((beta[,"COEF.PAR"] - beta[,"COEF.SIM"])^2)
  mse_subset <- mean((beta.subset[,"COEF.PAR"] - beta.subset[,"COEF.SIM"])^2)

  #########################################
  #refit the model using the beta_simulated
  #########################################
  #extract the covariates and outcomes
  x <- subset(df, select = -AE)
  y <- subset(df, select = AE)

  #extract non-zero covariates from simulation
  keep_X <- simulation_list$DRUG[simulation_list$COEF != 0]
  x <- x[,keep_X] #extract the columns from the beta simulated
  new_data <- cbind(x, y)
  new_outcome <- coef(summary(glm(AE ~ ., data = new_data, family = "binomial"))) %>%
    as.data.frame()

  # Filter for significant coefficients (p-value < 0.05)
  significant_coefficients <- new_outcome %>%
    filter(`Pr(>|z|)` < 0.05) %>%
    rownames_to_column(var = "DRUG") %>%
    filter(!(DRUG == "(Intercept)")) #get rid of the intercept

  significant_coefficients <- significant_coefficients[ , c("DRUG", "Estimate")]
  colnames(significant_coefficients) <- c("DRUG", "COEF")

  #########################################
  #exclude the non-drug columns in real beta and significant beta
  #########################################
  # Define the vector of column names to exclude
  not_select <- c("n_total", "TXGROUP.4", "TXGROUP.8", "TXGROUP.16",
                  "SEX.F", "RACE.Black", "RACE.Hispanic", "RACE.Other",
                  "AGE", "AE")


  # Subset the data frame to exclude the specified columns and transpose the result
  parameter_list <- data.frame(parameter_list[!(parameter_list$DRUG %in% not_select), ])
  significant_coefficients <- data.frame(significant_coefficients[!(significant_coefficients$DRUG %in% not_select), ])

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

  #print(beta_v1)

  #########################################
  #calculate number of betas (not including intercept or demographics)
  #########################################
  #select drug and drug interactions from original dataframe
  b <- colnames(df)[!( colnames(df) %in% not_select)]

  #calculate length
  beta_num <- length(b)

  #select drugs only from original dataframe
  beta_num_d <- length(b[!grepl("_", b)])

  #select drug interactions only from original dataframe
  beta_num_i <- length(b[grepl("_", b)])

  #########################################
  #calculate specificity (overall)
  #########################################
  #how many times does parameter_list == 0
  denom_spec <- beta_num - sum(ifelse(beta_v1[,"COEF.PAR"] != 0, 1, 0))

  #how many times does simulated beta != 0 but parameter_list == 0
  num_spec <- denom_spec - sum(ifelse(beta_v1[,"COEF.PAR"] == 0 &
                                        beta_v1[,"COEF.SIM"] != 0, 1, 0))

  #calculate specificity
  if(denom_spec != 0) {
    specificity <- num_spec/denom_spec
  } else{
    specificity = NA
  }

  #########################################
  #calculate specificity (drug)
  #########################################
  #extract only the drugs
  beta_d <- beta_v1[!grepl("_", beta_v1$DRUG), ]

  #how many times does parameter_list == 0
  denom_spec_d <- beta_num_d - sum(ifelse(beta_d[,"COEF.PAR"] != 0, 1, 0))

  #how many times does simulated beta != 0 but parameter_list == 0
  num_spec_d <- denom_spec_d - sum(ifelse(beta_d[,"COEF.PAR"] == 0
                                          & beta_d[,"COEF.SIM"] != 0, 1, 0))

  #calculate specificity
  if(denom_spec_d != 0) {
    specificity_d <- num_spec_d/denom_spec_d
  } else{
    specificity_d = NA
  }

  #########################################
  #calculate specificity (drug-interaction)
  #########################################
  #extract only the drugs interactions
  beta_i <- beta_v1[grepl("_", beta_v1$DRUG), ]

  #how many times does parameter_list == 0
  denom_spec_i <- beta_num_i - sum(ifelse(beta_i[,"COEF.PAR"] != 0, 1, 0))

  #how many times does simulated beta != 0 but parameter_list == 0
  num_spec_i <- denom_spec_i - sum(ifelse(beta_i[,"COEF.PAR"] == 0 & beta_i[,"COEF.SIM"] != 0, 1, 0))

  #calculate specificity
  if(denom_spec != 0) {
    specificity_i <- num_spec_i/denom_spec_i
  } else{
    specificity_i = NA
  }

  #########################################
  #calculate sensitivity (overall)
  #########################################
  #how parameters specified in the model
  denom_sen <- sum(ifelse(beta_v1[,"COEF.PAR"] != 0, 1, 0))

  #how many times do both simulated beta and parameter_list != 0
  num_sen <- sum(ifelse(beta_v1[,"COEF.PAR"] != 0 & beta_v1[,"COEF.SIM"] != 0, 1, 0))

  #calculate sensitivity
  if(denom_sen != 0) {
    sensitivity <- num_sen/denom_sen
  } else{
    sensitivity = NA
  }


  #########################################
  #calculate sensitivity (drugs)
  #########################################
  #how parameters specified in the model
  denom_sen_d <- sum(ifelse(beta_d[,"COEF.PAR"] != 0, 1, 0))

  #how many times do both simulated beta and parameter_list != 0
  num_sen_d <- sum(ifelse(beta_d[,"COEF.PAR"] != 0 & beta_d[,"COEF.SIM"] != 0, 1, 0))

  #calculate sensitivity
  if(denom_sen_d != 0) {
    sensitivity_d <- num_sen_d/denom_sen_d
  } else{
    sensitivity_d = NA
  }

  #########################################
  #calculate sensitivity (drugs-interactions)
  #########################################
  #how parameters specified in the model
  denom_sen_i <- sum(ifelse(beta_i[,"COEF.PAR"] != 0, 1, 0))

  #how many times do both simulated beta and parameter_list != 0
  num_sen_i <- sum(ifelse(beta_i[,"COEF.PAR"] != 0 & beta_i[,"COEF.SIM"] != 0, 1, 0))

  #calculate sensitivity
  if(denom_sen_i != 0) {
    sensitivity_i <- num_sen_i/denom_sen_i
  } else{
    sensitivity_i = NA
  }

  #########################################
  #output
  #########################################
  output <- data.frame(mse_all, mse_subset,
                       specificity_d, specificity_i, specificity,
                       sensitivity_d, sensitivity_i, sensitivity)
  output
}
