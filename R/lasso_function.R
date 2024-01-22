############################
#Performs LASSO regression
############################

#input: x, y, family, alpha
#output: dataframe of coefficients of best model

lasso_function <- function(x, #dataframe of covariates
                           y, #vector of outcomes
                           family.value = "binomial", #specify the default family
                           #to be binomial
                           alpha.value = 1 #specify the default alpha
                           #to be 1
){

  #create a data matrix from the dataframe of covariates
  input = data.matrix(x)

  #create a data matrix from vector of outcomes
  output = data.matrix(y)

  #perform k-fold cross-validation to find optimal lambda
  #value
  cv_model <- cv.glmnet(x = input, y = output, alpha = alpha.value,
                        family = family.value)

  #find optimal lambda value that minimizes test MSE
  best_lambda <- cv_model$lambda.min

  #find the best model
  best_model <- glmnet(x = input, y = output, alpha = alpha.value,
                       lambda = best_lambda,
                       family = family.value)

  #extract the coefficients of the best model
  coef_best_model <- coef(best_model)

  #create a dataframe with the coefficients of the best model
  #along with their names
  coef_data_frame <- data.frame(DRUG = coef_best_model@Dimnames[[1]][coef_best_model@i + 1],
                                COEF = coef_best_model@x)

  #output
  coef_data_frame
}
