#' Model Selection Using Lasso
#'
#' Takes in the generated dataset and fits a lasso model using
#' cross-validation.
#'
#' @param df The dataset with the predictors and outcome.
#' @param family.value Specifies the family for running the lasso model.
#' The default is family.value = "binomial".
#' @param alpha.value Specifies the default value for alpha in running
#' the lasso model.
#'
#' @return
#' Returns the coefficient dataframe for the dataset.
#'
#' @examples lasso_function(df, family.value = "binomial",
#' alpha.value = 1)
#'
#'
#'
#'
#' @export
lasso_function <- function(df, family.value = "binomial", alpha.value = 1){

  #extract the covariates and outcomes
  x <- subset(df, select = -AE)
  y <- subset(df, select = AE)

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
