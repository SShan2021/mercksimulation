#' Generate the response variable from the dataset
#'
#' Use the predictors to generate an outcome based on the logistic model.
#'
#' @param df The dataframe of predictors.
#' @param parameter_list The vector of parameters specifying beta values.
#' @param beta_zero The intercept parameter (default is 0).
#'
#' @return A vector of generated outcome variables
#'
#' @examples
#' sample_y(data_x, grid[10,])
#'
#'
#'
#' @export
sample_y <- function(df, parameter_list, beta_zero = 0){

  X <- as.matrix(df)                        # makes the data into a matrix
  beta <- parameter_list[,!(colnames(parameter_list) %in% c("n_total", "INTERCEPT"))] # extract the betas

  X <- X[,colnames(beta)]    #extract the columns from the data that correspond to the betas

  log_odds <- beta_zero + X %*% t(beta)                    # mean response
  pr <- 1 / (1 + exp(-log_odds))            # pass through an inv-logit function
  y <- rbinom(nrow(df), 1, pr)              # bernoulli response variable
  y

}
