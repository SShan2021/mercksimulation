############################
# Simulate log_odds of improvement event as a linear combination of predictors
############################
sample_y <- function(df, parameter_list, beta_zero = 0){

  X <- as.matrix(df)                        # makes the data into a matrix
  beta <- parameter_list[,!(colnames(parameter_list) %in% c("n_total", "INTERCEPT"))] # extract the betas

  X <- X[,colnames(beta)]    #extract the columns from the data that correspond to the betas

  log_odds <- beta_zero + X %*% t(beta)                    # mean response
  pr <- 1 / (1 + exp(-log_odds))            # pass through an inv-logit function
  y <- rbinom(nrow(df), 1, pr)              # bernoulli response variable
  y

}
