#' Performs n simulations and model generation.
#'
#' Takes in the original dataset and list of parameters and
#' perform n simulations as specified in the list of parameters.
#'
#' @param df The original dataset.
#' @param parameter_list The vector of parameters specifying beta values and n samples.
#' @param n_rep The number of simulations to run per scenario.
#' @param beta_num The number of total possible betas in the dataset.
#' @param replace Whether you want to sample with or without replacement.
#' Default is replace = TRUE.
#' @param iterations Number of iterations to run BKMR model. Default is 10000.
#' @param inclusion The % for inclusion for posterior inclusion probability. Default is 0.5
#'
#'
#' @return a list with the following elements:
#' \itemize{
#' \item{coefficients}
#' \item{validation}
#' }
#'
#' @examples
#' simulation_function(df = simulation, parameter_list = grid[1,])
#'
#'
#'
#' @export
simulation_function <- function(df,
                                parameter_list,
                                n_rep,
                                beta_num,
                                replace = TRUE,
                                iterations = 10000,
                                inclusion = 0.5){

  #initializes the list object:
  coefficient_rep_LASSO <- list()     #create list for storing the betas
  coefficient_rep_ELASTIC <- list()
  coefficient_rep_BKMR <- list()

  validation_rep_LASSO <- list()   #create list for storing validation values
  validation_rep_ELASTIC <- list()
  validation_rep_BKMR <- list()

  #number of samples
  n <- parameter_list$n_total

  #specify the intercept parameter
   x <- sample_x(df, n, replace = FALSE)
   y <- sample_y(x, parameter_list, beta_zero = 0)
   beta_zero <- adjust_beta_zero(x, y)

  #loop
  for (i in 1:n_rep){

    tryCatch ({

      #generate the data
      gendata <- simulate_data(df = df,
                               parameter_list = parameter_list,
                               beta_zero = beta_zero,
                               replace = FALSE)

      #perform LASSO variable selection on the betas
      model1 <- penalized_regression(df = gendata,
                                     model.type = "lasso")

      #perform Elastic Net variable selection on the betas
      model2 <- penalized_regression(df = gendata,
                                     model.type = "elastic net")

      #perform BKMR variabl selection on the betas
      model3 <- bkmr(df = gendata,
                     iterations = iterations,
                     inclusion = inclusion)

      #find the validation values
      model_validate_1 <- validation_function(df = gendata,
                                            parameter_list = parameter_list,
                                            simulation_list = model1,
                                            beta_num = beta_num)

      model_validate_2 <- validation_function(df = gendata,
                                              parameter_list = parameter_list,
                                              simulation_list = model2,
                                              beta_num = beta_num)

      model_validate_3 <- validation_function(df = gendata,
                                              parameter_list = parameter_list,
                                              simulation_list = model3,
                                              beta_num = beta_num,
                                              MSE = "not")


      #save the outputs
      coefficient_rep_LASSO[[i]] <- model1
      coefficient_rep_ELASTIC[[i]] <- model2
      coefficient_rep_BKMR[[i]] <- model3

      validation_rep_LASSO[[i]] <- model_validate_1
      validation_rep_ELASTIC[[i]] <- model_validate_2
      validation_rep_BKMR[[i]] <- model_validate_3

      #keep count
      print(paste0("I finished ", i))

      #go to the next
      i <<- i+1

    },

    #print out the error
    error = function(e){
      message(paste("An error occurred for item", i, ":\n"), e)

    })

  }

  # Return a list of results
  return(list("coefficients_LASSO" = coefficient_rep_LASSO,
              "coefficients_ELASTIC" = coefficient_rep_ELASTIC,
              "coefficients_coefficient_rep_BKMR" = coefficient_rep_BKMR,
              "validation_LASSO" = validation_rep_LASSO,
              "validation_ELASTIC" = validation_rep_ELASTIC,
              "validation_BKMR" = validation_rep_BKMR))
}
