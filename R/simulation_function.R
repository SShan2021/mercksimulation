#' Performs n simulations and model generation.
#'
#' Takes in the original dataset and list of parameters and
#' perform n simulations as specified in the list of parameters.
#'
#' @param df The original dataset.
#' @param parameter_list The vector of parameters specifying beta values and n samples.
#' @param n_rep The number of simulations to run per scenario.
#' @param beta_num The number of total possible betas in the dataset.
#' @param lasso.value Specifies which lambda you want to use: lambda.min or lambda.1se.
#' The default is lasso.value = "lambda.min"
#' @param cutoff The cutoff to consider coefficients to put into the final model.
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
                                lasso.value = "lambda.min",
                                cutoff = NULL){

  #initializes the list object:
  coefficient_rep <- list()     #create list for storing the betas
  validation_rep <- list()   #create list for storing validation values

  #number of samples
  n <- parameter_list$n_total

  #specify the intercept parameter
   x <- sample_x(df, n)
   y <- sample_y(x, parameter_list, beta_zero = 0)
   beta_zero <- adjust_beta_zero(x, y)

  #loop
  for (i in 1:n_rep){

    tryCatch ({

      #generate the data
      gendata <- simulate_data(df = df,
                               parameter_list = parameter_list,
                               beta_zero = beta_zero)

      #perform LASSO variable selection on the betas
      model <- lasso_function(df = gendata,
                              lasso.value = lasso.value)

      #find the validation values
      model_validate <- validation_function(df = gendata,
                                            parameter_list = parameter_list,
                                            simulation_list = model,
                                            beta_num = beta_num,
                                            cutoff = cutoff)

      #save the outputs
      coefficient_rep[[i]] <- model
      validation_rep[[i]] <- model_validate


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
  return(list("coefficients" = coefficient_rep,
              "validation" = validation_rep))
}
