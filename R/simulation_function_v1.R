#' Performs n simulations and model generation.
#'
#' Creates a new simulated dataset each time and performs model selection.
#'
#' @param parameter_list The vector of parameters specifying beta values and n samples.
#' @param n_rep The number of simulations to run per scenario.
#' @param demographic_list The list of parameters that specify the ratio of
#' different demographic values in your simulated dataset.
#' @param drug_list The names of the drugs that you want to include
#' in your dataset.
#' @param mean_vec Specify your mean vector for the simulated dataset.
#' @param covariance_matrix Specify your covariance matrix for the simulated dataset.
#' @param threshold The value of the threshold below which the values for the
#' drugs are set to 0. Default value is 0.
#' @param max_lambda Specifies the maximum lambda considered for LASSO.
#' Default is 10.
#' @param file_paths The output file names to add results to. Needs to be two files:
#' one for the coefficients from the original model, one with the validation
#' information.
#'
#' @export

simulation_function_v1 <- function(parameter_list,
                                   n_rep,
                                   demographic_list,
                                   drug_list,
                                   mean_vec,
                                   covariance_matrix,
                                   threshold = 0,
                                   max_lambda = 10,
                                   file_paths){

  #####################
  # Open a connection to the file(s) in append mode
  #####################
  file_conns <- lapply(file_paths, function(file_path) {
    file(file_path, open = "a")
  })

  #####################
  # Number of samples
  #####################
  n <- parameter_list$n_total

  #####################
  # Loop
  #####################
  for (i in 1:n_rep) {

    tryCatch({

      #####################
      # Generate the data
      #####################
      gendata <- simulate_dataset(parameter_list,
                                  demographic_list,
                                  drug_list,
                                  mean_vec,
                                  covariance_matrix,
                                  threshold)

      #####################
      # Perform LASSO variable selection with CV (lambda min)
      #####################
      method1 <- penalized_regression(df = gendata,
                                     model.type = "lasso",
                                     max_lambda = max_lambda,
                                     method = "CV",
                                     opt_lambda = "min")

      #####################
      # Perform LASSO variable selection with CV (lambda 1se)
      #####################
      method2 <- penalized_regression(df = gendata,
                                     model.type = "lasso",
                                     max_lambda = max_lambda,
                                     method = "CV",
                                     opt_lambda = "1se")

      #####################
      # Perform LASSO variable selection with BIC
      #####################
      method3 <- penalized_regression(df = gendata,
                                     model.type = "lasso",
                                     max_lambda = max_lambda,
                                     method = "BIC")

      #####################
      # Perform ELASTIC NET variable selection with CV (lambda min)
      #####################
      method4 <- penalized_regression(df = gendata,
                                     model.type = "elastic net",
                                     max_lambda = max_lambda,
                                     method = "CV",
                                     opt_lambda = "min")

      #####################
      # Perform ELASTIC NET variable selection with CV (lambda 1se)
      #####################
      method5 <- penalized_regression(df = gendata,
                                     model.type = "elastic net",
                                     max_lambda = max_lambda,
                                     method = "CV",
                                     opt_lambda = "1se")

      #####################
      # Perform ELASTIC NET variable selection with BIC
      #####################
      method6 <- penalized_regression(df = gendata,
                                     model.type = "elastic net",
                                     max_lambda = max_lambda,
                                     method = "BIC")

      #####################
      # Convert method to data frames and add method identifiers
      #####################
      method1_df <- as.data.frame(as.matrix(method1))
      method1_df$method <- "method1"

      method2_df <- as.data.frame(as.matrix(method2))
      method2_df$method <- "method2"

      method3_df <- as.data.frame(as.matrix(method3))
      method3_df$method <- "method3"

      method4_df <- as.data.frame(as.matrix(method4))
      method4_df$method <- "method4"

      method5_df <- as.data.frame(as.matrix(method5))
      method5_df$method <- "method5"

      method6_df <- as.data.frame(as.matrix(method6))
      method6_df$method <- "method6"

      #####################
      # Bind together results
      #####################
      coef_results <- cbind(method1_df, method2_df, method3_df,
                            method4_df, method5_df, method6_df)

      #####################
      # Write the data frame to the corresponding file
      #####################
      write.table(coef_results, file = file_conns[[1]], sep = ",", col.names = NA,
                  append = TRUE, quote = FALSE, row.names = TRUE)

      #####################
      # Validation function
      #####################
      m1 <- validation_function(df = gendata,
                                parameter_list = parameter_list,
                                simulation_list = method1)
      m2 <- validation_function(df = gendata,
                                parameter_list = parameter_list,
                                simulation_list = method2)
      m3 <- validation_function(df = gendata,
                                parameter_list = parameter_list,
                                simulation_list = method3)
      m4 <- validation_function(df = gendata,
                                parameter_list = parameter_list,
                                simulation_list = method4)
      m5 <- validation_function(df = gendata,
                                parameter_list = parameter_list,
                                simulation_list = method5)
      m6 <- validation_function(df = gendata,
                                parameter_list = parameter_list,
                                simulation_list = method6)

      #####################
      # Bind together results and convert to data frame
      #####################
      validation_results <- rbind(
        cbind(m1, method = "method1"),
        cbind(m2, method = "method2"),
        cbind(m3, method = "method3"),
        cbind(m4, method = "method4"),
        cbind(m5, method = "method5"),
        cbind(m6, method = "method6")
      )

      validation_results <- as.data.frame(validation_results)

      #####################
      # Write the data frame to the corresponding file
      #####################
      write.table(validation_results, file = file_conns[[2]], sep = ",", col.names = FALSE,
                  append = TRUE, quote = FALSE, row.names = TRUE)

      #####################
      # Keep count
      #####################
      print(paste0("I finished ", i))

    },
    error = function(e) {
      message(paste("An error occurred for item", i, ":\n"), e)
    })

  }

  #####################
  # Close the file connections
  #####################
  lapply(file_conns, close)

}

