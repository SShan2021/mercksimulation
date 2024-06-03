#' Simulate predictor values for dataset
#'
#' Simulate demographic covariates, drug, and drug interaction
#'
#' @param demographic_list The list of parameters that specify the ratio of
#' different demographic values in your simulated dataset
#' @param drug_list The names of the drugs that you want to include
#' in your dataset
#' @param mean_vec Specify your mean vector
#' @param covariance_matrix Specify your covariance matrix
#' @param threshold The value of the threshold which below the values for the
#' drugs are set to 0. Default value is 0.
#' @param n The number of rows you want in your dataset
#'
#'
#' @return A dataframe of rows for the generated dataset.
#'
#' @examples
#' demo <- c("TXGROUP.4", "TXGROUP.8", "TXGROUP.16",
#'           "SEX.F", "RACE.Black", "RACE.Hispanic", "RACE.Other")
#' perct <- c(1/4, 1/4, 1/4, 1/2, 1/5, 1/8, 3/40)
#' pl <- data.frame(Demographic = demo, Percentage = perct)
#'
#' drugs <- c("AAI", "BDG", "BIC", "YAC")
#' n_vars <- length(drugs)
#'
#' mean_vector <- rep(0.5, n_vars)
#'
#' rho <- 0.2
#' sigma_squared <- 0.24
#' cov_matrix <- matrix(nrow = n_vars, ncol = n_vars)
#' for (i in 1:n_vars) {
#'   for (j in 1:n_vars) {
#'     cov_matrix[i, j] <- rho^abs(i - j)
#'                       }
#'                     }
#' cov_matrix <- sigma_squared * cov_matrix
#'
#' data_x <- simulate_x(demographic_list = pl, drug_list = drugs,
#'                      mean_vec = mean_vector, covariance_matrix = cov_matrix,
#'                      threshold = 0, n = 1000)
#' @export

simulate_x <- function(demographic_list,
                       drug_list,
                       mean_vec,
                       covariance_matrix,
                       threshold = 0,
                       n) {

  #######################
  #simulate demographics
  #######################
  m <- dim(demographic_list)[1]
  demographic_data <- data.frame(matrix(NA, nrow = n, ncol = m))
  colnames(demographic_data) <- demographic_list$Demographic

  # Generate binary samples based on the proportions
  for (i in 1:m) {
    count <- as.integer(runif(n) < demographic_list[i,2])
    demographic_data[,i] <- count
  }

  # Simulate age
  demographic_data$AGE <- round(rnorm(n, mean = 50, sd = 10))

  #######################
  #simulate drugs
  #######################
  drugs_simulated <- mvrnorm(n = n, mu = mean_vec, Sigma = covariance_matrix)
  colnames(drugs_simulated) <- drug_list

  #Make all the values below the threshold 0:
  drugs_simulated[drugs_simulated < threshold] <- 0

  #######################
  #create the drug interactions
  #######################
  #Calculate the number of interaction terms
  d <- length(drug_list)
  num_interactions <- choose(d, 2)

  #Initialize an empty data frame to store the interaction terms
  interaction_df <- matrix(NA, nrow = nrow(drugs_simulated), ncol = num_interactions)

  #Initialize a vector to store column names
  interaction_colnames <- character(num_interactions)

  # Ensure that drugs_simulated's columns are in alphabetical order
  drugs_simulated <- drugs_simulated[, order(colnames(drugs_simulated))]

  #Initialize counter for the interaction term
  z <- 1

  # Loop over all pairs of columns to create interaction terms
  for(i in 1:(ncol(drugs_simulated)-1)) {
    for(j in (i+1):ncol(drugs_simulated)) {

      # Create the interaction column name
      interaction_name <- paste(colnames(drugs_simulated)[i], "_", colnames(drugs_simulated)[j], sep = "")
      interaction_colnames[z] <- interaction_name

      # Calculate the interaction and add it to the 'interaction_df' data frame
      interaction_df[,z] <- drugs_simulated[,i] * drugs_simulated[,j]


      # Increment the counter
      z <- z + 1
    }
  }

  #Add the colnames to interaction_df
  interaction_df <- as.data.frame(interaction_df)
  colnames(interaction_df) <- interaction_colnames

  #######################
  #Put it all together
  #######################
  data <- as.data.frame(cbind(demographic_data, drugs_simulated, interaction_df))

  data

}
