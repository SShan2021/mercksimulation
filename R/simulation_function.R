############################
#Function to generate n simulations
############################
#inputs: data, true_beta, formula, n_rep(# replications),
#sample_size (size of the bootstrap)
#returns: MSE, SENS, SPEC, coefficients

simulation_function <- function(df, beta, formula, n_rep, sample_size){

  #initializes the list object:
  coefficient_rep <- list()     #create list for storing the betas
  MSE_rep <- list()   #create list for storing MSE
  SENS_rep <- list() #create list for storing sensitivity
  SPEC_rep <- list()   #create list for storing specificity

  #create a dataframe of true betas with their coef dimnames
  beta_value <- cbind(colnames(df),
                      beta) %>%
    as.data.frame()
  colnames(beta_value) <- c("DRUG", "COEF")


  #loop
  for (i in 2:n_rep){

    tryCatch ({

      #generate the data
      gendata <- simulate_data(df = df,
                               n = sample_size,
                               beta = beta,
                               formula = formula)

      #perform LASSO variable selection on the betas
      model <- lasso_function(x = gendata[,-1],
                              y = gendata[,1])

      #find the MSE
      model_MSE <- mse_function(beta_sim = model,
                                beta_real = beta_value)

      #find the sensitivity
      model_SENS <- sensitivity_function(beta_sim = model,
                                         beta_real = beta_value)

      #find the specificity
      model_SPEC <- specificity_function(beta_sim = model,
                                         beta_real = beta_value)


      #save the outputs
      coefficient_rep[[i]] <- model
      MSE_rep[[i]] <- model_MSE
      SENS_rep[[i]] <- model_SENS
      SPEC_rep[[i]] <- model_SPEC


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
              "MSE" = MSE_rep,
              "sensitivity" = SENS_rep,
              "specificity" = SPEC_rep))
}
