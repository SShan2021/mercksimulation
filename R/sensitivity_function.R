############################
#Describe the sensitivity the model
############################
#if we have specified 10 variables, how many did we actually
#catch (in other words, both beta_sim and beta_real != 0 divided
#beta_real != 0

sensitivity_function <- function(beta_sim, beta_real){

  #convert coef data form to be the same for both
  beta_sim$COEF <- as.numeric(beta_sim$COEF)
  beta_real$COEF <- as.numeric(beta_real$COEF)

  #left bind the beta simulated to the beta real
  beta <- beta_real %>%
    left_join(beta_sim, by = "DRUG")

  #fill in the NA with 0
  beta <- beta %>%
    mutate_all(~ifelse(is.na(.),
                       0,
                       .))


  #how many times does beta_real != 0
  denom <- sum(ifelse(beta[,2] != 0, 1, 0))

  #how many times do both beta_sim and beta_real != 0
  num <- sum(ifelse(beta[,2] != 0 & beta[,3] != 0, 1, 0))

  #calculate sensitivity
  if(denom != 0) {
    sensitivity <- num/denom
  }
  else{
    sensitivity = 0
  }


  #output
  sensitivity

}
