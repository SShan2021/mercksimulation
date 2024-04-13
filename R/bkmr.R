#' Model Selection Using BKMR
#'
#' Takes in the generated dataset and fits a model using
#' bayesian kernel machine regression. 
#'
#' @param df The dataset with the predictors and outcome.
#' @param family.value Default fits a binomial model. 
#' @param iterations Number of iterations to run BKMR model. Default is 10000.
#'
#' @return
#' Returns the PIP for the dataset.
#'
#' @examples bkmr(df, family.value = "binomial")
#'
#'
#'
#'
#' @export
bkmr <- function(df,
                 family.value = "binomial",
                 iterations = 10000){
  
  ############################
  #outcome 
  ############################
  y <- as.matrix(subset(df, select = AE))

  ############################
  #demographics  
  ############################
  demo <- c("AGE", "TXGROUP.4", "TXGROUP.8", "TXGROUP.16",
            "SEX.F", "RACE.Black", "RACE.Hispanic", "RACE.Other")
  
  covariates <- as.matrix(df[,demo])

  ############################
  #drugs
  ############################  
  # Regular Expression to match exactly three capital letters
  pattern <- "^[^A-Z]*([A-Z][^A-Z]*){3}$"
  
  # Find column names matching the pattern
  drugs <- as.matrix(df[,names(df)[grepl(pattern, names(df)) & names(df) != "AGE"]])

  ############################
  #generate knots 
  ############################  
  knots <- fields::cover.design(drugs, nd = dim(df)[1]/10)$design
  
  ############################
  #estimation of the BKMR model
  ############################
  temp <-  kmbayes(y=y, Z=drugs, X=covariates, iter=iterations, verbose=FALSE, 
                   varsel = TRUE, family = family.value,
                   est.h = TRUE)
  
  ############################
  #investigate model convergence 
  ############################
  #Evaluate the convergence of the parameters 
 # sel<-seq(1000,10000,by=1) #after removing in burn-in of 1000
  
  #TracePlot(fit = temp, par = "beta", sel=sel)
  
  ############################
  #estimated posterior inclusion probabilities 
  ############################
  pips <- as.data.frame(ExtractPIPs(temp))
  
  pips <- pips %>%
    filter(PIP > 0.99)
  
  colnames(pips) <- c("DRUG", "COEF")
  
  pips
}
