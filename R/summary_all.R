#input: list output of the simulation_function
#output: list of summary statistics for MSE/SENS/SPEC (where)
#where you can call output$MSE_summary to get the summary
#for the MSE

summary_all <- function(df, model, n_rep, sample_size){

  #bind together the outputs
  MSE_rbind <- do.call(rbind,  df$MSE)            #MSE
  SENS_rbind <- do.call(rbind, df$sensitivity)   #Sensitivity
  SPEC_rbind <- do.call(rbind, df$specificity)   #Specificity

  #get the summaries
  MSE_sum <- summary_function(MSE_rbind)
  SENS_sum <- summary_function(SENS_rbind)
  SPEC_rbind <- summary_function(SPEC_rbind)

  #bind together
  output <- rbind(MSE_sum, SENS_sum, SPEC_rbind)
  output$type <- c("MSE", "sensitivity", "specificity")

  #add information about the model
  output$model <- model
  output$rep <- n_rep
  output$sample_size <- sample_size

  #here is the output
  output

}
