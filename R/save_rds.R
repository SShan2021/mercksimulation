############################
#Function to save RDS
############################
#input: list output of the simulation_function
#output: combines the rows and saves the objects to separate RDS files

save_rds <- function(df, model, n_rep, sample_size){

  #bind together the outputs
  MSE_rbind <- do.call(rbind,  df$MSE)            #MSE
  SENS_rbind <- do.call(rbind, df$sensitivity)   #Sensitivity
  SPEC_rbind <- do.call(rbind, df$specificity)   #Specificity

  #save the files
  saveRDS(df$coefficients, file=paste("/work/users/s/o/sophshan/", model, "/", "coefficients_", model, "_rep", n_rep, "_n",
                                      sample_size, ".rds", sep=""))
  saveRDS(MSE_rbind, file=paste("/work/users/s/o/sophshan/", model, "/", "MSE_", model, "_rep", n_rep, "_n",
                                sample_size, ".rds", sep=""))
  saveRDS(SENS_rbind, file=paste("/work/users/s/o/sophshan/", model, "/", "SENS_", model, "_rep", n_rep, "_n",
                                 sample_size, ".rds", sep=""))
  saveRDS(SPEC_rbind, file=paste("/work/users/s/o/sophshan/", model, "/", "SPEC_", model, "_rep", n_rep, "_n",
                                 sample_size, ".rds", sep=""))

}
