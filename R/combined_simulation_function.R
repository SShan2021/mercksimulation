############################
#Put it all together into ONE function
############################
#input: data, beta, formula, n_rep, sample_size, model (M1/M2/M3/M4)

combined_simulation_function <- function(df, beta, formula, n_rep, sample_size, model){

  #run the simulation
  sims <- simulation_function(df = df,
                              beta = beta,
                              formula = formula,
                              n_rep = n_rep,
                              sample_size = sample_size)

  #summary function
  sims_summary <- summary_all(df = sims,
                              model = model,
                              n_rep = n_rep,
                              sample_size = sample_size )
  #save to csv
  write.csv(sims_summary, file=paste("/work/users/s/o/sophshan/", model, "/", "SUMMARY_", model, "_rep", n_rep, "_n",
                                     sample_size, ".rds", sep=""))

  #save the outputs (coefficients, MSE, SENS, SPEC)
  save_rds(sims,
           model = model,
           n_rep = n_rep,
           sample_size = sample_size)
}
