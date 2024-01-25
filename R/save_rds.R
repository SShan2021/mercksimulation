#' Save RData Objects
#'
#' Take in the output from the simulation_function and
#' save it to the location specified.
#'
#' @param simulation_output The list object returned by the simulation_function.
#' @param parameter_list The vector of parameters specifying beta values.
#' @param model The scenario that we're running
#' @param n_rep The number of repetitions per simulation that we're running. 
#' @param location The directory to save the objects to.
#'
#' @return
#'
#' @examples
#' save_rds(simulation_output, location = "/work/users/s/o/sophshan/")
#'
#'
#'
#' @export
save_rds <- function(simulation_output, parameter_list, model, n_rep, location){

  #extract the n_rep from the parameter_list
  n_rep <- parameter_list$n_total

  #bind together the outputs
  validation_rbind <- do.call(rbind, simulation_output$validation)

  #save the files
  save(df$coefficients, file=paste(location, model, "/", "coefficients_", model, "_rep", n_rep, "_n",
                                      sample_size, ".RData", sep=""))
  save(validation_rbind, file=paste(location, model, "/", "validation_", model, "_rep", n_rep, "_n",
                                sample_size, ".RData", sep=""))
}
