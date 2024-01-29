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
#' save_rds(simulation_output, location = "/work/users/s/o/sophshan/simulation_sc12_n1000/results_sim/")
#'
#'
#'
#' @export
save_rds <- function(simulation_output, parameter_list, model, n_rep, location){

  #extract the n_rep from the parameter_list
  n <- parameter_list$n_total

  #save the files
  save(simulation_output, file=paste(location, "output_model", model, "_rep", n_rep,
                                     "_n", n, ".RData", sep=""))
}
