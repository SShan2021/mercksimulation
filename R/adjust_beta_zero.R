#' Adjust Beta Zero
#'
#' Adjust beta zero so that the proportion of AE for the original dataset matches
#' the number of AEs in the generated outcome.
#'
#' @param df_original  The original sampled dataset with the AE outcome.
#' @param df_generated The generate y outcome.
#'
#' @return a value for adjust_beta_zero
#'
#' @examples
#' new_beta_zero <- adjust_beta_zero(data_x, data_y)
#'
#'
#'
#' @export
adjust_beta_zero <- function(df_original, df_generated){

  prop_original <- prop.table(table(df_original$AE))[2] #get the proportional of AE for the original dataset
  prop_generated <- prop.table(table(df_generated))[2] #get the proportional of AE for the generated dataset

  #if the proportions are within 10%, don't adjust beta_zero
  if( abs(prop_original - prop_generated) <= 0.1 ){
    beta_zero = 0
  }

  #if the proportions are greater than 10%
  #but within 20%, adjust beta_zero to 0.2
  else if( abs(prop_original - prop_generated) > 0.1 &
             abs(prop_original - prop_generated) <= 0.2){ #but within 20%, adjust beta_zero to 0.2
    beta_zero = -0.9
  }

  #otherwise adjust beta_zero to 0.4
  else {
    beta_zero = -1.7
  }

  #return beta_zero
  beta_zero

}
