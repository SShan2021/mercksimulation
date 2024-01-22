############################
# Function to adjust for beta_zero
############################
adjust_beta_zero <- function(df_original, df_generated){

  prop_original <- prop.table(table(df_original$AE))[2] #get the proportional of AE for the original dataset
  prop_generated <- prop.table(table(df_generated))[2] #get the proportional of AE for the generated dataset

  if( abs(prop_original - prop_generated) <= 0.1 ){ #if the proportions are within 10%, don't adjust beta_zero
    beta_zero = 0
  } else if( abs(prop_original - prop_generated) > 0.1 & #if the proportions are greater than 10%
             abs(prop_original - prop_generated) <= 0.2){ #but within 20%, adjust beta_zero to 0.2
    beta_zero = -0.9
  } else {            #otherwise adjust beta_zero to 0.4
    beta_zero = -1.7
  }

  #return beta_zero
  beta_zero

}
