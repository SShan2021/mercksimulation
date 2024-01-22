############################
#Summary Statistics for the MSE/SENS/SPEC
############################
#for calling within the summary_all function
#input: a dataframe
#output: summary

summary_function <- function(df){

  #use dplyr to get the min, 1st quantile, median, mean,
  #sd, 3rd quantile, max
  output <- df %>%
    as.data.frame() %>%
    summarise(min = min(V1),
              quant_1st = quantile(V1, probs = c(0.25)),
              median = median(V1),
              mean = mean(V1),
              sd = sd(V1),
              quant_3rd = quantile(V1, probs = c(0.75)),
              max = max(V1)) %>%
    round(.,6)

  #here is the output
  output
}
