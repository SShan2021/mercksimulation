############################
#Sample from rows of dataframe with replacement
############################
sample_x <- function(df, n) {
  x <- df[sample(nrow(df), n, replace=TRUE),]
  x
}
