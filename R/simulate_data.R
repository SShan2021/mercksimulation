############################
# Function to generate entire new dataset
############################
simulate_data <- function(df, n, beta, formula) {
  x <- sample_x(df, n)
  y <- sample_y(x, beta, beta_zero)

  dependent <- as.character(formula[[2]])        # get left hand side of formula
  x <- x %>% select(-all_of(dependent))          # remove existing outcome variable

  new <- data.frame(cbind(y, x))                 # create output dataframe with generated y and x
  names(new)[names(new) == 'y'] <- dependent     # rename generated y to lhs of formula
  new
}
