#' Sample predictor values from dataset
#'
#' Sample n rows with replacement from the dataset
#'
#' @param df The dataset you want to sample from.
#' @param n The number of rows you want in your sample.
#'
#' @return A dataframe of rows from the original dataset.
#'
#' @examples
#' data_x <- sample_x(data, 1000)
#'
#'
#'
#' @export
sample_x <- function(df, n) {
  x <- df[sample(nrow(df), n, replace=TRUE),]
  x
}
