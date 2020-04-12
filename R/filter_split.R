#' Split a data frame on multiple conditions
#'
#' @param .x a table of data
#' @param ... conditions
#'
#' @return a list of data frames
#' @export
#'
#' @examples
#' filter_split(iris, Species == "versicolor", Species == "setosa")
filter_split <- function(.x, ...) {
  exprs <- rlang::enquos(...)
  expr_list <- list(exprs)
  purrr::map(expr_list[[1]], function(expr) dplyr::filter(.x, !!expr))
}
