#' Sum x in y
#'
#' @param x a vector
#' @param y a vector
#'
#' @return numeric, sum of x in y
#' @export
#'
#' @examples
#' si(c("A", "B", "C"), LETTERS)
si <- function(x,y) {sum(x %in% y)}

#' Sum x not in y
#'
#' @param x a vector
#' @param y a vector
#'
#' @return numeric, sum of x not in y
#' @export
#'
#' @examples
#' si(c("A", "B", "C"), LETTERS)
sni <- function(x,y) {sum(!x %in% y)}

#' Length of unique values
#'
#' @param x a vector
#'
#' @return numeric, length of unique values
#' @export
#'
#' @examples
#' lu(letters[1:10])
lu <- function(x) {length(unique(x))}

#' Get distinct values and pull from data frame
#'
#' @param .data a table of data
#' @param var variable name
#'
#' @return a vector
#' @export
#'
#' @examples
#' dp(iris, Species)
dp <- function(.data, var) {
  v <- rlang::enquo(var)
  out <- dplyr::distinct(.data, !!v)
  out <- dplyr::pull(out, !!v)
  out
}
