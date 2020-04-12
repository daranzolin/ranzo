#' Casting functions
#'
#' @param data a table of data
#' @param ... columns
#'
#' @export
#' @rdname casting
cast_character <- function(data, ...) {
  dplyr::mutate_at(data, vars(...), cast_func('character'))
}

#' @rdname casting
#' @export
cast_numeric <- function(data, ...) {
  dplyr::mutate_at(data, vars(...), cast_func('numeric'))
}

#' @rdname casting
#' @export
cast_logical <- function(data, ...) {
  dplyr::mutate_at(data, vars(...), cast_func('logical'))
}

cast_func <- function(type) {
  f <- switch(type,
              "character" = as.character,
              "logical" = as.logical,
              "numeric" = as.numeric
  )
}
