#' Reformat a string by pattern
#'
#' @param x a string
#' @param pattern a string pattern
#' @param pattern_replace the replacement character in the pattern
#'
#' @return a string
#' @export
#'
#' @examples
#' patternize(7072999216, "(XXX) XXX-XXXX")
patternize <- function(x, pattern, pattern_replace = "X") {
  stopifnot(is.character(pattern))
  stopifnot(grepl(pattern_replace, pattern))
  xs <- strsplit(as.character(x), "")[[1]]
  ps <- strsplit(pattern, "")[[1]]
  n_replace <- sum(ps == pattern_replace)
  if (length(xs) != n_replace) stop("number characters in x exceeds pattern replacements", call. = FALSE)
  x_ind <- 1
  for (i in seq_along(ps)) {
    if (ps[i] == pattern_replace) {
      ps[i] <- xs[x_ind]
      x_ind <- x_ind + 1
    }
  }
  paste(ps, collapse = "")
}
