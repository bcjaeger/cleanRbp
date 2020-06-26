
#' ABPM peaks/troughs
#'
#' @param x (numeric) ambulatory monitoring values
#' @param neighborhood_size (numeric) the number of points to include
#'   around the maximum or minimum value. If the maximum or minimum
#'   occurs on a boundary, then only neighbors within the boundary
#'   will be marked.
#'
#' @return a numeric vector the same length as `x` with values of
#'   0 or 1. A value of 1 indicates that the corresponding value of
#'   `x` is inside of the peak or trough.
#'
#' @export
#'
#' @examples
#'
#' x <- c(110, 125, 120, NA, 110)
#' mark_min(x, neighborhood_size = 0)
#' mark_max(x, neighborhood_size = 0)
#' mark_min(x, neighborhood_size = 1)
#' mark_max(x, neighborhood_size = 1)
#'
#' @note If there are ties, the value that appears
#  earliest is designated as the peak.


mark_max <- function(x, neighborhood_size = 1) {

  if(all(is.na(x))){
    warning(
      "cannot determine max of x - all values are missing",
      call. = FALSE
    )
    return(rep(0, length(x)))
  }

  .mark(x, neighborhood_size, which.max(x))
}

#' @rdname mark_max
#' @export

mark_min <- function(x, neighborhood_size = 1) {

  if(all(is.na(x))){
    warning(
      "cannot determine min of x - all values are missing",
      call. = FALSE
    )
    return(rep(0, length(x)))
  }

  .mark(x, neighborhood_size, which.min(x))
}

.mark <- function(x, neighborhood_size, index){

  if(!is.numeric(x)){
    stop("x must be numeric", call. = FALSE)
  }

  if(!is.numeric(neighborhood_size)){
    stop("neighborhood_size must be numeric", call. = FALSE)
  }

  if(length(neighborhood_size) > 1){

    neighborhood_size <- neighborhood_size[1]

    warning(
      "neighborhood size should be length 1.",
      "Only the first value will be used",
      call. = FALSE
    )
  }

  n <- length(x)
  output <- rep(0, n)

  lwr_index <- max(1, index - neighborhood_size)
  upr_index <- min(n, index + neighborhood_size)
  out_index <- seq(lwr_index, upr_index)

  output[out_index] <- 1

  output

}

