
#' Compute average real variability
#'
#' @param time numeric vector indicating time of measurement
#' @param value numeric vector indicating measurement reading.
#' @param phase numeric vector indicating the phase of measurement.
#'
#' @export
#'
#' @examples
#' # Example 1 -----
#'
#' time <- c(1,2,3,4)
#' value <- c(0,1,3,6)
#' # time differences = 1, 1, 1
#' # value differences = 1, 2, 3
#' # ARV = (1/3) * (1*1 + 1*2 + 1*3) = 1/3 * 6 = 2
#' cmp_arv(time, value)
#'
#' # Example 2 -----
#'
#' time <- c(1,3,4,10)
#' value <- c(0,1,3,6)
#' # time differences = 2, 1, 6
#' # value differences = 1, 2, 3
#' # ARV = (1/9) * (2*1 + 1*2 + 6*3) = 1/9 * 22 = 2.444
#' cmp_arv(time, value)
#'
#' # Example 3 (same as 2 but with negative values) -----
#'
#' time <- c(1,3,4,10)
#' value <- c(0,1,3,6) * -1
#' # time differences = 2, 1, 6
#' # value differences = 1, 2, 3
#' # ARV = (1/9) * (2*1 + 1*2 + 6*3) = 1/9 * 22 = 2.444
#' cmp_arv(time, value)
#'
#' # Example 4 (missing values) -----
#'
#' time <- c(1, 2, 4, 6)
#' value <- c(0, 1, NA_real_, 2)
#' # time differences = 1, 4
#' # value differences = 1, 1
#' # ARV = (1/5) * (1*1 + 4*1) = 5/5 = 1
#' cmp_arv(time, value)
#'
#' # Example 5 (phases) -----
#'
#' time <- c(1,3,  4,10)
#'
#' value <- c(0,1,3,6)
#'
#' phase <- c(1,1,2,2)
#'
#' # time differences = 2, 6
#' # value differences = 1, 3
#' # ARV = {2 * [(1/2) * (2*1)] + 6 * [(1/6) * (6*3)]} / 8
#' #     = {2*1 + 6*3} / 8
#' #     = 20 / 8, i.e., 2.5
#'
#' cmp_arv(time, value, phase)
#'
#'
#' # Example 6 (phases) -----
#'
#' time <- c(1,3,12,  4,10,13)
#' value <- c(0,1,5,  3,6,1)
#' phase <- c(1,1,1,  2,2,2)
#'
#' # time differences = {2, 9}, {6, 3} - totals of 11 and 9
#' # value differences = {1, 4}, {3, -5}
#' # ARV = (11 * ((1/11) * (2*1 + 9*4)) + 9 * ((1/9) * (6*3 +3*5))) / 20 = 3.55
#'
#' cmp_arv(time, value, phase)

cmp_arv <- function(time, value, phase=NULL){

  # Stop if the time vector isn't the
  # same length as the value vector

  if(is.null(phase)) phase = rep(1, length(time))

  if( !all.equal(length(time), length(value),length(phase)) ) {
    stop("vectors are not of the same length", call. = FALSE)
  }

  # catch atypical inputs and return a missing value
  # (if these weren't caught, 0 would be returned)
  if(all(is.na(time))) return(NA_real_)
  if(length(time) <= 1) return(NA_real_)

  # stop if time/value are not numeric
  if(!is.numeric(time)) stop("time should be numeric")
  if(!is.numeric(value)) stop("value should be numeric")
  if(!is.numeric(phase)) stop("phase should be numeric")

  # Check vectors for missing values
  time_na_index <- which(is.na(time))
  value_na_index <- which(is.na(value))
  phase_na_index <- which(is.na(phase))

  # Find indices where at least one vector
  # has a missing value
  na_index <- list(
    time_na_index,
    value_na_index,
    phase_na_index
  ) %>%
    purrr::reduce(union)

  # If there are any missing values, remove these
  # indices in both time and value vectors
  if(length(na_index) > 0){

    time = time[-na_index]
    value = value[-na_index]
    phase = phase[-na_index]

  }

  weights <- tapply(time, phase, diff, simplify = FALSE) %>%
    purrr::keep(~length(.x) > 0)

  bad_weights <- purrr::map_lgl(weights, ~any(.x) <= 0)

  if (any(bad_weights)) {
    stop(
      "time differences contain negative ",
      "values or identical adjacent values",
      call. = FALSE
    )
  }

  # Now that inputs are checked...

  # get the absolute value of the differences in values
  # e.g., diff( c(1,2,4) ) = c(1,2)

  diff_vals <- tapply(value, phase, diff, simplify = FALSE) %>%
    purrr::map(.f = abs) %>%
    purrr::keep(~length(.x) > 0)

  # Compute the product of weights and diff_vals and then
  # divide each of these values by the total sum of weights
  rslts_unwtd <- purrr::map2(
    .x = weights,
    .y = diff_vals,
    .f = ~ .x * .y / sum(.x)
  )

  if(length(rslts_unwtd) == 1){
    # ARV is the sum of the weighted values
    return(sum(rslts_unwtd[[1]]))
  }

  rslts <- data.frame(time=time, phase=phase) %>%
    dplyr::group_by(phase) %>%
    dplyr::slice(1, dplyr::n()) %>%
    dplyr::summarize(wt = diff(time)) %>%
    dplyr::mutate(val = purrr::map_dbl(rslts_unwtd, sum))

  sum(rslts$wt * rslts$val) / sum(rslts$wt)

}

