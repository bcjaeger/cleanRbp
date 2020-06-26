
#' ABPM Periods
#'
#' During ABPM (ambulatory blood pressure monitoring), a person wears an
#'   ABPM device for a number of hours - usually 24. In most cases,
#'   this duration includes two periods where the person is awake and one
#'   period where they are asleep. For example, I woke up on Monday at
#'   7am, began wearing my ABPM device on Monday at 9am, went to bed on
#'   Monday at 11pm, woke up on Tuesday at 7am, and took off my ABPM device
#'   on Tuesday at 9am. My first awake period is from 9am to 11pm on Monday.
#'   My second awake period is from 7am to 9am on Tuesday. My only asleep
#'   period is from 11pm Monday to 7am Tuesday.
#'
#'   In practice, it is helpful to label these periods. This framework
#'   makes it easier to compute blood pressure means and variability during
#'   specific periods, and filter out periods that aren't part of the
#'   intended 24-hour measurement. For example, suppose I had worn the
#'   ABPM device for another day and on the second night I slept at different
#'   times. If I had only given my sleep diary for the first night, then
#'   my second night of ABPM data would be mislabeled! In this case,
#'   it would be safer to include only my first night of blood pressure
#'   readings.
#'
#' @param x (character) status values, e.g. `c('Awake', 'Asleep', 'Awake')`
#'
#' @param period_value (character) what status values should constitute
#'   a period? Status values not included in `period_value` will be
#'   ignored.
#'
#' @param impute_from (character) if 'left', then missing values in `x`
#'   will be filled in using the nearest value to the left (i.e., earlier).
#'   If 'right', then missing values are filled in using the nearest value
#'   on the right (i.e., later). Missing values on the boundaries are always
#'   filled in using the right if the first value is missing and the left
#'   if the last value is missing.
#'
#' @return a numeric vector with values of 0, 1, 2, and so on.
#'   A value of 0 indicates that the corresponding `x` value
#'   was not in `period_value`. A value i > 0 indicates that the
#'   corresponding `x` value was in the ith ABPM period.
#' @export
#'
#' @examples
#' # count awake periods
#' x <- c("A","A","A","S","S",NA,"A","A")
#'
#' mark_period(x, "A")
#' # count awake and asleep periods
#' x <- c("A","A","A","S","S",NA,"A","A")
#'
#' # count awake periods, imputing from the right
#' x <- c("A","A","A","S","S",NA,"A","A")
#'
#' mark_period(x, c("A","S"), impute_from = 'right')#'
#'
#' mark_period(x, c("A","S"))
#'


mark_period <- function(
  x,
  period_value,
  impute_from = c('left', 'right')
){

  if(is.factor(x)) x <- as.character(x)

  if(!is.character(x)) {
    stop("x must be a character or factor variable", call. = FALSE)
  }

  if(is.factor(period_value)) period_value <- as.character(period_value)

  if(!is.character(period_value)) {
    stop("period value must be a character or factor variable", call. = FALSE)
  }

  if(!is.character(impute_from)) {
    stop("impute_from must be a character value", call. = FALSE)
  }

  impute_from <- tolower(impute_from[1])

  if( !(impute_from %in% c('left','right')) ) {
    stop("impute_from must be 'left' or 'right'", call. = FALSE)
  }

  x <- switch (impute_from,
    'left' = fill_na_left(x),
    'right' = fill_na_right(x)
  )

  all_vals <- unique(x)

  ignore <- setdiff(all_vals, period_value)

  # count the number of consecutive status values
  runs = rle(x)

  # initialize output
  vals <- rep(NA_real_, length(runs$values))

  # set the ignore values to zero
  if(!is.null(ignore)){
    vals[runs$values %in% ignore] <- 0
  }

  # count the total number of periods
  nperiods <- sum(is.na(vals))

  # list the periods out in order
  vals[is.na(vals)] <- 1:nperiods

  # return a vector with length equal to the input
  rep(vals, runs$lengths)

}


fill_na_left <- function(x){


  if(all(is.na(x))){
    warning(
      "Could not fill NA values because all values of x are NA",
      call. = FALSE
    )
    return(x)
  }

  if(!any(is.na(x))) return(x)

  # Fill in first x values from the right if necessary
  obs <- which(!is.na(x))
  x[1:min(obs)] <- x[min(obs)]

  # Fill in other x values from the left
  mis <- which(is.na(x))

  for(i in mis) x[i] <- x[i-1]

  x

}

fill_na_right <- function(x){

  if(all(is.na(x))){
    warning("Could not fill NA values because all values of x are NA")
    return(x)
  }

  if(!any(is.na(x))) return(x)

  n <- length(x)

  # Fill in last x values from the left if necessary
  obs <- which(!is.na(x))
  x[n:max(obs)] <- x[max(obs)]

  # Fill in other x values from the left
  mis <- which(is.na(x))

  for(i in rev(mis)) x[i] <- x[i+1]

  x

}


# x <- c(1, NA, 2)
#
# fill_na_left(x)
# fill_na_right(x)
#
# x <- c(1, NA, NA, 2)

