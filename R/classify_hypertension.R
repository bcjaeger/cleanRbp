
#' Classify hypertension
#'
#' @param sbp (numeric) -- systolic blood pressure values
#' @param dbp (numeric) -- diastolic blood pressure values
#' @param sbp_thresh (numeric) -- a threshold for systolic blood pressure.
#'   A value of `sbp` exceeding `sbp_thresh` indicates hypertension
#' @param dbp_thresh (numeric) -- a threshold for diastolic blood pressure.
#'   A value of `dbp` exceeding `dbp_thresh` indicates hypertension
#' @param strictly_greater (logical) -- Should hypertension only be indicated
#'   if a `sbp` or `dbp` value is strictly greater (i.e., not equal to) the
#'   corresponding threshold value? (default is `FALSE`)
#' @param labels (character) -- How should normotensive and hypertensive
#'   observations be labeled? Default is "normo" and "hyper" for normotensive
#'   and hypertensive blood pressure readings, respectively.
#'
#' @return a factor with two levels, one indicating normotension and the
#'   other indicating hypertension, created using the given systolic
#'   and diastolic blood pressure thresholds.
#' @export
#'
#' @examples
#'
#' sbp <- c(120, 130, 140)
#' dbp <- c(70, 80, 90)
#'
#' htn_classify(sbp, dbp)
#'

htn_classify <- function(
  sbp,
  dbp,
  sbp_thresh = 130,
  dbp_thresh = 80,
  strictly_greater = FALSE,
  labels = c("normotension","hypertension")
) {

  if( length(sbp) != length(dbp) ) {
    stop("sbp/dbp vectors must be the same length", call. = FALSE)
  }

  # stop if time/value are not numeric
  if(!is.numeric(sbp) | !is.numeric(dbp)) {
    stop("sbp/dbp values must be numeric", call. = FALSE)
  }

  if(!is.numeric(sbp_thresh) | !is.numeric(dbp_thresh)) {
    stop("sbp/dbp thresholds must be numeric", call. = FALSE)
  }

  if(!is.logical(strictly_greater)){
    stop("strictly_greater must be logical", call. = FALSE)
  }

  if(!is.character(labels)){
    stop("labels must be a character vector of length 2", call. = FALSE)
  }

  if(length(labels) != 2){
    stop("length of labels must be 2", call. = FALSE)
  }

  if(strictly_greater){
    sbp_over <- sbp > sbp_thresh
    dbp_over <- dbp > dbp_thresh
  } else {
    sbp_over <- sbp >= sbp_thresh
    dbp_over <- dbp >= dbp_thresh
  }

  hyp_clsf <- as.numeric(sbp_over | dbp_over)

  factor(hyp_clsf, levels = c(0,1), labels = labels)

}
