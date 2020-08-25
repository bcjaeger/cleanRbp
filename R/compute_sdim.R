
safe_log <- function(x){

  out <- rep(NA_real_, length(x))
  out[x == 0] <- 0
  out[x >  0] <- log(x[x > 0])
  out

}


#' Standard deviation independent of mean
#'
#' @param bp_mean a numeric vector of mean blood pressure values.
#' @param bp_sd a numeric vector of standard deviations corresponding to the
#'   mean blood pressure values.
#'
#' @return [cmp_sdim_coefs] returns the regression coefficients from
#'   a linear model fitted to the log-transformed values of `bp_mean`
#'   and `bp_sd` (i.e., `coef(lm(log_sd ~ log_mean))`). [cmp_sdim_values]
#'   uses the regression coefficients form [cmp_sdim_coefs] to compute
#'   the standard deviation of blood pressure independent from the mean.
#'
#' @note SDIM (standard deviation independent of the mean) is cohort-specific.
#'   So, if you remove a participant from the study, it will result in a
#'   new value of SDIM for everyone else.
#'
#' @export
#'
#' @examples
#' mean_bps = runif(n = 100, min = 80, max = 190)
#' sd_bps   = runif(n = 100, min = 1, max = 3)
#' cmp_sdim_coefs(mean_bps, sd_bps)
#' cmp_sdim_values(mean_bps, sd_bps)

cmp_sdim_coefs <- function(bp_mean, bp_sd, method = 'A'){

  if(length(bp_mean) != length(bp_sd))
    stop('bp_mean and bp_sd should have the same length', call. = FALSE)

  if(length(bp_mean) < 2)
    stop('at least 2 values are needed to fit a linear model', call. = FALSE)

  if(!is.numeric(bp_mean)) stop('bp_mean should be numeric', call. = F)
  if(!is.numeric(bp_sd)) stop('bp_sd should be numeric', call. = F)

  bp_okay <- all(stats::na.omit(bp_mean) > 0)
  sd_okay <- all(stats::na.omit(bp_sd) >= 0)

  if(!bp_okay) stop('all bp_mean values should be >0', call. = FALSE)
  if(!sd_okay) stop('all bp_sd values should be >=0', call. = FALSE)

  data.frame(
    bp_mean = bp_mean,
    bp_sd = bp_sd
  ) %>%
    tidyr::drop_na() %>%
    dplyr::transmute(
      log_mean = safe_log(bp_mean),
      log_sd = safe_log(bp_sd)
    ) %>%
    stats::lm(formula = log_sd ~ log_mean, data = .) %>%
    stats::coef()

}

#' @rdname cmp_sdim_coefs
#' @export
cmp_sdim_values <- function(bp_mean, bp_sd, method = 'A'){

  coefs <- cmp_sdim_coefs(bp_mean, bp_sd, method = method)

  slope <- coefs['log_mean']
  intercept <- coefs['(Intercept)']
  bp_mean_overall <- mean(bp_mean, na.rm = TRUE)


  switch(
    method,
    "A" = (bp_sd / (bp_mean^slope)) * bp_mean_overall^slope,
    "B" = intercept + bp_sd / (bp_mean^slope)
  )


}
