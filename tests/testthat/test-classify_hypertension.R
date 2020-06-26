
test_that(
  "correct inputs work",
  {

    sbp <- c(120, 130, 140, 135)
    dbp <- c(70, 80, 90, 65)
    levels = c('normotension','hypertension')

    expect_equal(
      htn_classify(sbp, dbp, strictly_greater = FALSE),
      factor(c('normotension', 'hypertension', 'hypertension', 'hypertension'), levels = levels)
    )

    expect_equal(
      htn_classify(sbp, dbp, strictly_greater = TRUE),
      factor(c('normotension', 'normotension', 'hypertension', 'hypertension'), levels = levels)
    )

  }
)

test_that(
  "incorrect inputs get good errors",
  {

    sbp <- c(120, 130, 140, 135)
    dbp <- c(70, 80, 90, 65)
    levels = c('normotension','hypertension')

    expect_error(
      htn_classify(1, dbp, strictly_greater = FALSE),
      regexp = 'must be the same length'
    )

    expect_error(
      htn_classify(as.character(sbp), dbp, strictly_greater = TRUE),
      regexp = 'must be numeric'
    )

    expect_error(
      htn_classify(sbp, dbp, sbp_thresh = '130', strictly_greater = TRUE),
      regexp = 'must be numeric'
    )

    expect_error(
      htn_classify(sbp, dbp, strictly_greater = 'TRUE'),
      regexp = 'must be logical'
    )

    expect_error(
      htn_classify(sbp, dbp, strictly_greater = T, labels = 1:2),
      regexp = 'must be a character'
    )

    expect_error(
      htn_classify(sbp, dbp, strictly_greater = T, labels = letters[1:3]),
      regexp = 'length of labels must be 2'
    )

  }
)
