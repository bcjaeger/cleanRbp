test_that(
  "correct inputs work",
  {

    x <- c(110, 125, 120, NA, 110)

    expect_equal(
      mark_min(x, neighborhood_size = 0),
      c(1, 0, 0, 0, 0)
    )

    expect_equal(
      mark_max(x, neighborhood_size = 0),
      c(0, 1, 0, 0, 0)
    )

    expect_equal(
      mark_min(x, neighborhood_size = 1),
      c(1, 1, 0, 0, 0)
    )

    expect_equal(
      mark_max(x, neighborhood_size = 1),
      c(1, 1, 1, 0, 0)
    )

  }
)

test_that(
  "incorrect inputs get good error messages",
  {

    x <- c(110, 125, 120, NA, 110)

    expect_warning(
      mark_max(x = NA_real_, neighborhood_size = 1),
      'cannot determine max'
    )

    expect_warning(
      mark_min(x = NA_real_, neighborhood_size = 1),
      'cannot determine min'
    )

    expect_warning(
      mark_min(x = x, neighborhood_size = c(1,2)),
      'length 1'
    )

    expect_error(
      mark_max(letters),
      'must be numeric'
    )

    expect_error(
      mark_min(letters),
      'must be numeric'
    )

    expect_error(
      mark_max(x, letters),
      'must be numeric'
    )

    expect_error(
      mark_min(x, letters),
      'must be numeric'
    )

  }
)
