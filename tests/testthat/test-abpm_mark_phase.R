test_that(
  "correct inputs work",
  {

    # count awake periods
    x <- c("A","A","A","S","S",NA,"A","A")
    x_complete <- c("A","A","A","S","S","A","A")
    x_factor <- factor(x)

    expect_equal(
      mark_period(x, "A", impute_from = 'left'),
      c(1,1,1,0,0,0,2,2)
    )

    expect_equal(
      mark_period(x_factor, "A", impute_from = 'left'),
      c(1,1,1,0,0,0,2,2)
    )

    expect_equal(
      mark_period(x_factor, factor("A"), impute_from = 'left'),
      c(1,1,1,0,0,0,2,2)
    )

    expect_equal(
      mark_period(x_complete, "A", impute_from = 'left'),
      c(1,1,1,0,0,2,2)
    )

    expect_equal(
      mark_period(x_complete, "A", impute_from = 'right'),
      c(1,1,1,0,0,2,2)
    )


    expect_equal(
      mark_period(x, "A", impute_from = 'right'),
      c(1,1,1,0,0,2,2,2)
    )

    expect_equal(
      mark_period(x_factor, "A", impute_from = 'right'),
      c(1,1,1,0,0,2,2,2)
    )

    # count all periods
    expect_equal(
      mark_period(x, c("A","S"), impute_from = 'left'),
      c(1,1,1,2,2,2,3,3)
    )

    expect_equal(
      mark_period(x, c("A","S"), impute_from = 'right'),
      c(1,1,1,2,2,3,3,3)
    )
  }
)

test_that(
  "incorrect inputs get good error message",
  {
    # count awake periods
    x_good <- c("A","A","A","S","S",NA,"A","A")
    x_bad <- 1:10

    expect_error(
      mark_period(x_bad, "A", impute_from = 'left'),
      regexp = 'must be a character or factor'
    )

    expect_error(
      mark_period(x_good, 1, impute_from = 'left'),
      regexp = 'must be a character or factor'
    )

    expect_error(
      mark_period(x_good, "1", impute_from = 1),
      regexp = 'must be a character value'
    )

    expect_error(
      mark_period(x_good, "1", impute_from = 'middle'),
      regexp = "'left' or 'right'"
    )

    expect_warning(
      mark_period(NA_character_, "A",impute_from = 'left'),
      regexp = "all values of x are NA"
    )

    expect_warning(
      mark_period(NA_character_, "A",impute_from = 'right'),
      regexp = "all values of x are NA"
    )




  }
)
