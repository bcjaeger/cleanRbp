

test_that("standard inputs work", {

  mean_bps = runif(n = 100, min = 80, max = 190)
  sd_bps = runif(n = 100, min = 1, max = 3)

  expect_is(cmp_sdim_coefs(mean_bps, sd_bps), 'numeric')
  expect_is(cmp_sdim_values(mean_bps, sd_bps), 'numeric')
  expect_equal(length(cmp_sdim_values(mean_bps, sd_bps)), 100)

  mean_bps = runif(n = 1, min = 80, max = 190)
  sd_bps = runif(n = 1, min = 1, max = 3)

  expect_error(cmp_sdim_coefs(mean_bps, sd_bps), 'at least')
  expect_error(cmp_sdim_coefs(letters[1:5], letters[1:5]), 'numeric')

})
