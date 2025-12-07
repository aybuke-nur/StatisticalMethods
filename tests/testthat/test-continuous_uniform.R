test_that("continuous_uniform solves the Bus Waiting Time example (p.17)", {
  # parameters: bus arrives beetwen 0-15 minutes
  a <- 0
  b <- 15
  
  # probability of waiting 5 to 10 minutes:
  # P(5 <= X <= 10) = CDF(10) - CDF(5)
  prob <- continuous_uniform_cdf(10, a, b) - continuous_uniform_cdf(5, a, b)
  
  # expected: 1/3
  expect_equal(prob, 1/3)
})
