
test_that("continuous_uniform_moments formulas are correct", {
  # Standard Uniform U(0,1):
  m <- continuous_uniform_moments(0, 1)
  
  # mean: (0+1)/2 = 0.5
  expect_equal(m$mean, 0.5)
  
  # variance: (1-0)^2 / 12 = 1/12
  expect_equal(m$variance, 1/12)
})