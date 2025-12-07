test_that("normal distribution solves Cholesterol Example (p.23)", {
  mu <- 210
  sigma <- 20
  
  # P[190<= X <= 230] (1 sigma less and more)
  prob <- normal_cdf(230, mu, sigma) - normal_cdf(190, mu, sigma)
  
  # expected:0.6827
  expect_equal(prob, 0.6826895, tolerance = 1e-4)
})

test_that("normal distribution solves Exam Example (p.29)", {
  mu <- 60
  sigma <- 10
  
  #  P(X >= 70)
  prob_ge_70 <- 1 - normal_cdf(70, mu, sigma)
  
  # expected: 0.1587
  expect_equal(prob_ge_70, 0.1587, tolerance = 1e-4)
})

test_that("normal_moments returns correct variance", {
  m <- normal_moments(mean = 10, sd = 3)
  expect_equal(m$variance, 9)
})