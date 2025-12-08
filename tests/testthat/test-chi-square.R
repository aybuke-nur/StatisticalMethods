test_that("chi_square CDF matches book critical value (Chapter 6, p.76)", {
  df <- 4
  critical_value <- 9.488
  
  #The 9.488 given in the pdf, should be equal to %95 trust level (0.05 significance).
  probability <- chi_square_cdf(critical_value, df)
  
  # Should be really close to 0.95
  expect_equal(probability, 0.95, tolerance = 1e-3)
})

test_that("chi_square_moments returns correct formulas", {
  df <- 10
  m <- chi_square_moments(df)
  
  # Mean = df = 10
  expect_equal(m$mean, 10)
  
  # Variance = 2 * df = 20
  expect_equal(m$variance, 20)
})