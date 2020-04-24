context('Low-level functionality')

test_that("GetBSpline computes correct value", {
  time <- 2000
  theta <- c(0.0000, 683.7634, 171.1121, 828.2901, 1015.1668, 935.0453, 1058.9732, 1182.9012)
  kOrder <- 4
  modelSplineN <- 8
  myKnots <- c(1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015)
  k <- 4
  minYear <- 1980
  maxYear <- 2015

  expect_equal(
    GetBSpline(time, theta, kOrder, modelSplineN, myKnots, minYear, maxYear),
    366.08365
  )
  expect_equal(
    GetBSplineCubic(time, theta, modelSplineN, myKnots, k),
    366.08365
  )
})
