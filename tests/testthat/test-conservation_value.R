test_that("test.dissimilarity", {
  res<-conservation_value(10,15,3)
  expect_equal(res$m1,0.3571429, tolerance = 0.01) 
  expect_equal(res$m2,0.5357143, tolerance = 0.01)
  expect_equal(res$m3,0.6, tolerance = 0.01)
})

