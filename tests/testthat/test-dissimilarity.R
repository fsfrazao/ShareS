test_that("test.dissimilarity", {
  res<-dissimilarity(10,15,3)
  expect_equal(res$sor,0.4736842, tolerance = 0.01) 
  expect_equal(res$jac,0.6428571, tolerance = 0.01)
})
