 test_that("test.cv_coef", {
 data(community_data)
 comm.A<-community_data[which(community_data$environment == "A"),]
 comm.A <- comm.A[, 3:ncol(comm.A)]
 comm.B<-community_data[which(community_data$environment == "B"),]
 comm.B <- comm.B[, 3:ncol(comm.B)]
 res<- cv_coef(comm.A, comm.B)
  expect_equal(res$shared,29) 
  expect_equal(res$unique_x,0) 
  expect_equal(res$unique_y,1) 
})
