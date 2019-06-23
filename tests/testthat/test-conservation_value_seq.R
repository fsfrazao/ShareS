 test_that("test.conservation_value_seq", {
 data(community_data)
 comm.A<-community_data[which(community_data$environment == "A"),]
 comm.A <- comm.A[, 3:ncol(comm.A)]
 comm.B<-community_data[which(community_data$environment == "B"),]
 comm.B <- comm.B[, 3:ncol(comm.B)]

 
 expected.m1<-c(0.9666667, 0.9333333, 0.9333333, 0.9000000, 0.9000000, 0.8333333)
 expected.m2<-c(0.00000000, 0.03333333, 0.03333333, 0.03333333, 0.03333333, 0.10000000)
 expected.m3<-c(0.00000000, 0.03448276, 0.03448276, 0.03571429, 0.03571429, 0.10714286)
 set.seed(8989)
 m<-conservation_value_seq(comm.A, comm.B, 5, removal_rule=2, removal_routine="a")

  expect_equal(m$m1, expected.m1, tolerance=0.01)
  expect_equal(m$m2, expected.m2, tolerance=0.01) 
  expect_equal(m$m3, expected.m3, tolerance=0.01) 
})

