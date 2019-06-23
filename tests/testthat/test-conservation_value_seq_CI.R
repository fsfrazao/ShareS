 test_that("test.conservation_value_seq", {
 data(community_data)
 comm.A<-community_data[which(community_data$environment == "A"),]
 comm.A <- comm.A[, 3:ncol(comm.A)]
 comm.B<-community_data[which(community_data$environment == "B"),]
 comm.B <- comm.B[, 3:ncol(comm.B)]
 
 set.seed(8989)
 x_boot<-bootstrap(comm.A, 30)
 y_boot<-bootstrap(comm.B, 30)

 col_names<-c("m", "2.5%", "97.5%")
 row_names<-c(0,1,2,3,4,5)




 expected.m1<-matrix(c(0.9500000,0.9255556,0.9044444,0.8755556,0.8444444,
                       0.8100000,0.9333333,0.8666667,0.8333333,0.8241667,
                       0.7908333,0.7333333,0.9666667,0.9666667,0.9425000,
                       0.9333333,0.9000000,0.8666667),
                       nrow=6, ncol=3, dimnames=list(row_names, col_names)) 
                      
 expected.m2<-matrix(c(0.01444444,0.03111111,0.04,0.05000000,0.07222222,
                       0.10222222,0.00000000,0.00000000,0.00,0.03333333,
                       0.03333333,0.06666667,0.03333333,0.07583333,0.10,
                       0.10000000,0.10000000,0.17583333),
                       nrow=6, ncol=3, dimnames=list(row_names, col_names)) 
 
 expected.m3<-matrix(c(0.01494253,0.03251536,0.04238617,0.05403702,0.07885902,
                       0.11211844,0.00000000,0.00000000,0.00000000,0.03448276,
                       0.03571429,0.07142857,0.03448276,0.07844828,0.10446429,
                       0.10823413,0.11228632,0.19318783),
                       nrow=6, ncol=3, dimnames=list(row_names, col_names)) 
 
 m<-conservation_value_seq_CI(x.boot, y.boot, seq=5, removal_rule=2, removal_routine="a")

  expect_equal(m$m1, expected.m1, tolerance=0.01)
  expect_equal(m$m2, expected.m2, tolerance=0.01) 
  expect_equal(m$m3, expected.m3, tolerance=0.01) 
})

