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




 expected.m1<-matrix(c(0.9555556, 0.9433333, 0.9088889, 0.8811111, 0.8511111, 0.8255556,
                       0.9241667, 0.9000000, 0.8575000, 0.8000000, 0.7908333, 0.7666667,
                       0.9666667, 0.9666667, 0.9666667, 0.9333333, 0.9091667, 0.8758333),
                       nrow=6, ncol=3, dimnames=list(row_names, col_names)) 
                      
 expected.m2<-matrix(c(0.01111111, 0.01888889, 0.04000000, 0.05000000, 0.06777778, 0.08111111,
                       0.00000000, 0.00000000, 0.00000000, 0.03333333, 0.03333333, 0.03333333,
                       0.04250000, 0.04250000, 0.07583333, 0.10916667, 0.10916667, 0.14250000),
                       nrow=6, ncol=3, dimnames=list(row_names, col_names)) 
 
 expected.m3<-matrix(c(0.01149425, 0.01970443, 0.04217448, 0.05375984, 0.07384777, 0.08932302,
                       0.00000000, 0.00000000, 0.00000000, 0.03448276, 0.03537562, 0.03667328,
                       0.04396552, 0.04485837, 0.08215198, 0.11428571, 0.12293956, 0.15651455),
                       nrow=6, ncol=3, dimnames=list(row_names, col_names)) 
 
 m<-conservation_value_seq_CI(x_boot, y_boot, seq=5, removal_rule=2, removal_routine="a")

  expect_equal(m$m1, expected.m1, tolerance=0.01)
  expect_equal(m$m2, expected.m2, tolerance=0.01) 
  expect_equal(m$m3, expected.m3, tolerance=0.01) 
})

