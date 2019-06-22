 data(community_data)
 test_that("test.occasional_occupancy", {
 data(community_data)
 comm.A<-community_data[which(community_data$environment == "A"),]
 comm.A <- comm.A[, 3:ncol(comm.A)]

 sp_names<-c("sp1","sp2","sp3","sp4","sp5","sp6","sp7","sp8","sp9","sp10","sp11","sp12","sp13","sp14","sp15","sp16","sp17","sp18","sp19","sp20","sp21","sp22","sp23","sp24","sp25","sp26","sp27","sp28","sp29","sp30")
 expected<-matrix(c(5,1,5,6,6,2,8,2,2,3,3,6,6,5,4,8,2,5,6,2,0,0,0,0,0,6,7,10,7,6,3,6,6,1,3,10,2,10,5,7,1,3,7,8,6,0,0,0,0,0,2,1,7,0,2,5,10,8,3,2,1,9,10,1,6,8,6,2,6,10,0,0,0,0,0,2,8,2,1,8,8,0,9,4,5,9,7,4,3,3,5,10,1,10,4,5,3,10,2,2,0,0,0,0,0,4,2,10,10,0,0,0,0,0,0,9,3,9,0,8,6,9,5,9,2,9,7,4,7,9,7,2,2,6,10,9,8,3,2,10,8,3,2,6,3,4,7,8,4,2),
            nrow=5, ncol=30)
 colnames(expected)<-sp_names

 res<- occasional_occupancy(comm.A,3)
  expect_equal(res, expected) 
})