#' Createas a confidence interval when sequentially removes occasional species according to increasing abundance and/or occupancy classes.
#' 
#'
#' This function takes bootstrapped communities (created with the 'bootstrap' function') and sequenctially removes occasional species 
#' according to increasing abundance and/or occupancy classes and calculates conservation value metrics.
#
#  The conservation value metrics are those produced by the 'conservation_value' function:
#' m1-> the proportion of all species that is shared between two communities
#' m2-> the proportion of all species that is unique to one of the two communities
#' m3-> the proportion of the species that occur in one of the communities that is unique to that community
#' Each metric will be calulated multiple times for each of the removal classes (1 to 'seq'), according to the number
#' of randomizations in the bootstrapped communities.
#'
#' @param x_boot A set of bootstrapped community matrices, with species in columns and samples in rows for each community in the 3rd axis
#' @param y_boot A set of bootstrapped community matrices, with species in columns and samples in rows for each community in the 3rd axis
#' @param seq The maximum removal class to be removed. The sequence will wil go from zero to this class (inclusive)
#' @param removal_rule The removal_rule to be used.If 1, applies the removal routine to x and y. If 2, applies on to y.
#' @param removal_routine The removal_routine to be used. 
#' If "a", removes by abundance class using the 'occasional_abundance' function.
#' If "o", removes by occupancy class using the 'occasional_occupancy' function.
#' If "b", removes by abundance and then occupancy, using the same occasional class for both.
#' @return A list with $m1, $m2 and $m3 matrices, each with seq rows and 3 columns the mean value for that metric (m), the lower (2.5%) and upper (97.5%) limit of the confidence interval
#' @importFrom stats quantile
#' @export
#' @examples
#' data(community_data)
#'
#' #Select all samples in Evironment 'A'
#' comm.A<-community_data[which(community_data$environment == "A"),]
#'
#' # Select only the abundance matrix (columns 1 and 2 contain the 'environment' and 'sample' data)
#' comm.A <- comm.A[, 3:ncol(comm.A)]
#'
#' #Select all samples in Evironment 'B'
#' comm.B<-community_data[which(community_data$environment == "B"),]
#'
#' # Select only the abundance matrix (columns 1 and 2 contain the 'environment' and 'sample' data)
#' comm.B <- comm.B[, 3:ncol(comm.B)]
#'
#' Create 30 bootstrapped communities for each of the two original communities (comm.A and comm.B)
#' x_boot<-bootstrap(comm.A, 30)
#' y_boot<-bootstrap(comm.B, 30)
#'  # Sequentially remove abundance classes from both communities and and calculate the conservation metrics with 95% confidence intervals
#' conservation_value_seq_CI(x.boot, y.boot, 5)
#' $m1
#'          m      2.5%     97.5%
#' 0 0.9500000 0.9333333 0.9666667
#' 1 0.9255556 0.8666667 0.9666667
#' 2 0.9044444 0.8333333 0.9425000
#' 3 0.8755556 0.8241667 0.9333333
#' 4 0.8444444 0.7908333 0.9000000
#' 5 0.8100000 0.7333333 0.8666667
#' 
#' $m2
#'            m       2.5%      97.5%
#' 0 0.01444444 0.00000000 0.03333333
#' 1 0.03111111 0.00000000 0.07583333
#' 2 0.04000000 0.00000000 0.10000000
#' 3 0.05000000 0.03333333 0.10000000
#' 4 0.07222222 0.03333333 0.10000000
#' 5 0.10222222 0.06666667 0.17583333
#' 
#' $m3
#'            m       2.5%      97.5%
#' 0 0.01494253 0.00000000 0.03448276
#' 1 0.03251536 0.00000000 0.07844828
#' 2 0.04238617 0.00000000 0.10446429
#' 3 0.05403702 0.03448276 0.10823413
#' 4 0.07885902 0.03571429 0.11228632
#' 5 0.11211844 0.07142857 0.19318783

conservation_value_seq_CI<-function(x_boot,y_boot,seq,removal_rule=2,removal_routine="a"){

if (dim(x_boot)[3]!=dim(y_boot)[3]) stop("x_boot and y_boot must have the same number of randomizations") else rand=dim(x_boot)[3]

results<-array(dim=c(seq+1,3,3))

a<-vector(length=rand)
b<-vector(length=rand)
c<-vector(length=rand)

m1<-vector(length=rand)
m2<-vector(length=rand)
m3<-vector(length=rand)


	for (i in 1:(seq+1)){
			
		if (removal_routine=="a" & removal_rule==2){		
			
			for (j in 1:rand){
				a[j]<-cv_coef(occasional_abundance(x_boot[,,j],i-1),occasional_abundance(y_boot[,,j],i-1))$shared
				b[j]<-cv_coef(occasional_abundance(x_boot[,,j],i-1),occasional_abundance(y_boot[,,j],i-1))$unique_x
				c[j]<-cv_coef(occasional_abundance(x_boot[,,j],i-1),occasional_abundance(y_boot[,,j],i-1))$unique_y
			}
		}

		if (removal_routine=="a" & removal_rule==1){		
			
			for (j in 1:rand){
				a[j]<-cv_coef(x_boot[,,j],occasional_abundance(y_boot[,,j],i-1))$shared
				b[j]<-cv_coef(x_boot[,,j],occasional_abundance(y_boot[,,j],i-1))$unique_x
				c[j]<-cv_coef(x_boot[,,j],occasional_abundance(y_boot[,,j],i-1))$unique_y
			}
		}	

		if (removal_routine=="o" & removal_rule==2 & dim(x_boot)[1]>=(i-1)){		
			
			for (j in 1:rand){
				a[j]<-cv_coef(occasional_occupancy(x_boot[,,j],i-1,0),occasional_occupancy(y_boot[,,j],i-1,0))$shared
				b[j]<-cv_coef(occasional_occupancy(x_boot[,,j],i-1,0),occasional_occupancy(y_boot[,,j],i-1,0))$unique_x
				c[j]<-cv_coef(occasional_occupancy(x_boot[,,j],i-1,0),occasional_occupancy(y_boot[,,j],i-1,0))$unique_y
			}
			
		}			

		if (removal_routine=="o" & removal_rule==1){		
			
			for (j in 1:rand){
				a[j]<-cv_coef(x_boot[,,j],occasional_occupancy(y_boot[,,j],i-1,0))$shared
				b[j]<-cv_coef(x_boot[,,j],occasional_occupancy(y_boot[,,j],i-1,0))$unique_x
				c[j]<-cv_coef(x_boot[,,j],occasional_occupancy(y_boot[,,j],i-1,0))$unique_y
			}
		}			
		
		if (removal_routine=="b" & removal_rule==2 & dim(x_boot)[1]>=(i-1)){		
			
			for (j in 1:rand){
				a[j]<-cv_coef(occasional_occupancy(x_boot[,,j],i-1,i-1),occasional_occupancy(y_boot[,,j],i-1,i-1))$shared
				b[j]<-cv_coef(occasional_occupancy(x_boot[,,j],i-1,i-1),occasional_occupancy(y_boot[,,j],i-1,i-1))$unique_x
				c[j]<-cv_coef(occasional_occupancy(x_boot[,,j],i-1,i-1),occasional_occupancy(y_boot[,,j],i-1,i-1))$unique_y
			}
			
		}			
		
		if (removal_routine=="b" & removal_rule==1){		
			
			for (j in 1:rand){
				a[j]<-cv_coef(x_boot[,,j],occasional_occupancy(y_boot[,,j],i-1,i-1))$shared
				b[j]<-cv_coef(x_boot[,,j],occasional_occupancy(y_boot[,,j],i-1,i-1))$unique_x
				c[j]<-cv_coef(x_boot[,,j],occasional_occupancy(y_boot[,,j],i-1,i-1))$unique_y
			}
		}




			for(k in 1:rand){
				m1[k]<-conservation_value(a[k],b[k],c[k])$m1
				m2[k]<-conservation_value(a[k],b[k],c[k])$m2
				m3[k]<-conservation_value(a[k],b[k],c[k])$m3
			}
				
		

		results[i,1,1]<-mean(m1)
		results[i,2,1]<-quantile(m1,probs=0.025,na.rm="TRUE")
		results[i,3,1]<-quantile(m1,probs=0.975,na.rm="TRUE")

		results[i,1,2]<-mean(m2)
		results[i,2,2]<-quantile(m2,probs=0.025,na.rm="TRUE")
		results[i,3,2]<-quantile(m2,probs=0.975,na.rm="TRUE")

		results[i,1,3]<-mean(m3)
		results[i,2,3]<-quantile(m3,probs=0.025,na.rm="TRUE")
		results[i,3,3]<-quantile(m3,probs=0.975,na.rm="TRUE")

		
	
 }
	colnames(results)<-c("m","2.5%","97.5%")
	rownames(results)<-0:seq
	results_list<-list(m1=results[,,1],m2=results[,,2],m3=results[,,3])

	return(results_list)
	
}
		
       