#' Sequentially removes occasional species according to increasing abundance and/or occupancy classes.
#'
#' This function sequenctially removes occasional species according to increasing abundance and/or occupancy
#' classes and calculates conservation value metrics in order to produce a curve.
#  The conservation value metrics are those produced by the 'conservation_value' function:
#' m1-> the proportion of all species that is shared between two communities
#' m2-> the proportion of all species that is unique to one of the two communities
#' m3-> the proportion of the species that occur in one of the communities that is unique to that community
#' Each metric will be calulated once for each of the removal classes (1 to 'seq').
#'
#' @param x A community matrix, with species in columns and samples in rows
#' @param y A community matrix, with species in columns and samples in rows
#' @param seq The maximum removal class to be removed. The sequence will wil go from zero to this class (inclusive)
#' @param removal_rule The removal_rule to be used.If 1, applies the removal routine to x and y. If 2, applies on to y.
#' @param removal_routine The removal_routine to be used. 
#' If "a", removes by abundance class using the 'occasional_abundance' function.
#' If "o", removes by occupancy class using the 'occasional_occupancy' function.
#' If "b", removes by abundance and then occupancy, using the same occasional class for both.
#' @return A list with $m1, $m2 and $m3 vectors, each with seq elements
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
#'  # Sequentially remove abundance classes from both communities and and calculate the conservation metrics.
#'  conservation_value_seq(comm.A, comm.B, 5, removal_rule=2, removal_routine="a")
#' $m1
#' [1] 0.9666667 0.9333333 0.9333333 0.9000000 0.9000000 0.8333333
#' 
#' $m2
#' [1] 0.00000000 0.03333333 0.03333333 0.03333333 0.03333333 0.10000000
#' 
#' $m3
#' [1] 0.00000000 0.03448276 0.03448276 0.03571429 0.03571429 0.10714286


conservation_value_seq<-function(x,y,seq,removal_rule=2,removal_routine="a"){

results<-matrix(nrow=seq+1,ncol=3)

	for(i in 1:(seq+1)){

		if (removal_routine=="a" & removal_rule==2){		
			
				a<-cv_coef(occasional_abundance(x,i-1),occasional_abundance(y,i-1))$shared
				b<-cv_coef(occasional_abundance(x,i-1),occasional_abundance(y,i-1))$unique_x
				c<-cv_coef(occasional_abundance(x,i-1),occasional_abundance(y,i-1))$unique_y
			
		}

		if (removal_routine=="a" & removal_rule==1){		
			
				a<-cv_coef(x,occasional_abundance(y,i-1))$shared
				b<-cv_coef(x,occasional_abundance(y,i-1))$unique_x
				c<-cv_coef(x,occasional_abundance(y,i-1))$unique_y
			
		}	

		if (removal_routine=="o" & removal_rule==2 & dim(x)[1]>=(i-1)){		
			

				a<-cv_coef(occasional_occupancy(x,i-1,0),occasional_occupancy(y,i-1,0))$shared
				b<-cv_coef(occasional_occupancy(x,i-1,0),occasional_occupancy(y,i-1,0))$unique_x
				c<-cv_coef(occasional_occupancy(x,i-1,0),occasional_occupancy(y,i-1,0))$unique_y

		}			

		if (removal_routine=="o" & removal_rule==1){		
			

				a<-cv_coef(x,occasional_occupancy(y,i-1,0))$shared
				b<-cv_coef(x,occasional_occupancy(y,i-1,0))$unique_x
				c<-cv_coef(x,occasional_occupancy(y,i-1,0))$unique_y
			
		}			
		
		if (removal_routine=="b" & removal_rule==2 & dim(x)[1]>=(i-1)){		
			

				a<-cv_coef(occasional_occupancy(x,i-1,i-1),occasional_occupancy(y,i-1,i-1))$shared
				b<-cv_coef(occasional_occupancy(x,i-1,i-1),occasional_occupancy(y,i-1,i-1))$unique_x
				c<-cv_coef(occasional_occupancy(x,i-1,i-1),occasional_occupancy(y,i-1,i-1))$unique_y

		}			
		
		if (removal_routine=="b" & removal_rule==1){		
			

				a<-cv_coef(x,occasional_occupancy(y,i-1,i-1))$shared
				b<-cv_coef(x,occasional_occupancy(y,i-1,i-1))$unique_x
				c<-cv_coef(x,occasional_occupancy(y,i-1,i-1))$unique_y


		}
			results[i,1]<-conservation_value(a,b,c)$m1
			results[i,2]<-conservation_value(a,b,c)$m2
			results[i,3]<-conservation_value(a,b,c)$m3

		}
	
	
	
	results<-list(m1=results[,1],m2=results[,2],m3=results[,3])

	return(results)

}	