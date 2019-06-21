#' Extracts conservation value coefficients from a pair a community matr conservation value metricsices
#'
#' This function takes two community matrices and extracts 3 coefficients used to calculate conservation value metrics
#' @param x A community matrix, with species in columns and samples in rows
#' @param y A community matrix, with species in columns and samples in rows
#' @return a list with $shared (the number of shared species), $unique_x (the number of species unique to community x) and 
#' $unique_y (the number of species unique to community y)
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
#' #Calculate coefficients
#' cv_coef(comm.A, comm.B)

cv_coef<-function(x,y){

	if(is.vector(x)) soma_x<-x else soma_x<-apply(x,2,sum)
	if(is.vector(y)) soma_y<-y else soma_y<-apply(y,2,sum)
		
	a<-0
	b<-0
	c<-0

	for(i in 1:ncol(x)){

		if (soma_x[i]!=0 & soma_y[i]!=0) a=a+1
		if (soma_x[i]!=0 & soma_y[i]==0) b=b+1
		if (soma_x[i]==0 & soma_y[i]!=0) c=c+1
	}

	coefficients <- list(shared=a, unique_x=b, unique_y=c)

	return(coefficients)
}