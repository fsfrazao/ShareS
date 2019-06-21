#' Excludes occasional species defined by occupancy
#'
#' This function considers any species with total occupancy lower than the specified occupancy class to be an occasional species.
#' It replaces the abundance of occasional species with zeros.
#' @param data A data.frame containing species in the columns and samples in the rows. 
#' @param occup_class The occupancy threshold. Species for which the total occupancy is lower than this value are considered occasional.
#' The function considers the total occupancy (i.e.: the number of samples/rows in which the species is present).
#' @param abund_class The abundance threshold.If greater than zero, applies the occasional_abundance function before excluding by occupancy.
#' Species for which the total abundance is lower than this value are considered occasional.
#' The total abundance (i.e.: the sum accross all samples/rows) not the abundance per sample.
#' @param dont_apply_to A list of species (as column names or indices) to disregard.
#' These species abundances will not be modified even if their total abundances are below the abund_class threshold.
#' @return The community data with occasional species replaced by zeros
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
#' Consider all species with total occupancy lower than 5 to be occasional.
#' These will be replaced by zeros, such as sp.5, sp.15, sp.21 and sp.23.
#' 
#'
#' occasional_occupancy(comm.A,3)
#'     sp1 sp2 sp3 sp4 sp5 sp6 sp7 sp8 sp9 sp10 sp11 sp12 sp13 sp14 sp15 sp16
#' [1,]   5   2   3   8   0   6   3  10   1    0    2    5    1    8    0    2
#' [2,]   1   8   6   2   0   7   6   2   3    0    1   10    9    6    0    8
#' [3,]   5   2   6   5   0  10   6  10   7    0    7    8   10    2    0    2
#' [4,]   6   2   5   6   0   7   1   5   8    0    0    3    1    6    0    1
#' [5,]   6   3   4   2   0   6   3   7   6    0    2    2    6   10    0    8
#'      sp17 sp18 sp19 sp20 sp21 sp22 sp23 sp24 sp25 sp26 sp27 sp28 sp29 sp30
#' [1,]    8    9    5    5    0    4    0    9    6    9    7    9    8    4
#' [2,]    0    7   10    3    0    2    0    3    9    7    2    8    3    7
#' [3,]    9    4    1   10    0   10    0    9    5    4    2    3    2    8
#' [4,]    4    3   10    2    0   10    0    0    9    7    6    2    6    4
#' [5,]    5    3    4    2    0    0    0    8    2    9   10   10    3    2


occasional_occupancy<-function(data,occup_class,abund_class=0,dont_apply_to=0){
	
	abund<-occasional_abundance(data,abund_class)
	res_mat<-apply(abund,2,function(x) if (sum(x!=0)>occup_class) result<-x else result<-rep(0,length(x)))
	if(dont_apply_to!=0){

	 data<-as.matrix(data)		
	 res_mat[,dont_apply_to]<-data[,dont_apply_to]
	
	}

	
	return(res_mat)

}