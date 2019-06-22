#' Bootstraps a community matrix
#'
#' This function bootstraps a community matrix. The output can be used by 'conservation_value_seq_CI',
#' which computes confidence intervals for conservation value curves.
#' @param data The community matrix, with species in columns and samples in rows.
#' @param n The number of bootstraped communities.
#' @return A r x c x n array, whith the same number of rows (r) and columns (c) as data. 
#' @export
#' @examples
#'data(community_data)
#'
#' #Select all samples in Evironment 'A'
#' comm.A<-community_data[which(community_data$environment == "A"),]
#'
#' # Select only the abundance matrix (columns 1 and 2 contain the 'environment' and 'sample' data)
#' comm.A <- comm.A[, 3:ncol(comm.A)]
#' 
#' bootstrap(comm.A, 3)
#', , 1
#'
#'     [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14]
#' [1,]    1    2    5    6    1    7    1    5    6     0     0    10     9     8
#' [2,]    5    2    6    2    0    6    6   10    3     0     2     2     1     6
#' [3,]    1    2    3    6    1   10    3    2    3     0     2     8     1     6
#' [4,]    6    3    6    2    1    6    3    5    1     0     2     2    10     6
#' [5,]    5    2    4    6    1    7    6   10    6     0     7     3     9    10
#'      [,15] [,16] [,17] [,18] [,19] [,20] [,21] [,22] [,23] [,24] [,25] [,26]
#' [1,]     8     8     0     3     5     5     1    10     0     0     6     7
#' [2,]     0     2     5     7     1     5     8     0     0     9     2     7
#' [3,]     2     8     0     4    10     2     8     0    10     8     9     7
#' [4,]     0     2     8     9     5     2     1     4     0     9     9     7
#' [5,]     2     2     9     7     4    10     8     2    10     8     9     9
#'      [,27] [,28] [,29] [,30]
#' [1,]    10     2     8     4
#' [2,]     7     8     3     7
#' [3,]     2     2     3     2
#' [4,]     7    10     6     4
#' [5,]     7     2     3     4

#' , , 2

#'      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14]
#' [1,]    6    8    5    5    1   10    6    5    3     0     7     8     6     6
#' [2,]    5    2    6    6    0    7    6   10    8     0     2     2    10     6
#' [3,]    6    2    6    2    1    7    3   10    7     0     7     3     9     2
#' [4,]    5    3    6    2    1    6    6    7    6     0     0    10     1    10
#' [5,]    5    8    5    2    1    7    3   10    1     0     7     3     9     2
#'      [,15] [,16] [,17] [,18] [,19] [,20] [,21] [,22] [,23] [,24] [,25] [,26]
#' [1,]     8     1     4     3     4     2     8     4     0     8     9     9
#' [2,]     0     2     5     9     5     5     0     4    10     9     2     9
#' [3,]     0     8     8     9     1     2     0     4     0     3     5     9
#' [4,]     3     8     4     3     4     3     1     2    10     0     9     7
#' [5,]     0     1     5     9     5     3     8     2     0     9     5     4
#'      [,27] [,28] [,29] [,30]
#' [1,]     6    10     2     4
#' [2,]     6     2     3     4
#' [3,]    10    10     3     2
#' [4,]     2     8     8     4
#' [5,]     2    10     6     8
#' 
#' , , 3
#' 
#'      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14]
#' [1,]    1    3    6    2    1    6    3    7    1     0     7     2    10    10
#' [2,]    5    2    4    2    1    6    3    2    8     0     2    10    10     2
#' [3,]    5    2    5    2    1    6    6    5    7     0     2     8     1     8
#' [4,]    6    3    6    2    0    6    6   10    8     0     0     5     6    10
#' [5,]    5    2    5    2    0    6    1    7    6     0     7     3     1     6
#'      [,15] [,16] [,17] [,18] [,19] [,20] [,21] [,22] [,23] [,24] [,25] [,26]
#' [1,]     0     8     9     3     1     3     0    10     5     3     2     7
#' [2,]     0     1     5     3     4     5     8     4     0     9     2     9
#' [3,]     3     8     0     3    10     2     1    10     0     3     9     7
#' [4,]     3     8     0     3     1     3     8    10     0     0     9     9
#' [5,]     8     8     0     9     4     2     0    10     0     3     5     9
#'      [,27] [,28] [,29] [,30]
#' [1,]     2    10     3     4
#' [2,]     2    10     8     7
#' [3,]     6     2     2     4
#' [4,]    10     8     6     4
#' [5,]     2     8     3     4

bootstrap<-function(data,n){
		
	data.boot<-array(dim=c(nrow(data),ncol(data),n))

	for (i in 1:n) data.boot[,,i]<-apply(data,2,function(x) x[sample(length(x),replace=TRUE)])


return(data.boot)

}