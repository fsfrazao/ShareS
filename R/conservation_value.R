#' Calculates conservation value metrics
#'
#' This function calculates 3 conservation value metrics:
#' m1-> the proportion of all species that is shared between two communities;
#' m2-> the proportion of all species that is unique to one of the two communities;
#' m1-> the proportion of the species that occur in one of the communities that is unique to that community;
#' @param a The number of shared species
#' @param b The number of species unique to one of the communities
#' @param c The number of species unique to the other community
#' @return A list with $m1, $m2 and $m3
#' @export
#' @examples
#' conservation_value(10,15,3)
#'  conservation_value(10,15,3)
#' $m1
#' [1] 0.3571429
#' 
#' $m2
#' [1] 0.5357143
#' 
#' $m3
#' [1] 0.6

conservation_value<-function(a,b,c){
	
	if ((a+b+c)==0) m1<-0 else m1<-a/(a+b+c)
	if ((a+b+c)==0) m2<-0 else m2<-b/(a+b+c)
	if ((a+b)==0) m3<-0 else m3<-b/(a+b)

		conservation_values<-list(m1=m1,m2=m2,m3=m3)

	conservation_values
}