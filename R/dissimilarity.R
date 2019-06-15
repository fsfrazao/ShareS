#' Calculates dissimilarity metrics
#'
#' This function calculates dissimilarity metrics between two communities (Sorensen and Jaccard indices).
#' @param a The number of shared species
#' @param b The number of species unique to one of the communities
#' @param c The number of species unique to the other community
#' @return A list with $sor (the Sorensen index) and $jac (the Jaccard index)
#' @export
#' @examples
#' dissimilarity(10,15,3)
#' $sor
#' [1] 0.4736842
#' 
#' $jac
#' [1] 0.6428571


dissimilarity<-function(a,b,c) list(sor=(b+c)/(2*a+b+c),jac=(b+c)/(a+b+c))