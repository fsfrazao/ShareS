#' Converts Fahrenheit to Kelvin
#'
#' This function converts input temperatures in Fahrenheit to Kelvin.
#' @param temp_F The temperature in Fahrenheit.
#' @return The temperature in Kelvin.
#' @export
#' @examples
#' fahrenheit_to_kelvin(32)

occasional_occupancy<-function(data,occup_class,abund_class=0,dont_apply_to=0){
	
	abund<-occasional_abundance(data,abund_class)
	res_mat<-apply(abund,2,function(x) if (sum(x!=0)>occup_class) result<-x else result<-rep(0,length(x)))
	if(dont_apply_to!=0){

	 data<-as.matrix(data)		
	 res_mat[,dont_apply_to]<-data[,dont_apply_to]
	
	}

