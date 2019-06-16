#' Converts Fahrenheit to Kelvin
#'
#' This function converts input temperatures in Fahrenheit to Kelvin.
#' @param temp_F The temperature in Fahrenheit.
#' @return The temperature in Kelvin.
#' @export
#' @examples
#' fahrenheit_to_kelvin(32)

occasional_abundance<-function(data,abund_class,dont_apply_to=0){
	
	res_mat<- apply(data,2,function(x) if (sum(x)>abund_class) result<-x else result<-rep(0,length(x)))
	if(dont_apply_to!=0){

		data<-as.matrix(data)		 
		res_mat[,dont_apply_to]<-data[,dont_apply_to]
	}
	
	return(res_mat)
}