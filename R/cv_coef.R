#' Converts Fahrenheit to Kelvin
#'
#' This function converts input temperatures in Fahrenheit to Kelvin.
#' @param temp_F The temperature in Fahrenheit.
#' @return The temperature in Kelvin.
#' @export
#' @examples
#' fahrenheit_to_kelvin(32)

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