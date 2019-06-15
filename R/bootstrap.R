bootstrap<-function(data,n){
		
	data.boot<-array(dim=c(nrow(data),ncol(data),n))

	for (i in 1:n) data.boot[,,i]<-apply(data,2,function(x) x[sample(length(x),replace=TRUE)])


return(data.boot)

}