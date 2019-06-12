occasional_abundance<-function(data,abund_class,dont_apply_to=0){
	
	res_mat<- apply(data,2,function(x) if (sum(x)>abund_class) result<-x else result<-rep(0,length(x)))
	if(dont_apply_to!=0){

		data<-as.matrix(data)		 
		res_mat[,dont_apply_to]<-data[,dont_apply_to]
	}
	
	return(res_mat)
}