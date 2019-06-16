plot_graph_CI<-function(x){


	plot(x[1:nrow(x),1], type="b",ylim=c(0,1))
	arrows(1:nrow(x),x[1:nrow(x),2],1:nrow(x),x[1:nrow(x),3],code=3,angle=90,length=0.1)
	
	
}