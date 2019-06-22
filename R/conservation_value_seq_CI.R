\


conservation_value_seq_CI<-function(x_boot,y_boot,seq,removal_rule=2,occasional="a"){

if (dim(x_boot)[3]!=dim(y_boot)[3]) stop("x_boot and y_boot must have the same number of randomizations") else rand=dim(x_boot)[3]

results<-array(dim=c(seq+1,3,3))

a<-vector(length=rand)
b<-vector(length=rand)
c<-vector(length=rand)

m1<-vector(length=rand)
m2<-vector(length=rand)
m3<-vector(length=rand)


	for (i in 1:(seq+1)){
			
		if (occasional=="a" & removal_rule==2){		
			
			for (j in 1:rand){
				a[j]<-cv_coef(occasional_abundance(x_boot[,,j],i-1),occasional_abundance(y_boot[,,j],i-1))$shared
				b[j]<-cv_coef(occasional_abundance(x_boot[,,j],i-1),occasional_abundance(y_boot[,,j],i-1))$unique_x
				c[j]<-cv_coef(occasional_abundance(x_boot[,,j],i-1),occasional_abundance(y_boot[,,j],i-1))$unique_y
			}
		}

		if (occasional=="a" & removal_rule==1){		
			
			for (j in 1:rand){
				a[j]<-cv_coef(x_boot[,,j],occasional_abundance(y_boot[,,j],i-1))$shared
				b[j]<-cv_coef(x_boot[,,j],occasional_abundance(y_boot[,,j],i-1))$unique_x
				c[j]<-cv_coef(x_boot[,,j],occasional_abundance(y_boot[,,j],i-1))$unique_y
			}
		}	

		if (occasional=="o" & removal_rule==2 & dim(x_boot)[1]>=(i-1)){		
			
			for (j in 1:rand){
				a[j]<-cv_coef(occasional_occupancy(x_boot[,,j],i-1,0),occasional_occupancy(y_boot[,,j],i-1,0))$shared
				b[j]<-cv_coef(occasional_occupancy(x_boot[,,j],i-1,0),occasional_occupancy(y_boot[,,j],i-1,0))$unique_x
				c[j]<-cv_coef(occasional_occupancy(x_boot[,,j],i-1,0),occasional_occupancy(y_boot[,,j],i-1,0))$unique_y
			}
			
		}			

		if (occasional=="o" & removal_rule==1){		
			
			for (j in 1:rand){
				a[j]<-cv_coef(x_boot[,,j],occasional_occupancy(y_boot[,,j],i-1,0))$shared
				b[j]<-cv_coef(x_boot[,,j],occasional_occupancy(y_boot[,,j],i-1,0))$unique_x
				c[j]<-cv_coef(x_boot[,,j],occasional_occupancy(y_boot[,,j],i-1,0))$unique_y
			}
		}			
		
		if (occasional=="b" & removal_rule==2 & dim(x_boot)[1]>=(i-1)){		
			
			for (j in 1:rand){
				a[j]<-cv_coef(occasional_occupancy(x_boot[,,j],i-1,i-1),occasional_occupancy(y_boot[,,j],i-1,i-1))$shared
				b[j]<-cv_coef(occasional_occupancy(x_boot[,,j],i-1,i-1),occasional_occupancy(y_boot[,,j],i-1,i-1))$unique_x
				c[j]<-cv_coef(occasional_occupancy(x_boot[,,j],i-1,i-1),occasional_occupancy(y_boot[,,j],i-1,i-1))$unique_y
			}
			
		}			
		
		if (occasional=="b" & removal_rule==1){		
			
			for (j in 1:rand){
				a[j]<-cv_coef(x_boot[,,j],occasional_occupancy(y_boot[,,j],i-1,i-1))$shared
				b[j]<-cv_coef(x_boot[,,j],occasional_occupancy(y_boot[,,j],i-1,i-1))$unique_x
				c[j]<-cv_coef(x_boot[,,j],occasional_occupancy(y_boot[,,j],i-1,i-1))$unique_y
			}
		}




			for(k in 1:rand){
				m1[k]<-conservation_value(a[k],b[k],c[k])$m1
				m2[k]<-conservation_value(a[k],b[k],c[k])$m2
				m3[k]<-conservation_value(a[k],b[k],c[k])$m3
			}
				
		

		results[i,1,1]<-mean(m1)
		results[i,2,1]<-quantile(m1,probs=0.025,na.rm="TRUE")
		results[i,3,1]<-quantile(m1,probs=0.975,na.rm="TRUE")

		results[i,1,2]<-mean(m2)
		results[i,2,2]<-quantile(m2,probs=0.025,na.rm="TRUE")
		results[i,3,2]<-quantile(m2,probs=0.975,na.rm="TRUE")

		results[i,1,3]<-mean(m3)
		results[i,2,3]<-quantile(m3,probs=0.025,na.rm="TRUE")
		results[i,3,3]<-quantile(m3,probs=0.975,na.rm="TRUE")

		
	
 }
	colnames(results)<-c("m","2.5%","97.5%")
	rownames(results)<-0:seq
	results_list<-list(m1=results[,,1],m2=results[,,2],m3=results[,,3])

	return(results_list)
	
}
		
       