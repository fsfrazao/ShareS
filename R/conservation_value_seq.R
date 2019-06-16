conservation_value_seq<-function(x,y,seq,removal_rule=2,occasional="a"){

results<-matrix(nrow=seq+1,ncol=3)

	for(i in 1:(seq+1)){

		if (occasional=="a" & removal_rule==2){		
			
				a<-cv_coef(occasional_abundance(x,i-1),occasional_abundance(y,i-1))$shared
				b<-cv_coef(occasional_abundance(x,i-1),occasional_abundance(y,i-1))$unique_x
				c<-cv_coef(occasional_abundance(x,i-1),occasional_abundance(y,i-1))$unique_y
			
		}

		if (occasional=="a" & removal_rule==1){		
			
				a<-cv_coef(x,occasional_abundance(y,i-1))$shared
				b<-cv_coef(x,occasional_abundance(y,i-1))$unique_x
				c<-cv_coef(x,occasional_abundance(y,i-1))$unique_y
			
		}	

		if (occasional=="o" & removal_rule==2 & dim(x)[1]>=(i-1)){		
			

				a<-cv_coef(occasional_occupancy(x,i-1,0),occasional_occupancy(y,i-1,0))$shared
				b<-cv_coef(occasional_occupancy(x,i-1,0),occasional_occupancy(y,i-1,0))$unique_x
				c<-cv_coef(occasional_occupancy(x,i-1,0),occasional_occupancy(y,i-1,0))$unique_y

		}			

		if (occasional=="o" & removal_rule==1){		
			

				a<-cv_coef(x,occasional_occupancy(y,i-1,0))$shared
				b<-cv_coef(x,occasional_occupancy(y,i-1,0))$unique_x
				c<-cv_coef(x,occasional_occupancy(y,i-1,0))$unique_y
			
		}			
		
		if (occasional=="b" & removal_rule==2 & dim(x)[1]>=(i-1)){		
			

				a<-cv_coef(occasional_occupancy(x,i-1,i-1),occasional_occupancy(y,i-1,i-1))$shared
				b<-cv_coef(occasional_occupancy(x,i-1,i-1),occasional_occupancy(y,i-1,i-1))$unique_x
				c<-cv_coef(occasional_occupancy(x,i-1,i-1),occasional_occupancy(y,i-1,i-1))$unique_y

		}			
		
		if (occasional=="b" & removal_rule==1){		
			

				a<-cv_coef(x,occasional_occupancy(y,i-1,i-1))$shared
				b<-cv_coef(x,occasional_occupancy(y,i-1,i-1))$unique_x
				c<-cv_coef(x,occasional_occupancy(y,i-1,i-1))$unique_y


		}
			results[i,1]<-conservation_value(a,b,c)$m1
			results[i,2]<-conservation_value(a,b,c)$m2
			results[i,3]<-conservation_value(a,b,c)$m3

		}
	
	
	
	results<-list(m1=results[,1],m2=results[,2],m3=results[,3])

	return(results)

}	