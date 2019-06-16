conservation.value.seq.CI<-function(x.boot,y.boot,seq,removal.rule=2,occasional="a"){

if (dim(x.boot)[3]!=dim(y.boot)[3]) stop("x.boot and y.boot must have the same number of randomizations") else rand=dim(x.boot)[3]

results<-array(dim=c(seq+1,3,3))

a<-vector(length=rand)
b<-vector(length=rand)
c<-vector(length=rand)

m1<-vector(length=rand)
m2<-vector(length=rand)
m3<-vector(length=rand)


	for (i in 1:(seq+1)){
			
		if (occasional=="a" & removal.rule==2){		
			
			for (j in 1:rand){
				a[j]<-cv.coef(occasional.abundance(x.boot[,,j],i-1),occasional.abundance(y.boot[,,j],i-1))$shared
				b[j]<-cv.coef(occasional.abundance(x.boot[,,j],i-1),occasional.abundance(y.boot[,,j],i-1))$unique.x
				c[j]<-cv.coef(occasional.abundance(x.boot[,,j],i-1),occasional.abundance(y.boot[,,j],i-1))$unique.y
			}
		}

		if (occasional=="a" & removal.rule==1){		
			
			for (j in 1:rand){
				a[j]<-cv.coef(x.boot[,,j],occasional.abundance(y.boot[,,j],i-1))$shared
				b[j]<-cv.coef(x.boot[,,j],occasional.abundance(y.boot[,,j],i-1))$unique.x
				c[j]<-cv.coef(x.boot[,,j],occasional.abundance(y.boot[,,j],i-1))$unique.y
			}
		}	

		if (occasional=="o" & removal.rule==2 & dim(x.boot)[1]>=(i-1)){		
			
			for (j in 1:rand){
				a[j]<-cv.coef(occasional.occupancy(x.boot[,,j],i-1,0),occasional.occupancy(y.boot[,,j],i-1,0))$shared
				b[j]<-cv.coef(occasional.occupancy(x.boot[,,j],i-1,0),occasional.occupancy(y.boot[,,j],i-1,0))$unique.x
				c[j]<-cv.coef(occasional.occupancy(x.boot[,,j],i-1,0),occasional.occupancy(y.boot[,,j],i-1,0))$unique.y
			}
			
		}			

		if (occasional=="o" & removal.rule==1){		
			
			for (j in 1:rand){
				a[j]<-cv.coef(x.boot[,,j],occasional.occupancy(y.boot[,,j],i-1,0))$shared
				b[j]<-cv.coef(x.boot[,,j],occasional.occupancy(y.boot[,,j],i-1,0))$unique.x
				c[j]<-cv.coef(x.boot[,,j],occasional.occupancy(y.boot[,,j],i-1,0))$unique.y
			}
		}			
		
		if (occasional=="b" & removal.rule==2 & dim(x.boot)[1]>=(i-1)){		
			
			for (j in 1:rand){
				a[j]<-cv.coef(occasional.occupancy(x.boot[,,j],i-1,i-1),occasional.occupancy(y.boot[,,j],i-1,i-1))$shared
				b[j]<-cv.coef(occasional.occupancy(x.boot[,,j],i-1,i-1),occasional.occupancy(y.boot[,,j],i-1,i-1))$unique.x
				c[j]<-cv.coef(occasional.occupancy(x.boot[,,j],i-1,i-1),occasional.occupancy(y.boot[,,j],i-1,i-1))$unique.y
			}
			
		}			
		
		if (occasional=="b" & removal.rule==1){		
			
			for (j in 1:rand){
				a[j]<-cv.coef(x.boot[,,j],occasional.occupancy(y.boot[,,j],i-1,i-1))$shared
				b[j]<-cv.coef(x.boot[,,j],occasional.occupancy(y.boot[,,j],i-1,i-1))$unique.x
				c[j]<-cv.coef(x.boot[,,j],occasional.occupancy(y.boot[,,j],i-1,i-1))$unique.y
			}
		}




			for(k in 1:rand){
				m1[k]<-conservation.value(a[k],b[k],c[k])$m1
				m2[k]<-conservation.value(a[k],b[k],c[k])$m2
				m3[k]<-conservation.value(a[k],b[k],c[k])$m3
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
	results.list<-list(m1=results[,,1],m2=results[,,2],m3=results[,,3])

	return(results.list)
	
}
		