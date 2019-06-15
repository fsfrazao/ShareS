conservation_value<-function(a,b,c){

	
	
	if ((a+b+c)==0) m1<-0 else m1<-a/(a+b+c)
	if ((a+b+c)==0) m2<-0 else m2<-b/(a+b+c)
	if ((a+b)==0) m3<-0 else m3<-b/(a+b)

		conservation_values<-list(m1=m1,m2=m2,m3=m3)

	conservation_values
}