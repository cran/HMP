Z.statistics <-
function(data){
	if(missing(data)){
		stop("data missing")
	}

	K=dim(data)[2]
	Num1= sum(data)
	Num2= sum(apply(data,2,function(x) { p=sum((x-1)*x);p})/apply(data,2,sum)) 
	Num3= sum(apply(data,1,function(x){nx=sum(x);b=nx*(nx-1);b}))
	Denominator=(sqrt(2*(K-1)*sum(apply(data,1,function(x){nx=sum(x);b=nx*(nx-1);b}))))
	Z= (Num1*Num2-Num3)/Denominator
}

