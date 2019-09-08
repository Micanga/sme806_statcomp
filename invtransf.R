inv.fx <- function(n,A){
	u = runif(n,0,1)
	x = A*sqrt(u)
	return(x)
}

plot.inv.fx <- function(n,A){
	x <- round(inv.fx(n,A),3)
	print(summary(x))

	hist(x,main="",prob=T)
	rug(x)
	curve((2/A**2)*x,from=0,to=A,add=T)
}

inv.logi <- function(n){
	u = runif(n,0,1)
	x = -log((1/u) - 1)
	return(x)
}

plot.inv.logi <- function(n){
	x <- round(inv.logi(n),3)
	print(summary(x))

	hist(x,main="",prob=T)
	rug(x)
	curve((exp(1)**(-x)/(1+exp(1)**(-x))),from=-10,to=10,add=T)
}

inv.exp <- function(n,lamda){
	u = runif(n,0,1)
	x = -log(1 -u)/lamda
	return(x)
}

plot.weibull <- function(n,lamda,alpha){
	x <- round(inv.exp(n,lamda),3)
	print(summary(x))

	hist(x,main="",prob=T)
	rug(x)

	curve((lamda*exp(1)**(-lamda*x))**(1/alpha),from=0,to=10,add=T)
}