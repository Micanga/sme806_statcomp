mc.gamma <- function(n,a,b,alpha,beta){
	x = runif(n,a,b)

	I = (b-a)*(1/n)*((beta**alpha)/gamma(alpha))*(sum( (x**(alpha-1))*(exp(1)**(-beta*x)) ))
	print(paste('My-MC :',I))
	print(paste('pgamma:',pgamma(q=b,shape=alpha,rate=beta) - pgamma(q=a,shape=alpha,rate=beta)))
}