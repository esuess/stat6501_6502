### Example of a joint density.

x = seq(0,5,0.2)
y = x
f = function(x,y){ 2*exp(-x)*exp(-2*y) }
z = outer(x,y,f)

X11()
par(mfrow=c(2,2))

persp(z)
persp(z,eye=c(5,-1,3))
persp(z,eye=c(-1,5,3))
persp(z,eye=c(5,5,3))

### Example of plotting the joint normal distribution. 

x = seq(-2,2,0.1)
y = seq(-2,2,0.1)

X11()
par(mfrow=c(2,2))

bvn = function(x,y){
	mu1 = 0
	sigma1 = 1
	mu2 = 0
	sigma2 = 1
	rho = 0
	((2*pi*sigma1*sigma2*sqrt(1-rho^2))^(-1))*
			exp( -(2*((1-rho^2))^(-1)) * ( (x-mu1)^2/sigma1^2 + (y-mu2)^2/sigma2^2
			- ( (2*rho*(x-mu1)*(y-mu2) )/ (sigma1*sigma2) ) ) )
}

z1 = outer(x,y,bvn)
persp(x,y,z1)

bvn = function(x,y){
	mu1 = 0
	sigma1 = 1
	mu2 = 0
	sigma2 = 1
	rho = 0.3
	((2*pi*sigma1*sigma2*sqrt(1-rho^2))^(-1))*
			exp( -(2*((1-rho^2))^(-1)) * ( (x-mu1)^2/sigma1^2 + (y-mu2)^2/sigma2^2
			- ( (2*rho*(x-mu1)*(y-mu2) )/ (sigma1*sigma2) ) ) )
}

z2 = outer(x,y,bvn)
persp(x,y,z2)

bvn = function(x,y){
	mu1 = 0
	sigma1 = 1
	mu2 = 0
	sigma2 = 1
	rho = 0.6
	((2*pi*sigma1*sigma2*sqrt(1-rho^2))^(-1))*
			exp( -(2*((1-rho^2))^(-1)) * ( (x-mu1)^2/sigma1^2 + (y-mu2)^2/sigma2^2
			- ( (2*rho*(x-mu1)*(y-mu2) )/ (sigma1*sigma2) ) ) )
}

z3 = outer(x,y,bvn)
persp(x,y,z3)

bvn = function(x,y){
	mu1 = 0
	sigma1 = 1
	mu2 = 0
	sigma2 = 1
	rho = -0.9
	((2*pi*sigma1*sigma2*sqrt(1-rho^2))^(-1))*
			exp( -(2*(1-rho^2)^(-1)) * ( (x-mu1)^2/sigma1^2 + (y-mu2)^2/sigma2^2
			- ( (2*rho*(x-mu1)*(y-mu2) )/ (sigma1*sigma2) ) ) )
}

z4 = outer(x,y,bvn)
persp(x,y,z4)

X11()
par(mfrow=c(2,2))

contour(x,y,z1)
contour(x,y,z2)
contour(x,y,z3)
contour(x,y,z4)




