### imperfect particle counter

lam = 10
p = 0.9

# probability distribution

len = 5
z = matrix(0,1+len,1+len)

for(i in 0:len){
       for(j in 0:i){
               z[1+j,1+i] = dbinom(j,size=i,prob=p)
       }
}

z

z.sum = apply(z,2,sum)
z.sum

x.mean=0
for(i in 1:len){
       x.mean = c(x.mean,i*p)
}
x.mean

for(i in 0:len){
       X11()
       plot(z[,1+i],type="h")
}


# simulation, how to estimate p using linear regression through the
# origin

B = 1000000

n = rpois(B,lam)

x = rbinom(n=B,size=n,prob=p)

plot(n,x,main="Counts detected vs Counts emitted, with E[X|N]")

x.fit = lm(x ~ 0+n)
summary(x.fit)
abline(x.fit)

# Note that the estimated regression slope is very close to the true p
