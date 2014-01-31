### R program that simulates Groups Testing.  Example C, pp 128-129.

n = 100000	    # samples
m = 10000		# groups
k = n/m		    # samples in each group Note: n = m*k

Pi = 0.05		# prevalence P(D)
Eta = 1		    # sensativity P(+|D)
Theta = 1		# specificity P(-|D^c)

p = 1 - ( Eta*Pi + (1-Theta)*(1-Pi) )	# probability a sample tests negative

N = 0			# count of number of tests performed

for (i in 1:m){
	# test m groups
	if(rbinom(1,1,p^k) == 1){ 
		# group is negative, with probability p^k, and only one test needs to be performed
		N = N + 1
	}
	else{
		# group is postive, so one test is done and k more need to be performed
		N = N + k + 1
	}
}

# the simlates value for N
N

# the theoretical expected value for N
ev.N = n*(1 + (1/k) - (p^k))
ev.N
