## To source this file into R session, type:
# source('https://raw.githubusercontent.com/skyshixia/Rfunction/master/RBetaBuster.r')

## input
# percentile: percentile, e.g. 0.05, 0.99 etc
# percentileValue: the prior value at qth percentile
# momeValue: prior mode/mean
# mu=TRUE if the input value is mean, default is FALSE (input is mode)

RBetaBuster <- function(percentile, percentileValue, momeValue, mu=FALSE){
	qth = percentile
	qthValue = percentileValue
	stats = momeValue
	
	if(mu == TRUE){
			b2a <- function(stats, b){stats*b/(1-stats)}
			print("Given mean,")
		}else{
			b2a <- function(stats, b){(1 + stats*(b-2))/(1-stats)}
			print("Given mode,")
	}

	b = 1:1000
	a = b2a(stats, b)
	li = qbeta(qth, a, b)

	intb = which((diff(li >= qthValue, lag=1)) != 0)

	decb = intb + b/1000
	a = b2a(stats, decb)
	li2 = qbeta(qth, a, decb)

	realb = intb + which((diff(li2 >= qthValue, lag=1)) != 0)/1000
	reala = b2a(stats, realb)

	answer = list("a is", reala, "b is", realb)
	return(answer)
}

