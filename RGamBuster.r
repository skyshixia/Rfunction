## To source this file into R session, type:
# source('https://raw.githubusercontent.com/skyshixia/Rfunction/master/RGamBuster.r')

## input
# percentile: percentile, e.g. 0.05, 0.99 etc
# percentileValue: the prior value at qth percentile
# momeValue: prior mode/mean
# mu=TRUE if the input value is mean, default is FALSE (input is mode)

RGamBuster <- function(percentile, percentileValue, momeValue, mu=FALSE){
	qth = percentile
	qthValue = percentileValue
	stats = momeValue
	
	if(mu == TRUE){
			b2a <- function(stats, b){stats*b}
			print("Given mean,")
		}else{
			b2a <- function(stats, b){stats*b+1}
			print("Given mode,")
	}

	b = 1:10000/100
	a = b2a(stats, b)
	li = qgamma(qth, a, b)

	intb = which((diff(li >= qthValue, lag=1)) != 0)/100

	decb = intb + b/10000
	a = b2a(stats, decb)
	li2 = qgamma(qth, a, decb)

	realb = round(intb + which((diff(li2 >= qthValue, lag=1)) != 0)/1000000, 3)
	reala = round(b2a(stats, realb), 3)

	answer = list("a is", reala, "b is", realb)
	return(answer)
}

