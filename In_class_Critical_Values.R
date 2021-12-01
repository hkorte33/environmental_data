#critical z-values for a 90% CI of the standard normal distribution
qnorm(c(0.05,0.95))
#95% T-distribution critical values
#2
qt(c(.025,.975), 10)
#3
qt(c(.025,.975), df = 75)
qnorm(c(.025,.975))
#4
qt(c(.025,.975), df = 475)
#95% CL using t-dist
#5
qt(c(.025,.975),49)
#6
(3.14/(sqrt(50)))*qt(c(.025,.975),49)
