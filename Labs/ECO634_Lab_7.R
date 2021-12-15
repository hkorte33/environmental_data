#Calculate parametric 95% CI for Gentoo penguins mean bill length(mm)
#Q1-Q5
require(palmerpenguins)
#What is the sample size, n?
Gentoo=subset(penguins,species=="Gentoo")
dat_size=sum(!is.na(Gentoo$bill_length_mm))
dat_size
#What is the sample standard deviation?
pen_sd=sd(Gentoo$bill_length_mm,na.rm=TRUE)
pen_sd
#What are the critical t-values? 
crit_t= qt(c(.025,.975),df=dat_size-1,lower.tail = TRUE)
crit_t
#What is the sample standard error?
sse_mean=pen_sd/sqrt(dat_size)
sse_mean
#Construct the CI 
mean_bill= mean(Gentoo$bill_length_mm,na.rm=TRUE)
Gentoo_CI= mean_bill + crit_t * sse_mean
Gentoo_CI

#Calculate bootstrap 95% CI for Gentoo penguins mean bill length(mm)
#Q6-Q8
#Create an empty vector to hold the bootstrap sample means
m=10000
#numeric() creates an vector of length m with all values initialized to zero
result = numeric(m)
head(result)
#Create the resampled data sets and calculate the means
for(i in 1:m)
{
  result[i] = mean(sample(Gentoo$bill_length_mm, replace=TRUE), na.rm = TRUE)
}
result
#Calculate the CI from the quantiles of the resulting bootstrap means
#What is the CI?
mean_bill2 = mean(result, na.rm = TRUE)
mean_bill2
quantile(result,c(.025,.975))
#The R code I used to call the boot() function.
install.packages("boot")
require(boot)
boot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}
Gentoo_boot = 
  boot(
    data = Gentoo$bill_length_mm,
    statistic = boot_mean,
    R = 10000)
print(Gentoo_boot)
#The R code I used to calculate the upper and lower 2.5% quantiles
quantile(Gentoo_boot$t, c(.025, .975))
#Rarefaction Sampler 
#Q9-Q13
#Completed rarefaction sampler function
rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  for(i in 1:n_iterations)
  {
    for(j in 1:n_input_rows)
    {
      rows_j = sample(n_input_rows, size = j, replace = TRUE)
      t1 = input_dat[rows_j, ]
      t2 = apply(t1, 2, sum)
      results_out[i,j] = sum(t2>0)
    }
  }
  return(results_out)
}
rarefact = rarefaction_sampler(moth_dat, 10000)
head(rarefact)
#The code I used to perform the simulations and construct the curve.
rarefact = rarefaction_sampler(moth_dat, 10000)
rare_mean = apply(rarefact, 2, mean)
rare_quant = apply(rarefact, 2, quantile, probs=c(.025, .975))
rare = t(rbind(rare_mean, rare_quant))
#My rarefaction curve plot and the R-code I used to create the plot.
matplot(
  rare,
  type='l',lwd = 2, col=c(1,4,6),
  xlab='Number of Sampling Sites',
  ylab='Number of Species Observed',
  main='Rarefaction Curve of 10 Rare\n Moth Species Abundances Observed\n in Southeast Massachusetts')

legend(12,4,
  legend=c('mean Abundance','2.5% Confidence Interval','97.5% Confidence Interval'),      
  lty=c(1,5,3),lwd=2, col=c(1,4,6), inset=c(.1,.1))

