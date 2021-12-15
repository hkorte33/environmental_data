#Sample Standard Error Function
rm(list = ls())

sse_mean = function(x)
{
  sd(x, na.rm = TRUE) / sqrt(length(x) - sum(is.na(x)))
}
sse_mean(penguins$body_mass_g)
sse_mean(mtcars$mpg)
#Two Group Resampling
set.seed(54321)
two_group_resample = function(x, n_1, n_2)
{
  mean(
    sample(x, n_1, replace = TRUE), na.rm = TRUE) - mean(
      sample(x, n_2, replace = TRUE), na.rm = TRUE)
}
#Resampling Flipper Lengths
n = 2000
dat_pen = droplevels(subset(penguins, species != "Gentoo"))
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$flipper_length_mm, 68, 152)
  )
}
#Histogram of resampled differences of means
hist(mean_differences, main="Histogram of Resampled Mean\nFlipper Length Differences",
xlab="Mean Differences",col=4)
#Number of differences of means greater than 5.8
agg_means = aggregate(
  flipper_length_mm ~ species,
  data = dat_pen,
  FUN = mean,
  na.rm = TRUE)

diff_observed = diff(agg_means[,2])

sum(abs(mean_differences) >= diff_observed)
#Boxplot of chosen variable
boxplot(dat_pen$bill_length_mm ~ dat_pen$species,
main = "Penguin Bill Lengths",
xlab="Species",ylab="Bill length (mm)", col= 15) 
#Group means and difference between them
agg_pen_means = aggregate(
  bill_length_mm ~ species,
  data = dat_pen,
  FUN = mean,
  na.rm = TRUE)
agg_pen_means
diff_crit = diff(agg_pen_means[,2])
diff_crit
#P-value from t-test output
t.test(dat_pen$bill_length_mm ~ dat_pen$species)
#Number of differneces in means that are greater than diff_crit
n = 2000
mean_differences_bill = c()
for (i in 1:n)
{
  mean_differences_bill = c(
    mean_differences_bill,
    two_group_resample(dat_pen$bill_length_mm, 68, 152)
  )
}

sum(abs(mean_differences_bill) >= diff_crit)
#Histogram of differences of means-Bill length
hist(mean_differences_bill, main = "2000 Trial Simulated Mean Difference in\nPenguin Bill Length", 
xlab = "Mean Difference in Bill Length (mm)", col=5)





