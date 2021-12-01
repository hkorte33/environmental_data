#Function to calculate the standard error of the mean
require(palmerpenguins)
sse_mean= function(x)
{sd(x,na.rm=TRUE)/sqrt(length(x)-sum(is.na(x)))
  }
sse_mean(penguins$bill_depth_mm)
#Boxplot of the flipper lengths of three species of penguins
boxplot(flipper_length_mm~species,data=penguins)
#2-species data
dat_pen = subset(penguins, species != "Gentoo")
boxplot(flipper_length_mm ~ species, data = dat_pen)
#Fixing it with droplevels()
dat_pen = droplevels(subset(penguins, species != "Gentoo"))
{
  par(mfrow = c(1, 2))
  boxplot(flipper_length_mm ~ species, data = penguins)
  boxplot(flipper_length_mm ~ species, data = dat_pen)
}
#Resampling with replacement
#For reproducibility
set.seed(123)

flipper_shuffled = sample(penguins$flipper_length_mm, replace = TRUE)
par(mfrow = c(1, 2))
boxplot(flipper_length_mm ~ species, data = penguins)
boxplot(flipper_shuffled ~ penguins$species, xlab = "species")
#T-test-Adelie and Chinstrap penguins
t.test(dat_pen$flipper_length_mm ~ dat_pen$species)
#Two-sample resampling
# Reset the random number generator state for reproduceablility
set.seed(1)
flipper_shuffled = sample(dat_pen$flipper_length_mm)
boxplot(flipper_shuffled ~ dat_pen$species)
#T-test- on reshuffled and resampled data
t_test_1 = t.test(flipper_shuffled ~ dat_pen$species)
t_test_1
#Difference of means
t_test = t.test(dat_pen$flipper_length_mm ~ dat_pen$species)
t_test
t_test$estimate
diff_observed = round(diff(t_test$estimate), digits = 3)
print(diff_observed, digits = 3)
#Using aggregate()
agg_means = aggregate(
  flipper_length_mm ~ species, 
  data = dat_pen, 
  FUN = "mean", 
  na.rm = TRUE)
diff_observed = diff(agg_means[, 2])

agg_means
diff_observed
#Sample size
table(dat_pen$species)

n_1 = 68
n_2 = 152

dat_1 = sample(dat_pen$flipper_length_mm, n_1, replace = TRUE)
dat_2 = sample(dat_pen$flipper_length_mm, n_2, replace = TRUE)

diff_simulated = 
  mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)
print(c(observed = diff_observed, simulated = diff_simulated))
#Simulation function
x = dat_pen$flipper_length_mm
n_1 = 68
n_2 = 152

dat_1 = sample(x, n_1, replace = TRUE)
dat_2 = sample(x, n_2, replace = TRUE)

diff_simulated = 
  mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)
two_group_resample = function(x, n_1, n_2) 
{

  dat_1 = sample(x, n_1, replace = TRUE)
  dat_2 = sample(x, n_2, replace = TRUE)
  
  diff_simulated = 
    mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)
  return(diff_simulated)
}
set.seed(54321)
two_group_resample(dat_pen$flipper_length_mm, 68, 152)
#Resampling experiment
n=2000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$flipper_length_mm, 68, 152)
  )
}
hist(mean_differences)
sum(abs(mean_differences) >= diff_observed)
#Retrieving named elements
t_test = t.test(flipper_shuffled ~ dat_pen$species)
str(t_test)
t_test$estimate
