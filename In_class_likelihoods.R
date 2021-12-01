#Likelinood of Two Observations
x_observed=c(2,6)
print(x_observed)
dpois(x = 2, lambda = 4.5)
dpois(x = 6, lambda = 4.5)
dpois(x = 2, lambda = 4.5) * dpois(x = 6, lambda = 4.5)
wiwa_counts = c(2, 6)
dpois(x = wiwa_counts, lambda = 4.5)
#Product of likelihoods
prod(dpois(x = wiwa_counts, lambda = 4.5))
#Sum of Log-Likelihoods
sum(log(dpois(x = wiwa_counts, lambda = 4.5)))
#Likelihood of Many Observations
dat_bird = read.csv(here::here("data", "bird.sta.csv"))
dat_habitat = read.csv(here::here("data", "hab.sta.csv"))
dat_all = merge(dat_bird, dat_habitat)
#Numerical Data Exploration
summary(dat_all$WIWA)
#Default Histogram
hist(dat_all$WIWA)
#Hist with custom breaks
hist(dat_all$WIWA,breaks = 7)
#Hist with custom breaks attempt 2
hist(dat_all$WIWA, breaks = 0:7)
#Hist with custom breaks attempt 3
0:7-0.5
hist(dat_all$WIWA, breaks = 0:7 - .5)
#Histograms with (discrete) count data
par(mfrow = c(1, 2))
dat = dat_all$WIWA
hist(dat, breaks = 0:(max(dat) + 1) - 0.5, main = "Histogram of\nWilson's Warbler counts")

dat = dat_all$GRJA
hist(dat, breaks = 0:(max(dat) + 1) - 0.5, main = "Histogram of\nGray Jay counts")
#Wilson’s Warbler Sum of Log-Likelihoods
sum(log(dpois(x = dat_all$WIWA, lambda = 1.0)))
#Q1
wiwa_counts = c(2, 6)
dpois(x = wiwa_counts, lambda = 4.5)
sum(log(dpois(x=wiwa_counts, lambda = 3.999)))
#Q2
mean(dat_all$WIWR)
sd(dat_all$WIWR)
sum(log(dpois(x=dat_all$WIWR,lambda=1.46)))
hist(dat_all$WIWR)
hist(dat_all$WIWR,breaks=0:7-.5, main= "Histogram of\nWinter Wren Counts", xlab="Winter Wren Counts")
#Q3- We didn't get to this question or Q4
  #Binomial
summary(dat_all$WIWR)

n=20
prob_guess= 1.46/n

sum(log(dbinom(
  x= dat_all$WIWR,
  size=n,
  prob= prob_guess + 0.0001)))


