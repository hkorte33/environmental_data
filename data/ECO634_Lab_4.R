#Generate a vector of x-values
x=seq(-3,3, length.out = 1000)
y=dnorm(x)
plot(x, y, main="Normal PDF", type="l")
abline(h=0)
#Penguin data example
require(palmerpenguins)
hist(
  penguins$body_mass_g,
  main="Histogram of Penguin Body Mass",
  xlab= "Body Mass(g)" )
mean(penguins$body_mass_g, na.rm=TRUE)
sd(penguins$body_mass_g, na.rm=TRUE)
nrow(penguins)
mean(penguins$body_mass_g, na.rm=FALSE)
sd(penguins$body_mass_g, na.rm=FALSE)
#Random Penguin Masses and Histograms
set.seed(1)
n_samples = 344
pop_sd = 802
pop_mean = 4202

dat_1 = rnorm(n = n_samples, mean = pop_mean, sd = pop_sd)
dat_2 = rnorm(n = n_samples, mean = pop_mean, sd = pop_sd)
dat_3 = rnorm(n = n_samples, mean = pop_mean, sd = pop_sd)
dat_4 = rnorm(n = n_samples, mean = pop_mean, sd = pop_sd)
par(mfrow=c(2,2))
hist(dat_1)
hist(dat_2)
hist(dat_3)
hist(dat_4)
#Random Unifrom Numbers
set.seed(12)
dat_unif= runif(n=270,min=0,max=4)
hist(dat_unif)
set.seed(1)
dat_unif_1 = runif(n = 270, min = 0, max = 4)
set.seed(1)
dat_unif_2 = runif(n = 270, min = 0, max = 4)

par(mfrow = c(1, 2))
hist(dat_unif_1)
hist(dat_unif_2)
#Measuring error-Residuals
set.seed(123)
n = 17
slope = 0.7
intcp = 0.2

guess_x = 6
guess_y = 4
guess_slope = 0.72

x = runif(n = n, min = 1, max = 10)
y = rnorm(n = n, mean = slope * x + intcp)

plot(x, y, pch = 16)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)
#create the data
set.seed(420)
n_pts = 10
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)

dat = data.frame(x = x, y_observed = rnorm(n_pts))

plot(y_observed ~ x, data = dat, pch = 8)
#Fit linear deterministic model
set.seed(123)
n_pts = 10
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)

dat = data.frame(x = x, y_observed = rnorm(n_pts))

plot(y_observed ~ x, data = dat, pch = 8)

guess_x = 6
guess_y = 0
guess_slope = 0.1

plot(y_observed ~ x, data = dat, pch = 8)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)
dat$y_predicted=line_point_slope(dat$x,guess_x,guess_y,guess_slope)
head(dat)
dat$resids=line_point_slope(dat$x,guess_x,guess_y,guess_slope)
head(dat)
sum(dat$resids)
abs(dat$resids)
#Creating four vectors
set.seed(1)
n_17=17
n_30 = 30
n_300= 300
n_3000= 3000
vec_sd = 2.4
vec_mean = 10.4

norm_17 = rnorm(n = n_17, mean = vec_mean, sd = vec_sd)
norm_30 = rnorm(n = n_30, mean = vec_mean, sd = vec_sd)
norm_300 = rnorm(n = n_300, mean = vec_mean, sd = vec_sd)
norm_3000 = rnorm(n = n_3000, mean = vec_mean, sd = vec_sd)
#Normal Vectors:Histograms
require(here)
png(
  here("images", "lab_04_hist_01.png"),
  width= 1500, height=1600, res=180)

par(mfrow=c(2,2))
hist(norm_17, main= "17 Element Vector",xlab="Random Generated Points")
hist(norm_30, main= "30 Element Vector",xlab="Random Generated Points")
hist(norm_300, main= "300 Element Vector",xlab="Random Generated Points")
hist(norm_3000, main= "3000 Element Vector",xlab="Random Generated Points")
dev.off()
#Normal Density Function
require(here)
n_mean=10.4
n_sd=2.4
x=seq(-50,50,length.out = 1000)
y=dnorm(x,mean=n_mean,sd=n_sd)

pdf(
  here("images","norm_1.pdf"), 
  width = 7,height = 7 
  )
plot(x, y, main = "Normal PDF: Mean = 10.4, SD = 2.4", type = "l", xlim = c(3,20))
abline(h = 0)
dev.off()
#Random Data Sets
require(here)
set.seed(223)
n_pts = 500
x_min = 1
x_max = 35
pdf(
  here("images","random_data_1.pdf"),
  width=7,height=7
)
par(mfrow=c(2,2))
x = runif(n = n_pts, min = x_min, max = x_max)
dat1 = data.frame(x = x, y_observed = rnorm(n_pts))
plot(y_observed ~ x, data = dat1, pch = 8, col=12, main= "dat1 Plot")
x = runif(n = n_pts, min = x_min, max = x_max)
y = runif(n = n_pts, min = x_min, max = x_max)
dat2 = data.frame(x = x,y=y)
plot(dat2,col=2,main= "dat2 Plot")
x2 = runif(n = n_pts, min = x_min, max = x_max)
y2 = rnorm(n = n_pts, mean=5,sd=1)
dat3= data.frame(x=x2,y=y2)
plot(dat3,col=6,main= "dat3 Plot")
x3 = rnorm(n = n_pts, mean=7,sd=2)
y3 = rnorm(n = n_pts, mean=5,sd=1)
dat4= data.frame(x=x3,y=y3)
plot(dat4,col=15,main= "dat4 Plot")
dev.off()
#Random Dataset Model Fit
require(here)
set.seed(223)
n_pts = 500
x_min = 1
x_max = 35
pdf(
  here("images","random_data_2.pdf"),
  width=7,height=7
)
x = runif(n = n_pts, min = x_min, max = x_max)
y = runif(n = n_pts, min = x_min, max = x_max)
dat2 = data.frame(x = x,y=y)
#line_point_slope function
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}
#plot dat2
plot(dat2,col=2,main= "dat2 Plot")
#fit curve
guess_x= 2
guess_y= 0.5
guess_slope= 0.9
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = TRUE)
dev.off()
#Random data model residuals
set.seed(223)
n_pts = 500
x_min = 1
x_max = 35
x = runif(n = n_pts, min = x_min, max = x_max)
y = runif(n = n_pts, min = x_min, max = x_max)
dat2 = data.frame(x = x,y=y)
plot(dat2,col=2, main= "dat2 Plot")
#calculate and add predicted y-values
dat2$y_predicted = line_point_slope(dat2$x, guess_x, guess_y, guess_slope)
#calculate and add residuals
dat2$resids = (dat2$y - dat2$y_predicted)
#Histogram of model's residuals
hist(dat2$resids, main = "dat2 Fitted Line Residuals", xlab = "Residual Values", col =2)
#Scatterplot of model's predicted values and residuals
plot(dat2$y_predicted, dat2$resids, main = "dat2 Predicted Values vs. Residuals", 
     xlab = "Predicted Values", ylab = "Residuals", pch = 22, col = 14)
       

