here()
dat_dist=read.csv("data/dispersal.csv")
#Q2 Exponential Functions-Different Curves
exp_fun = function(x, a, b) 
{
  return(a * exp(-b * x))
}
curve(
  exp_fun(x,1.9,0.1),add=TRUE,from=0,to=50,
  main= "Negative Exponential Curves",ylab="y",xlab="x",col="black",
  lty="solid")
curve(
  exp_fun(x,1.9,0.3),add=TRUE,from=0,to=50,col="black",
  lty="dotted")
curve(
  exp_fun(x,1.2,0.2),add=TRUE,from=0,to=50,col="red",
  lty="solid")
curve(
  exp_fun(x,1.2,0.4),add=TRUE,from=0,to=50,col="red",
  lty="dotted")
#Q5 Ricker Functions-Different Curves
ricker_fun = function(x, a, b) 
{
  return(a *x* exp(-b * x))
}
curve(
  ricker_fun(x, 25, .1),add=TRUE, 
  col = "black", lty = "solid", from = 0, to = 75,
  main="Ricker Curves",ylab="y",xlab="x")
curve(
  ricker_fun(x, 20, .2), 
  col = "black", lty = "dotted", add = TRUE)
curve(
  ricker_fun(x, 10, .2), 
  col = "black", lty = "dotted", add = TRUE)
curve(
  ricker_fun(x, 75, .3), 
  col = "red", lty = "solid", add = TRUE)
curve(
  ricker_fun(x, 50, .3), 
  col = "red", lty = "dotted", add = TRUE)
curve(
  ricker_fun(x, 40, .3), 
  col = "red", lty = "dotted", add = TRUE)
#Q8-13-Salamander Models
#FTB Linear Fit
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
guess_x= 500
guess_y=0.40
guess_slope= -0.00042
plot(dat_dist$dist.class,dat_dist$disp.rate.ftb,
     main= "Salamander First-Time Breeders Linear Fit",
     xlab="Distance Class (m)",ylab="Dispersal Rate")
curve(line_point_slope(x,guess_x,guess_y,guess_slope),add=TRUE)
#Scatterplot of sal data w/fitted exponentail model
exp_fun = function(x, a, b) 
{
  return(a * exp(-b * x))
}
plot(dat_dist$dist.class,dat_dist$disp.rate.ftb,
     main= "Salamander First-Time Breeders Exponential Fit",
     xlab="Distance Class (m)",ylab="Dispersal Rate")
curve(exp_fun(x,1,0.0041),add=TRUE)
#Scatterplot of sal data w/fitted Ricker Model
ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}
plot(dat_dist$dist.class,dat_dist$disp.rate.ftb,
     main= "Salamander First-Time Breeders Ricker Fit",
     xlab="Distance Class (m)",ylab="Dispersal Rate")
curve(ricker_fun(x,0.008,0.0043),add=TRUE)
#Salamander Model Residuals
par(mfrow=c(3,1))
dat_dist$linear = line_point_slope(dat_dist$dist.class, guess_x, guess_y, guess_slope)
dat_dist$exp = exp_fun(dat_dist$dist.class, 1, .0041)
dat_dist$ricker = ricker_fun(dat_dist$dist.class, .008, .0043)

dat_dist$resids_linear = dat_dist$disp.rate.ftb - dat_dist$linear
dat_dist$resids_exp = dat_dist$disp.rate.ftb - dat_dist$exp
dat_dist$resids_ricker = dat_dist$disp.rate.ftb - dat_dist$ricker

hist(dat_dist$resids_linear, main = "Histogram of Linear Fit Residuals" ,xlab = "Residual Value", col=15)
hist(dat_dist$resids_exp, main = "Histogram of ExponentialFit Residuals",xlab = "Residual Value", col=5)
hist(dat_dist$resids_ricker, main = "Histogram of Ricker Fit Residuals",xlab = "Residual Value", col=2)

data_frame = data.frame(dat_dist$resids_exp, dat_dist$resids_linear, dat_dist$resids_ricker)










