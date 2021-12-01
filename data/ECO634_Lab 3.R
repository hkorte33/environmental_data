install.packages("psych")
require(psych)
pairs.panels(iris)
require(here)
dat_bird=read.csv(
  here("data","bird.sta.csv")
)
head(dat_bird)
require(here)
dat_habitat=read.csv(
  here("data","hab.sta.csv")
)
head(dat_habitat)
dat_all<-merge(dat_bird,dat_habitat)
dat_all
plot(ba.tot~ elev,data=dat_all)
sample(dat_all$CEWA, 100)
dat_all$CEWA > 1
as.numeric(dat_all$CEWA >1)
cewa_present_absent<- as.numeric(dat_all$CEWA>1)
cewa_present_absent[TRUE]
plot(x=dat_all$elev, y=cewa_present_absent)
# Function to calculate the logistic parameter a given the slope and midpoint
get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}
# Function to calculate the logistic parameter b given the slope
get_logistic_param_b = function(slope)
{
  return (slope / 4)
}
# Calculate the value of the logistic function at x, given the parameters a and b.
logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}
# Calculate the value of the logistic function at x, given a slope and midpoint.
logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}
par(mfrow=c(1,1))
#positive slope
plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = 0.1), add = TRUE)
#negative slope
plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.1), add = TRUE)
#shallow negative slope
plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.05), add = TRUE)
#pairplot panel for terrain and basal area
dat_terrain=dat_all[c("elev","aspect","slope","ba.tot")]
pairs.panels(dat_terrain,main="Pairplots of Elevation, Aspect, Slope, and Total Basal Area")
#song sparrow logical model
SOSP_present_absent=as.numeric(dat_all$SOSP>1)
plot(x = dat_all$ba.tot, y = SOSP_present_absent, main= "Song Sparrow vs Basal area", xlab= "Basal area (m2 per ha)", ylab="Song Sparrow presence or absence")
curve(logistic_midpoint_slope(x, midpoint = 75, slope = -0.9), add = TRUE)
# Purple Finch logical model
PUFI_present_absent=as.numeric(dat_all$PUFI>1)
plot(x = dat_all$ba.tot, y = PUFI_present_absent, main= "Purple Finch vs Basal area", xlab= "Basal area (m2 per ha)", ylab="Purple Finch presence or absence")
curve(logistic_midpoint_slope(x, midpoint = 45, slope = -0.9), add = TRUE)
#Total number of Gray Jays observed
sum(dat_all$GRJA)
#Total number for sampling sites where Gray Jays were observed
dat_all$GRJA>=1
sum(dat_all$GRJA>=1)
