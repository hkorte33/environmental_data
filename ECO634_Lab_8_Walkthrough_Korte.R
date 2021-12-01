#Penguin Data
require(palmerpenguins)
penguin_dat= droplevels(
  subset(
    penguins,
    species!= "Gentoo"))

#Parametric Two-Sample Test
t.test(
  flipper_length_mm~species,
  data=penguin_dat,
  alternative="less")

#Bootstrap two-sample test
#install.packages("simpleboot")
require(simpleboot)
Adelie= subset(penguins,species=="Adelie")
Chinstrap=subset(penguins, species=="Chinstrap")
simpleboot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}
#Find the bootstrap for 10000 iterations
penboot = 
  two.boot(
    Adelie$flipper_length_mm,
    Chinstrap$flipper_length_mm,
    simpleboot_mean, 10000)
print(penboot)
str(penboot)
hist(penboot$t)

#Tree data
require(here)
Veg=read.csv(here("data","vegdata.csv"))
boxplot(
  pine~treatment,
  dat=Veg)

#Tree treatments and boxplot
dat_tree = droplevels(
  subset(
    Veg, 
    treatment %in% c("control", "clipped")))
boxplot(pine~treatment,data=dat_tree)
#table() to determine how many observations are in each of the treatments.
table(
  subset(
    Veg,
    treatment=="clipped")$pine) 

table(
  subset(
  Veg,treatment=="control")$pine)

#Nonparametric two-sample test
##Conduct a Wilcoxon ranked sum test on the difference in means between the treatments.
wilcox.test(pine~treatment,data=dat_tree)
#What is the p-value?
  #.1005

#Bootstrap of tree data
require(boot)
tree_boot = 
  two.boot(
    subset(dat_tree, treatment == "clipped")$pine,
    subset(dat_tree, treatment == "control")$pine,
    FUN = mean,
    R = 10000,
    na.rm = TRUE
  )

boot.ci(tree_boot)

hist(tree_boot$t,main="Bootstrap sampling distribution")
quantile(tree_boot$t,0.025)

#Bird data
  #Read the data files and merge into a single data.frame
require(here)
dat_bird = read.csv(here("data","bird.sub.csv"))
dat_habitat = read.csv(here("data","hab.sub.csv"))

dat_all = merge(
  dat_bird, 
  dat_habitat,
  by = c("basin", "sub"))

head(dat_all[, c("b.sidi", "s.sidi")])

#Illustrate the z-standardization process for both columns
# Calculate the sample mean and sd:
  #b.sidi
b_sidi_mean = mean(dat_all$b.sidi, na.rm = TRUE)
b_sidi_sd   = sd(dat_all$b.sidi, na.rm = TRUE)

# Use the subset-by-name symbol ($) to create a 
# new column of z-standardized values.

dat_all$b.sidi.standardized = (dat_all$b.sidi - b_sidi_mean)/b_sidi_sd
#examine the standardized data to see if our standardization worked
mean(dat_all$b.sidi.standardized)
sd(dat_all$b.sidi.standardized)

#s.sidi
s_sidi_mean = mean(dat_all$s.sidi, na.rm = TRUE)
s_sidi_sd   = sd(dat_all$s.sidi, na.rm = TRUE)

# Use the subset-by-name symbol ($) to create a 
# new column of z-standardized values.

dat_all$s.sidi.standardized = (dat_all$s.sidi - s_sidi_mean)/s_sidi_sd
#examine the standardized data to see if our standardization worked
mean(dat_all$s.sidi.standardized)
sd(dat_all$s.sidi.standardized)

#Model Variables
  #Plotting the data
plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")

#Simple Linear Regression
fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
coef(fit_1)
slope_observed=coef(fit_1)[2]
plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_1)

#The slope coefficient
dat_1=
  subset(
    dat_all,
    select = c(b.sidi,s.sidi))

#Resampling the data
index_1 = sample(nrow(dat_1), replace = TRUE)
index_2 = sample(nrow(dat_1), replace = TRUE)

dat_resampled_i = 
  data.frame(
    b.sidi = dat_1$b.sidi[index_1],
    s.sidi = dat_1$s.sidi[index_2]
  )

fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
slope_resampled_i = coef(fit_resampled_i)[2]

print(slope_resampled_i)
#Recreating the scatterplot with regression line
plot(
  b.sidi ~ s.sidi, data = dat_resampled_i,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_resampled_i)

#Randomization Loop
m=10000
result=numeric(m)

for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE)
  
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index_1],
      s.sidi = dat_1$s.sidi[index_2]
    )
  
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i) 
  
  result[i] = coef(fit_resampled_i)[2]
} 

#The Null Distribution
  #Plot a histogram of the slope values
hist(result, main = "Null Distribution of Regression Slope", xlab = "Slope Parameter")
abline(v = slope_observed, lty = 2, col = "red", lwd = 2)

#Critical Slope Value
quantile(result,c(.05))
result<slope_observed
sum(
  result<slope_observed
)

sum(result<slope_observed)/m


