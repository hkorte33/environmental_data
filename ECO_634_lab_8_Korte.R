#Q1:Calculate the sd of the differences in mean flipper length from bootstrap simulation. 
Adelie = subset(penguins, species == "Adelie")
Chinstrap = subset(penguins, species == "Chinstrap")

boot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}

pen_boot = 
  two.boot(
    Adelie$flipper_length_mm, 
    Chinstrap$flipper_length_mm,
    FUN = boot_mean,
    R = 10000,
  )

sd(pen_boot$t)

#Q2- Histogram of above info
hist(
  pen_boot$t, 
  main = "Adelie and Chinstrap Mean Flipper Length", 
  xlab = "Difference in Mean Flipper Length (mm)", col = 3
  )

#Q3- The 95% bootstrap CI
quantile(pen_boot$t,c(.025,.975))

#Q4- Expain why its not a skewed distribution

#Q5- Show the R-code you used to create pen_ecdf()
pen_ecdf= ecdf(pen_boot$t)

#Q6- What is the probability, according to the empirical distribution function, 
  #of observing a mean difference of -4.5 or greater? 
1 - pen_ecdf(-4.5)

#Q7-What is the probability, according to the empirical distribution function, 
#of observing a mean difference of -8 or smaller?
pen_ecdf(-8)

#Q8- State the null and alternative hypotheses of a two-sample, two-tailed test 
#for the difference in mean flipper lengths between the two penguin species.

#Q9- What was the p-value? Show the R-code.
require(here)
veg = read.csv(here("data", "vegdata.csv"))

dat_veg = droplevels(
  subset(
    veg, 
    treatment %in% c("control", "clipped")))

wilcox.test(
  pine ~ treatment, 
  data = dat_veg)

#Q10- What were the endpoints of your bootstrap CI?
tree_boot = 
  two.boot(
    subset(dat_veg, treatment == "clipped")$pine,
    subset(dat_veg, treatment == "control")$pine,
    FUN = mean,
    R = 1000,
    na.rm = TRUE
  )
quantile(tree_boot$t,c(.025,.975))

#Q11-  What is the observed difference in mean tree counts and does it
#fall within the 95% bootstrap CI?
mean(subset(dat_veg, treatment == "clipped")$pine) -
  mean(subset(dat_veg, treatment == "control")$pine)

#Data for Q12-17
dat_bird = read.csv(here("data", "bird.sub.csv"))
dat_hab = read.csv(here("data", "hab.sub.csv"))

dat_all = merge(
  dat_bird,
  dat_hab,
  by = c("basin", "sub"))

b_sidi_mean = mean(dat_all$b.sidi, ra.rm = TRUE)
b_sidi_sd = sd(dat_all$b.sidi, na.rm = TRUE)

dat_all$b.sidi.standardized = (dat_all$b.sidi - b_sidi_mean)/b_sidi_sd

#Q12- Briefly describe the Simpson diversity index, and explain what it quantifies.

#Q13- Show the code you used to z-standardize the s.sidi column.
s_sidi_mean = mean(dat_all$s.sidi, ra.rm = TRUE)
s_sidi_sd = sd(dat_all$s.sidi, na.rm = TRUE)

dat_all$s.sidi.standardized = (dat_all$s.sidi - s_sidi_mean)/s_sidi_sd

#Q14- Show the code for your completed loop.
fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
slope_observed = coef(fit_1)[2]

dat_1 =
  subset(
    dat_all,
    select = c(b.sidi, s.sidi))

m = 10000
result = numeric(m)

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

#Q15-Histogram with vertical lines showing the observed slope
#and the critical value from the resampled MC slopes.
hist(result, main = "Monte Carlo Resampling:
Simpson's Diversity Indices for Vegetation & Birds", 
     xlab = "Slope Parameter", col="gray")
abline(v = slope_observed, col = "blue", lwd = 2)
abline(v = quantile(result, 0.05), col = "red", lty = "dotted", lwd = 2)
  
#Q16-What was your critical value? 
#Was the observed slope less than the critical value? 
quantile(result,0.05)
print(slope_observed)

#Q17- What is your conclusion regarding the evidence of a negative 
#relationship between vegetation cover diversity and bird diversity? 



