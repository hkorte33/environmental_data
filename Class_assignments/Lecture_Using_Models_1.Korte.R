#Data
require(here)
catrate= read.csv(here("data", "catrate.csv"))

#Q1-Create a histogram of the salamander reproduction catastrophic rates.
hist(
  catrate$cat.rate, 
  main = "Histogram of Salamander Reproduction
  Catastrophe Rates", 
  xlab = "Catastrophe Rate", col=3)

#Q2-Conduct a Shapiro-Wilk test of normality of the salamander catastrophic rates. 
#Report the p-value and show the R-code you used to conduct the test.
shapiro.test(catrate$cat.rate)

#Q3-What is the null hypothesis for the Shapiro test?
  #The null hypothesis for the Shapiro test is a normally distributed data set.

#Q4-Based on the Shapiro test results, is there strong evidence that the sample
#came from a non-normally-distributed population?
  #Yes because the p-value was less than 0.05.

#Q5-Show the code you used to conduct the t-test.
t.test(catrate$cat.rate,mu=2/7)

#Q6-State the null hypothesis of the test, in plain nontechnical English.
  #The mean of the two rates are the same (0.28). 

#Q7-Is this a one- or two-tailed test?
  #Two-tailed because we are comparing the means of the two rates.

#Q8-What is the p-value from your t-test?
#Interpret the p-value as a false-positive rate.
  #P-value is .01193. There is no correlation between the two rates and only 1.19% of the time they are equal.

#Q9-What is the confidence interval for the difference between the null hypothesis and alternative hypothesis means? 
#Did it include zero?
  #The confidence interval is 0.3526250  to 0.7261295 and does not include zero. 

#Q10-Did you conclude that there was strong evidence to reject the null hypothesis?
  # Reject it because p-value is less than 0.05.

#Q11-Show the code you used to conduct the test.
wilcox.test(catrate$cat.rate, mu = 2/7)

#Q12-Compare the p-value with the p-value you got from the t-test.
  #The p-value of the Wilcoxon test is 0.006275, which is lower than the t-test p-value of 0.01193. 

#Q13- From the results of the rank sum test, did you conclude strong evidence to reject the null hypothesis?
  #Reject it because p-value is less than 0.05.

#Q14-Compare the overall conclusions you could draw from the results of the two tests.
  #Both p-vaues are below 0.05, so should be rejected but wilcoxon test p-value is smaller

#Q15-Considering the numerical and graphical data exploration, which test do you think was more appropriate for these data?
  #Wilcoxon is better because data sets aren't normally distributed.
#Data for Q16-20
require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))

dat_Adelie = subset(penguin_dat, species == "Adelie")
dat_Chinstrap = subset(penguin_dat, species == "Chinstrap")

#Q16-Show the R-code you used to conduct tests of normality for the flipper lengths of Chinstrap and Adelie penguins.
shapiro.test(dat_Adelie$flipper_length_mm)
shapiro.test(dat_Chinstrap$flipper_length_mm)

#Q17-Do you conclude that the flipper lengths are normally-distributed for each species?
  #They are normally distributed bc he p-value was greater than 0.05 in each Shapiro test.

#Q18- Save your figure (Histogram) to a file and include it in your report.
  #File attachment in moodle
png(
  filename = here("images", "models_1_hist.png"),
  width = 1500, height = 800, units = "px")

par(mfrow = c(1,2))

hist(
  dat_Adelie$flipper_length_mm, 
  main = "Adelie Flipper Lengths", 
  xlab = "Flipper Length (mm)", 
  col = 3
  )

hist(
  dat_Chinstrap$flipper_length_mm, 
  main = "Chinstrap Flipper Lengths", 
  xlab = "Flipper Length (mm)",
  col = 4)
dev.off()

#Q19-State the alternative hypothesis of the test.Consider whether you used a one- or two- tailed test.
  #The alternative hypothesis is that the mean flipper lengths between the two species are different from each other.
  #Also a two-tailed test because we are comparing the means of the two species' flipper lengths. 

#Q20- Include the code you used to conduct the t-test.
t.test(flipper_length_mm ~ species, data = penguin_dat)




