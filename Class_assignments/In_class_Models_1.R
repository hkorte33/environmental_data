require(palmerpenguins)
dat_ade=droplevels(subset(penguins,species=="Adelie"))
hist(
  dat_ade$body_mass_g,
  main="Adelie Penguins: Body Mass",
  xlab="Body Mass (g)")

#Q1:Boxplots of body mass for male and female Adelie penguins
boxplot(body_mass_g~sex,
  data = dat_ade, 
  main="Body Mass of Adelie Penguins",
  ylab="Body Mass (g)")

#Q2:One-sample t-test of alternative hypothesis: female Adelie penguins have a body mass different from zero grams
sex_female= subset(dat_ade,sex=="female")
t.test(sex_female$body_mass_g,mu=0)
  #It is a two-tailed test because we used the default value of “alternative”, which is two-tailed.

#Q3:Describe your conclusions based on the p-value of the t-test.
  #The body mass of female Adelie penguins is different from a value of 0.

#Q4:one-sample t-test of the null hypothesis that male Adelie penguins have a mean body mass greater than 4000 grams.
sex_male=subset(dat_ade,sex=="male")
t.test(sex_male$body_mass_g,
       mu=4000,
       alternative = "greater")
  #This test would be a one-tailed test because we are looking at body mass specifically being larger than 4000g, rather than the body mass just being different from 4000g.

#Q5:Describe your conclusions based on the p-value of the t-test
  #The p value we got was 0.1438, above a significance level of .05. This tells us that the male body mass is not larger than a mean of 4000 grams.

#Q6:two-sample t-test of the alternative hypothesis that male and female Adelie penguins have different mean body masses.
t.test(x=sex_female$body_mass_g,
       y=sex_male$body_mass_g,
       alternative = "t")

#Q7:Describe your conclusions based on the p-value of the t-test.
  #The p-value is very small, which means that the mean body mass is different between male and female Adelie penguins.

#Q8:Conduct a two-sample (one-tailed) t-test of the directional alternative hypothesis that male Adelie penguins are heavier than females.
t.test(x=sex_male$body_mass_g, 
       y=sex_female$body_mass_g, 
       alternative = "greater")
#Q9:Conduct a two-sample (one-tailed) t-test of the directional alternative hypothesis that male Adelie penguins are lighter than females.
t.test(x=sex_female$body_mass_g, 
       y=sex_male$body_mass_g, 
       alternative = "greater")

#Q10:Explain why the p-values are so drastically different in the two directions.
  #Using the two-sided t-test, we didn’t know which sex was larger, 
  #but using the p-values from the t-tests we are able to determine that the mean male mass is larger than female mass. 
  #The two-sided t- tests do not tell us which is larger because it only has a directional output. 
  #Using the two different t-tests with the different alternatives (greater or less) we are able to see the p values that describe a specific directionality.









