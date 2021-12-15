#Walkthrough




#Q1-Re-create the conditional boxplot of penguin body mass conditioned on sex and species.
require(palmerpenguins)

boxplot(body_mass_g ~ species + sex, data = penguins,
        main = "Penguin Body Mass by Sex and Species",
        xlab = NULL,
        ylab = "Body Mass (g)",
        names = c("Female\nAdelie", "Female\nChinstrap", "Female\nGentoo", "Male\nAdelie", "Male\nChinstrap", "Male\nGentoo"),
        las = 2,
        col= 3)

#Q2- Based on the boxplots, do you think male penguins (of any species) are significantly heavier than female penguins? Explain your reasoning.
  # Yes, All three male penguin species have means that are much higher than the female penguins.

#Q3- Do you think adding sex to a model that already includes species will improve the model fit?
  #Yes, The variance of the samples will decrease and the box heights will be more constant. 

#Q4- Show the R-code you used to build fit_both
fit_both = lm(body_mass_g ~ species * sex, data = penguins)
summary(fit_both)

#Q5- What is the base case for the two-way model that includes sex and species?
  #The base case is female adelie penguins.

#Q6- What are the names of the two coefficients (from the first column of the coefficient table) you need to calculate the average mass of female Chinstrap penguins?
  #The two coefficients you need are the intercept and speciesChinstrap.

#Q7- What is the predicted average mass of female Chinstrap penguins in the interactive model?
  #The predicted average mass of female Chinstrap penguins is 3527.21g.

#Q8- What is the observed average mass of female Chinstrap penguins, calcluated from the penguins data?
aggregate(
  body_mass_g ~ species + sex,
  data = penguins,
  FUN = function(x) mean(x, na.rm = TRUE)
)

