#Chi-square test on the table of Brown Creeper presence/absence in edge and interior habitats.
require(here)
birds = read.csv(here("data","bird.sta.csv"))
hab = read.csv(here("data", "hab.sta.csv"))
birdhab = merge(birds, hab, by = c("basin", "sub", "sta"))

table(birdhab$s.edge, birdhab$BRCR > 0)

br_creeper_table = table(birdhab$s.edge, birdhab$BRCR > 0)[, 2:1]

chisq.test(br_creeper_table)

#Q1:State the null hypothesis of the Chi-square test.
  #The null hypothesis is that there’s no relationship between Brown Creeper presence/absence in edge and interior habitats. 

#Q2:Explain whether you think that Brown Creepers show a significant habitat preference.
  #Based on the results from my test, it appears that Brown Creepers prefer interior 
  #habitats over exterior habitats. This can be observed due to the very low p-value 1.386e-06.  

#Q3:R-code to create a model fit of penguin body mass as predicted by penguin species.
require(palmerpenguins)
fit_species = 
  lm(
    formula = body_mass_g ~ species,
    data = penguins)

#Q4:R-code to create a model fit of penguin body mass as predicted by sex.
fit_sex=
  lm(
    formula=body_mass_g ~ sex,
    data= penguins
  )

#Q5:R-code to create a model fit of penguin body mass as predicted by species and sex.
fit_both=
  lm(
    formula= body_mass_g ~ sex + species,
    data= penguins
  )

#Q6:Include a conditional boxplot corresponding to your fit_species model.
boxplot(body_mass_g ~ species, data= penguins,
    main = "Conditional Boxplot: 
    Body Mass by Penguin Species",
    xlab = "Penguin Species",
    ylab = "Body Mass (g)",
    col= "orange")

#Q7:Include a conditional boxplot corresponding to your fit_sex model.
boxplot(body_mass_g ~ sex, data = penguins,
        main = "Conditional Boxplot:
        Body Mass by Penguin Sex",
        xlab = "Sex",
        ylab = "Body Mass (g)",
        col= "green")

#Q8:Include a conditional boxplot corresponding to your fit_both model.
boxplot(body_mass_g ~ species + sex, data = penguins,
        main = "Conditional Boxplot:
        Body Mass by Penguin Species and Sex",
        xlab = NULL,
        ylab = "Body Mass (g)",
        names = c("Female\nAdelie", "Female\nChinstrap", "Female\nGentoo", "
                  Male\nAdelie", "Male\nChinstrap", "Male\nGentoo"),
        las = 2,
        col= rainbow(6))

#Q9:Which of the models do you think may have problems fulfilling the homogeneity assumption?
  # I think the fit_both model might have problems fulfilling the homogeneity assumption 
  #because we’re comparing variances between two models using different numbers of variables. 

#Q10:State the null hypothesis of the Bartlett test
  #The null assumption of the Bartlett test is that the sample variances are the same.

#Q11:What was the p-value from the Bartlett test of homogeneity for observations grouped by species?
bartlett.test(body_mass_g ~ species, data = penguins)

#Q12:What was the p-value from the Bartlett test of homogeneity for observations grouped by sex?
bartlett.test(body_mass_g ~ sex, data = penguins)

#Q13:What was the p-value from the Bartlett test of homogeneity for observations grouped by both factors?
dat_groups = aggregate(
  body_mass_g ~ species + sex,
  data = penguins,
  FUN = c)

bartlett.test(dat_groups$body_mass_g)

#Q14:Based on the results of the Bartlett tests, do you anticipate any issues with heterogeneity in any of the models? 
  # I anticipate the fit_both to have issues with heterogeneity. I came to this conclusion because when grouped by 
  #both sex and species factors, the p-value was > 0.05, which means I failed to reject the null hypothesis of the 
  #sample variables being the same.










