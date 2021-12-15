#Q1- Submit the code you used to build your ANOVA by hand.
rm(list = ls())
require(here)

rope = read.csv(here("data","rope.csv"))
rope$rope.type = factor(rope$rope.type)
                        
    n_obs = nrow(rope)
    n_groups = length(levels(rope$rope.type))
                        
    ss_tot = sum((rope$p.cut - mean(rope$p.cut))^2)
    df_tot = (n_obs-1)
    
    agg_sq_resids = aggregate(
      x = rope$p.cut,
      by = list(rope$rope.type),
      FUN = function(x) sum((x - mean(x))^2))
    
    ss_within = sum(agg_sq_resids$x)
    df_within = (n_obs-n_groups)
                        
    ss_among = (ss_tot - ss_within)
    df_among = (n_groups-1)
                        
    ms_within = ss_within / (n_obs - n_groups)
    ms_among  = ss_among / (n_groups - 1)
                        
    f_ratio = ms_among/ ms_within
    f_pval = 1-pf(f_ratio, df_among, df_within)
#Q1- self check
    # number comparison tolerance
    digits_check = 5
    
    # Build the reference model using R functions
    fit_1 = lm(p.cut ~ rope.type, data=rope)
    anova(fit_1)
    anova_fit_1 = anova(fit_1)
    
    # Check degrees of freedom
    anova_fit_1$Df == c(df_among, df_within)
    
    # Check sums of squares
    round(anova_fit_1$`Sum Sq`, digits = digits_check) == round(c(ss_among, ss_within), digits = digits_check)
    
    # Check mean squares
    round(anova_fit_1$`Mean Sq`, digits = digits_check) == round(c(ms_among, ms_within), digits = digits_check)
    
    # Check the F-ratio
    round(anova_fit_1$`F value`[1], digits = digits_check) == round(f_ratio, digits = digits_check)
    
    # Check the F test statistic p-value
    round(anova_fit_1$`Pr(>F)`[1], digits = digits_check) == round(f_pval, digits = digits_check)  
    
#Q2- Examine the conditional boxplot in the Partitioning Variance: Within-Group section of the walkthrough.
#Based on the figure, do you think there are equal variances among the groups?
    #Based on the figure, I don’t think there are equal variances among the groups.
    #The rope type boxes visually have widely-varying heights from each other.

#Q3- Conduct a Bartlett test to assess the homogeneity of variances of the percent cut among the rope type groups. Report the p-value.
bartlett.test(p.cut ~ rope.type, data = rope)
    
#Q4- Given your graphical assessment and the Bartlett test, do you think an ANOVA-type analysis is appropriate on the raw data? Explain why or why not.
 #No, ANOVA assumes contant varaible and p-value was <0.05, so I can reject the null hypothesis of equal variance among the groups.

#Q5-7 Data
fit_rope_1 = lm(p.cut ~ rope.type, data = rope)
summary(fit_rope_1)

#Q5- Which rope type is the base case?
  #Blaze

#Q6- What is the mean percent cut of the base case rope? 
  #It is 0.36714. doesn't require any calculation, since the intercept coefficient is interpreted as the intercept mean but I here is the calculation anyway. 
blaze = subset(rope, rope.type == "BLAZE")
mean(blaze$p.cut)

#Q7-What is the mean percent cut rope type XTC? 
rope_XTC = subset(rope, rope.type == "XTC")
mean(rope_XTC$p.cut)


    
    
    
    
    