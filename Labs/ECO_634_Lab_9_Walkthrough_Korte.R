#Catastrophic Rate Data
require(here)
catrate = read.csv(here("data", "catrate.csv"))
head(catrate)

#Reproductive Success and Failure
n_success=sum(catrate$success)
n_years=sum(catrate$years)
binom.test(n_success,n_years)

#Reproductive Catastrophe and Late Filling
late_fill_rate = 2/7
normal_fill_rate = 1 - late_fill_rate

  #Two-sided alternative test
binom.test(
  n_success,
  n_years,
  p= normal_fill_rate)

  #One-sided alternative test
binom.test(
  n_success,
  n_years,
  p = normal_fill_rate,
  alternative ='less')

#Comparing two variances
  #F-distribution Example: Vegetation Data
veg=read.csv(here("data","vegdata.csv"))
head(veg)

boxplot(pine~treatment,data = veg)

  #F-test to compare two variances 
var.test(
  pine~treatment,
  data=veg,
  subset = treatment %in% c('control','clipped'))

  #F-tests assume normality
shapiro.test(veg$pine[veg$treatment=="control"])
shapiro.test(veg$pine[veg$treatment=="clipped"])

  #Non-parametric Variance Test(non-normal)
fligner.test(
  pine~treatment,
  data = veg,
  subset = treatment %in% c('control','clipped'))

  #ksample parametric test: Bartlett’s test, 
  #tests for homogeneity of variances among more than two groups   
bartlett.test(pine~treatment,data=veg)  

  #Fligner test can test more than two variances as well
fligner.test(pine~treatment,data=veg)

#Comparing two sample means
  #T-test
t.test(
  pine ~ treatment,
  data = veg,
  subset = treatment %in% c('control','clipped'))

  #Wilcox test
wilcox.test(
  pine ~ treatment,
  data = veg,
  subset = treatment %in% c('control','clipped'))

#Tests for paired samples
control = veg$pine[veg$treatment=='control']
clipped = veg$pine[veg$treatment=='clipped']

t.test(control,clipped, paired=TRUE)

wilcox.test(control,clipped,paired = TRUE)

#Correlation
disp = read.csv(here("data", "dispersal.csv"))
disp

plot(disp$disp.rate.ftb, disp$disp.rate.eb)

cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use='complete.obs')

  #Kendall or Spearman tests may be used if the 
  #data do not necessarily come from a bivariate normal distribution.
cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use='complete.obs',
  method='spearman')

#Comparing two distributions
plot(
  ecdf(disp$disp.rate.ftb),
verticals = TRUE)
plot(
  ecdf(disp$disp.rate.eb),
  verticals = TRUE,
  lty= 3,
add=TRUE)

  #use the Kolmogorov-Smirnov test (ks.test) to determine
  #if they differ significantly in any aspect
ks.test(disp$disp.rate.ftb,disp$disp.rate.eb)

#Comparing two or more proportions
prop.test(c(4,16),c(40,250))

prop.test(c(8,32),c(80,500))

#Dependence of variables in a contingency table
  #Chi-squared test-expects cell values to be large, generally greater than 4 or 5. 
owls = matrix(c(16, 9, 4, 11), nrow=2)
rownames(owls) = c("present", "absent")
colnames(owls) = c("old", "young")
chisq.test(owls)

  #Fisher’s exact test- used when one or more of expected frequencies is less than 4 or 5.
fisher.test(owls)

#Bird habitat data
birds   = read.csv(here("data", "bird.sta.csv"))
hab     = read.csv(here("data", "hab.sta.csv"))
birdhab = merge(birds, hab, by=c("basin", "sub", "sta"))
  # Create a contingency table for edge/interior and brown creeper presence/absence
table(birdhab$s.edge, birdhab$BRCR > 0)

  # set the presence to be in the first column
br_creeper_table = table(birdhab$s.edge, birdhab$BRCR > 0)[, 2:1]
br_creeper_table

  
  