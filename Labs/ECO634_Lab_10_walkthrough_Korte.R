#Building an ANOVA by Hand
rm(list = ls())
require(here)

rope = read.csv(here("data","rope.csv"))
rope$rope.type = factor(rope$rope.type)
levels(rope$rope.type)

n_obs = nrow(rope)
n_groups = length(levels(rope$rope.type))

ss_tot = sum((rope$p.cut - mean(rope$p.cut))^2)
df_tot = (n_obs-1)

agg_resids = aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) mean(x))

agg_sq_resids = aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) sum((x - mean(x))^2))
str(agg_sq_resids)

ss_within = sum(agg_sq_resids$x)
df_within = (n_obs-n_groups)

ss_among = (ss_tot - ss_within)
df_among = n_groups-1

ms_within = ss_within / (n_obs - n_groups)
ms_among  = ss_among / (n_groups - 1)

f_ratio = ms_among/ ms_within
f_pval = 1- pf(f_ratio, df_among, df_within)

#ANOVA in R
fit_1 = lm(p.cut ~ rope.type, data=rope)
anova(fit_1)
anova_fit_1 = anova(fit_1)
str(anova_fit_1)
anova_fit_1$"Sum Sq"




