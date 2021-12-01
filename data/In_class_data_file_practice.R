require(here)
dat_catrate= read.csv(here("data","catrate.csv"))
dat_delomys= read.csv(here("data","delomys.csv"))
dat_rope= read.csv(here("data","rope.csv"))
head(dat_catrate)
head(dat_delomys)
head(dat_rope)
hist(dat_delomys$body_mass, main= "Delomy Body Mass- Hannah", xlab= "Body Mass", ylab= "Frequency")
dat_delomys
