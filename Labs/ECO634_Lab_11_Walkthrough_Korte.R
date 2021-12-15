#Data
require(here)
birds= read.csv(here("data","bird.sub.csv"))
hab= read.csv(here("data","hab.sub.csv"))
birdhab = merge(birds, hab, by = c("basin", "sub"))
dim(birdhab)

#Simulating Static Environmental Processes: Linear Regression
  #Graphical Exploration
plot(
  BRCR ~ ls, data= birdhab,ylab= "Brown Creeper abundance",
  xlab= "late-successional forest extent"
)

  #Fit a model
fit_1=lm(BRCR ~ ls, data= birdhab)
coef(fit_1)
slope_observed=coef(fit_1)[2]

plot(
  BRCR ~ ls, data= birdhab,
  ylab= "Brown Creeper abundance",
  xlab= "late-successional forest extent")
abline(fit_1)

#Deterministic Model: Linear Function
linear= function(x, y_int, slope)
{y_int + (slope * x)}
linear(x = 1, y_int = 1, slope = 1)
linear(x = 3:5, y_int = 1, slope = 1)
linear(x = 3:5, y_int = -1, slope = 1)
linear(x = 3:5, y_int = -1, slope = 0.01)

#Simulation Function
linear_simulator= function(x, y_int, slope, st_dev)
{y_int+ (slope*x)+rnorm(length(x),sd=st_dev)} 

#Parameters values 1
n = 200

par(mfrow = c(2, 2))
for (i in 1:4)
{
  x = runif(n = n)
  plot(
    x,
    linear_simulator(x, y_int = 1, slope = 4.5, st_dev = 0.1),
    main = "", xlab = "x", ylab = "y",
    pch = 16, col = rgb(0, 0.2, 0, 0.2))
}
dev.off()
#Parameters values 2
n = 400

par(mfrow = c(2, 2))
for (i in 1:4)
{
  x = runif(n = n)
  plot(
    x, linear_simulator(x, y_int = 10, slope = -6.5, st_dev = 1.1),
    main = "", xlab = "x", ylab = "y",
    pch = 16, col = rgb(0, 0.2, 0, 0.2))
}

#Build the simulation
  #Retrieve the model coefficients
fit_1_coefs = coefficients(fit_1)
str(fit_1_coefs)

fit_1_summary = summary(fit_1)
str(fit_1_summary)
fit_1_summary$sigma

int_obs= fit_1_summary$coefficients[1,1]
slope_obs= fit_1_summary$coefficient[2,1]
sd_obs= fit_1_summary$sigma

#Simulate Data
plot(
  x = birdhab$ls, 
  y = linear_simulator(
    x = birdhab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  ),
  main = "Simulated Data",
  xlab = "late-successional forest",
  ylab = "Brown Creeper Abundance")

  #plot the observed data first, then add the simulated data to the existing plot
plot(
  birdhab$ls, birdhab$BRCR, 
  xlab = "late-successional forest extent",
  ylab = "Brown Creeper abundance",
  pch = 19)

points(
  x = birdhab$ls, 
  y = linear_simulator(
    x = birdhab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  ),
  col = adjustcolor("red", alpha = 0.3),
  pch = 16)

legend(
  "topleft",
  legend = c("data", "simulation"),
  pch = 16,
  col = c(1, adjustcolor("red", alpha = 0.3)))

#Single Simulation
y_sim = linear_simulator(
  x = birdhab$ls,
  y_int = int_obs,
  slope = slope_obs,
  st_dev = sd_obs
)

fit_sim = lm(y_sim ~ birdhab$ls)
summary(fit_sim)

sum_1 = summary(fit_sim)
sum_1$coefficients
sum_1$coefficients[2,4]

#Repeated Simulations
n_sims = 1000
p_vals = numeric(n_sims)
alpha = 0.05
for(i in 1:n_sims)
{
  y_sim = linear_simulator(
    x = birdhab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  )
  fit_sim = lm(y_sim ~ birdhab$ls)
  
  p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
}
sum(p_vals < alpha) / n_sims

linear_sim_fit = function(x, slope, y_int, st_dev)
{
  y_sim = linear_simulator(
    x = x,
    y_int = y_int,
    slope = slope,
    st_dev = st_dev
  )
  fit_sim = lm(y_sim ~ x)
  return(fit_sim)
}

  #Simulating Effect Sizes
alpha = 0.05
n_sims = 1000
p_vals = numeric(n_sims)

n_effect_sizes = 20
effect_sizes_1 = seq(-.01, .01, length.out = n_effect_sizes)

effect_size_powers = numeric(n_effect_sizes)

for(j in 1:n_effect_sizes)
{
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x = birdhab$ls,
      y_int = int_obs,
      slope = effect_sizes_1[j],
      st_dev = sd_obs
    )
    
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  effect_size_powers[j] = sum(p_vals < alpha) / n_sims
}

sim_effect_size = 
  data.frame(
    power       = effect_size_powers,
    effect_size = effect_sizes_1)

plot(
  power ~ effect_size, data = sim_effect_size,
  type = 'l', xlab = 'Effect size', ylab = 'Power')
abline(v = slope_obs, lty = 2, col = 'red')

  #Simulating Sample Sizes
alpha = 0.05
n_sims = 1000
p_vals = numeric(n_sims)

sample_sizes = seq(5, 100)
sample_size_powers = numeric(length(sample_sizes))

for(j in 1:length(sample_sizes))
{
  x_vals = seq(0, 100, length.out = sample_sizes[j])
  
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x = x_vals,
      y_int = int_obs,
      slope = slope_obs,
      st_dev = sd_obs
    )
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  sample_size_powers[j] = sum(p_vals < alpha) / n_sims
}

sim_sample_size = 
  data.frame(
    power       = sample_size_powers,
    sample_size = sample_sizes)

plot(
  power ~ sample_size, data = sim_sample_size,
  type = 'l', xlab = 'Sample size', ylab = 'Power')
abline(v = nrow(birdhab), lty = 2, col = 'red')

#Bivariate Power Analysis
alpha = 0.01
n_sims = 50

p_vals = numeric(n_sims)

n_effect_sizes = 20
effect_sizes = seq(-.01, .01, length.out = n_effect_sizes)

sample_sizes = seq(10, 50)

sim_output_2 = matrix(nrow = length(effect_sizes), ncol = length(sample_sizes))

for(k in 1:length(effect_sizes))
{
  effect_size = effect_sizes[k]
  for(j in 1:length(sample_sizes))
  {
    x_vals = seq(0, 100, length.out = sample_sizes[j])
    
    for(i in 1:n_sims)
    {
      fit_sim = linear_sim_fit(
        x = x_vals,
        y_int = int_obs,
        slope = effect_size,
        st_dev = sd_obs
      )
      p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
    }
    sim_output_2[k, j] = sum(p_vals < alpha) / n_sims
  }
  print(paste0("computing effect size ", k," of ", length(effect_sizes)))
}

sim_n_effect_size = 
  list(
    power = sim_output_2,
    effect_size = effect_sizes,
    sample_size = sample_sizes
  )
  #Creating a image
image(
  sim_n_effect_size$power,
  xlab = "Effect size",
  ylab = "Sample Size",
  axes = FALSE)

# add x-axis labels
axis(
  1, 
  at = c(0, 0.5, 1), 
  labels = c(-.01, 0.0, .01))

# add y=axis labels
axis(
  2, 
  at = c(0, 1), 
  labels = c(sample_sizes[1], tail(sample_sizes, 1)))

#Plotting 3-Dimensional Data

  #Contour plotting
contour(
  x = sim_n_effect_size$effect_size,
  y = sim_n_effect_size$sample_size,
  z = sim_n_effect_size$power,
  xlab = "effect size",
  ylab = "sample size",
  main = "Contour Plot of Statistical Power",
  levels = seq(0, 1, length.out = 9),
  drawlabels = TRUE,
  # method = "simple")
  method = "edge")

  #Perspective Plots
    #Static Plot
persp(
  x = sim_n_effect_size$effect_size,
  y = sim_n_effect_size$sample_size,
  z = sim_n_effect_size$power,
  xlab = "beta", ylab = "n", zlab = "power",
  col = 'lightblue',
  theta = 30, phi = 30, expand = .75,
  ticktype = 'detailed')

    #Interactive Plot
install.packages("rgl")
install.packages("crosstalk")
install.packages("shiny")
install.packages("manipulateWidget")
require(rgl)
persp3d(x = sim_n_effect_size$effect_size,
        y = sim_n_effect_size$sample_size,
        z = sim_n_effect_size$power,
        xlab = "beta", ylab = "n", zlab = "power",
        col = 'lightblue',
        theta = 30, phi = 30, expand = .75,
        ticktype = 'detailed')
    
     #Saving an interactive plot
require(htmlwidgets)
saveWidget(
  rglwidget(),
  file = here(
    "docs",
    "n_effect_size_power_sim_plot.html"),
  selfcontained = TRUE
)

#Saving R Data Objects
save(
  sim_n_effect_size,
  file = here::here("data", "lab_11_n_effect_sizes.Rdata"))
save(
  sim_n_effect_size,
  file = here::here("data", "lab_11_n_effect_sizes.Rdata"))

#Population dispersion analysis
alpha = 0.05
n_sims = 100
p_vals = numeric(n_sims)

require(here)

habitat = read.csv(here("data", "hab.sub.csv"))
head(habitat)

birds = read.csv(here("data", "bird.sub.csv"))
head(birds)

birdhab = merge(
  birds, 
  habitat,
  by = c("basin", "sub"))
dim(birdhab)

fit_1 = lm(BRCR ~ ls, data = birdhab)

linear_simulator = function(x, y_int, slope, st_dev)
{
  y_int + (x * slope) + rnorm(length(x),  sd = st_dev)
}

fit_1_coefs = coefficients(fit_1)
str(fit_1_coefs)

fit_1_summary = summary(fit_1)
str(fit_1_summary)
sd_obs = fit_1_summary$sigma
int_obs = fit_1_summary$coefficients[1,1]
slope_obs = fit_1_summary$coefficients[2,1]; slope_obs

linear_sim_fit = function(x, slope, y_int, st_dev)
{
  y_sim = linear_simulator(
    x = x,
    y_int = y_int,
    slope = slope,
    st_dev = st_dev
  )
  fit_sim = lm(y_sim ~ x)
  return(fit_sim)
}

  #What was the observed standard deviation?
sd_obs

  #Specify the number of different standard deviation values to simulate:
n_sds = 20
pop_sds = seq(from = 0.01, to = 1.5, length.out = n_sds)

pop_sd_power = numeric(length(n_sds))

for(j in 1:length(pop_sds))
{
  pop_sd_j = pop_sds[j]
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x = birdhab$ls,
      y_int = int_obs,
      slope = slope_obs,
      st_dev = pop_sds[j])
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  pop_sd_power[j] = sum(p_vals < alpha) / n_sims
}

sim_output_dispersion = 
  data.frame(
    sd = pop_sds,
    power = pop_sd_power)

  #You should save your simulation results so you don't have to run it every time.
save(
  sim_output_dispersion, 
  file = here("data", "lab_ll_dat_dispersion_sim.RData"))

  #Line plot of standard deviation (x-axis) and statistical power (y-axis)
plot(
  power ~ sd, data = sim_output_dispersion,
  type = "l",
  xlab = "standard deviation", 
  ylab = "stastical power")


fit_x = lm(power ~ sd, data = sim_output_dispersion)

  #Add a dotted vertical red line at the observed population standard deviation value.
abline(v = sd_obs, lty = 2, col = 'red')

#Population dispersion and sample size analysis
alpha = 0.05

  #Start with a small number
n_sims = 100
p_vals = numeric(n_sims)

  #What was the observed standard deviation?

sd_obs

  #specify the number of different standard deviation values to simulate:
    #Start with a small number
n_sds = 20
pop_sds = seq(from = 0.05, to = 1.5, length.out = n_sds); pop_sds

pop_sd_power = numeric(length(n_sds))

sample_sizes = seq(5, 100)

sim_output_3 = matrix(nrow = length(pop_sds), ncol = length(sample_sizes))

for(k in 1:length(pop_sds))
{
  pop_sd_k = pop_sds[k]
  
  for(j in 1:length(sample_sizes))
  {
    x_vals = seq(0, 100, length.out = sample_sizes[j])
    
    for(i in 1:n_sims)
    {
      fit_sim = linear_sim_fit(
        x = x_vals,
        y_int = int_obs,
        slope = slope_obs,
        st_dev = pop_sd_k
      )
      p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
    }
    
    sim_output_3[k, j] = sum(p_vals < alpha) / n_sims
  }
  print(paste0("Testing standard deviation ", k, " of ", n_sds))
}

image(sim_output_3)

sim_3_dat = 
  list(
    power = sim_output_3,
    sample_size = sample_sizes,
    pop_sd = pop_sds)


  #You should save your simulation results so you don't have to run it every time.
save(
  sim_3_dat, 
  file = here::here("data", "lab_ll_sim_output_dispersion_n_1000.RData"))












