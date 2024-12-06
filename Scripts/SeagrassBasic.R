#Basic Logistic Growth Model


#Load necessary libraries
install.packages(c("tidyverse", "readr"))
library(tidyverse)
library(readr)

#Parameters for each scenario, including Control
parameters <- list(
  "Control" = list(delta_temp = 0, delta_pH = 0, delta_SLR = 0, delta_storms = 0, Kb = 63, Kd = 83),
  "RCP2.6" = list(delta_temp = 1, delta_pH = 0.07, delta_SLR = 0.29, delta_storms = 0, Kb = 60, Kd = 80),
  "RCP4.5" = list(delta_temp = 1.5, delta_pH = 0.14, delta_SLR = 0.36, delta_storms = 0, Kb = 50, Kd = 70),
  "RCP8.5" = list(delta_temp = 3.5, delta_pH = 0.31, delta_SLR = 0.53, delta_storms = 2, Kb = 35, Kd = 55)
)

#Constants
r0 <- 0.095  #Baseline growth rate
#Placeholder values
alpha <- 0.02; beta <- 0.01; gamma <- 0.03; delta <- 0.05 #Stressor impacts
B0 <- 63  #Initial biomass (g DW/m²)
D0 <- 83  #Initial shoot density (shoots/m²)
time_steps <- 730  #Number of time steps (days - this is reflective of the length of the experiment)

#Data storage
results <- data.frame()

#Simulation loop for each scenario
for (scenario in names(parameters)) {
  params <- parameters[[scenario]]
  r <- r0 * (1 - alpha * params$delta_temp) * 
    (1 - beta * params$delta_pH) * 
    (1 - gamma * params$delta_SLR) * 
    (1 - delta * params$delta_storms)
  
#Scenario-specific carrying capacities
  Kb <- params$Kb
  Kd <- params$Kd
  
  biomass <- numeric(time_steps); density <- numeric(time_steps)
  biomass[1] <- B0; density[1] <- D0
  
  for (t in 2:time_steps) {
    biomass[t] <- biomass[t-1] + r * biomass[t-1] * (1 - biomass[t-1] / Kb)
    density[t] <- density[t-1] + r * density[t-1] * (1 - density[t-1] / Kd)
  }
  
  results <- rbind(results, data.frame(
    Scenario = scenario,
    Time = 1:time_steps,
    Biomass = biomass,
    Density = density
  ))
}

#Convert data to long format for ggplot
results_long <- results %>%
  pivot_longer(cols = c("Biomass", "Density"), names_to = "Variable", values_to = "Value")

#Custom labels for facets
custom_labels <- c(
  Biomass = "Biomass (g DW/m²)",
  Density = "Shoot Density (shoots/m²)"
)

#Plot using ggplot2
ggplot(results_long, aes(x = Time / 365, y = Value, color = Scenario)) +  # Time in years
  geom_line(size = 1) +
  facet_wrap(~Variable, scales = "free_y", labeller = as_labeller(custom_labels)) +
  labs(title = "",
       x = "Time (years)", y = NULL, color = "Scenario") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

