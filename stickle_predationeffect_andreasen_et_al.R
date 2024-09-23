## Sticklebacks preying upon M. leidyi larvae
## Larvae are produced overnight only (8 PM to 8 AM)
## Larvae production = 6571 added every night at 1 AM
## Predation is based on functional responses 
## Starting concentration is 12 900 prey per m3
## Predator numbers are based on low, mid and high estimates
## Feeding during the day decreases due to stomach fullness (Beukema 1986)
## Percentage increase in prey pop is measured from 8AM to 8PM the following day (36 hrs)

setwd("C:/Users/mhean/OneDrive - Danmarks Tekniske Universitet/Dokumenter/Manuscripts/m2_sticklebackpredation/m2_data/") 

library(ggplot2)

# Temporal prey dynamics model #############

# Parameters
total_prey_pool <- 12900  # Initial total prey
prey_addition <- 6571     # Fixed number of prey added at 1 AM every night

feeding_start <- 8        # 8 AM
feeding_end <- 20         # 8 PM
total_hours <- 48         # Simulating for 48 hours

predator_densities <- c(0.1, 2.5, 45)

params <- list(
  normoxia = list(a = 0.147465, h = 0.0001015652),
  hypoxia  = list(a = 0.07696762, h = 0.007773594)
)

predation_rate_modifier <- function(t) {
  hour_in_day <- t %% 24
  if (hour_in_day == 0) hour_in_day <- 24  
  ifelse(hour_in_day >= feeding_start & hour_in_day < feeding_end, 1, 0)  # Feeding only during the day
}

# Function to simulate predation with prey added at 1 AM every night (except the first night)
simulate_predation <- function(predator_density, total_hours, total_prey_pool, prey_addition, a, h) {
  times <- 1:total_hours
  prey_over_time <- numeric(total_hours)
  
  for (t in times) {
    hour_in_day <- t %% 24
    if (hour_in_day == 0) hour_in_day <- 24  
    
    if (hour_in_day == 1 && t > 24) {
      total_prey_pool <- total_prey_pool + prey_addition
    }

    modifier <- predation_rate_modifier(t)
    prey_density_per_12_5L <- (total_prey_pool / 1000) * 12.5
    
    if (modifier > 0 && total_prey_pool > 0) {
      predation_rate <- (a * predator_density * prey_density_per_12_5L) / (1 + h * prey_density_per_12_5L)
      total_prey_pool <- total_prey_pool - predation_rate  # Subtract the prey removed from the pool
      if (total_prey_pool < 0) total_prey_pool <- 0       # Ensure prey pool doesn't go negative
    }
    
    prey_over_time[t] <- total_prey_pool  # Track prey pool over time
  }
  
  return(data.frame(Time = times, Prey = prey_over_time, Predator_Density = predator_density))
}

# Simulate for each condition and predator density
results <- data.frame()
for (density in predator_densities) {
  for (condition in names(params)) {
    sim_data <- simulate_predation(
      density,
      total_hours,
      total_prey_pool,
      prey_addition,
      params[[condition]]$a,
      params[[condition]]$h
    )
    sim_data$Condition <- condition
    sim_data$Predator_Density <- factor(density, levels = predator_densities)
    results <- rbind(results, sim_data)
  }
}

library(scales)

ggplot(results, aes(x = Time, y = Prey, color = Condition, linetype = as.factor(Predator_Density))) + 
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = c("normoxia" = "cornflowerblue", "hypoxia" = "red4"),
                     labels = c("Normoxia", "Hypoxia"),
                     breaks = c("normoxia", "hypoxia")) +
  scale_linetype_manual(values = c("0.1" = "dotted", "2.5" = "dashed", "45" = "solid")) +
  labs(y = expression(paste("Prey density (", "larvae"~m^{-3}, ")", sep = "")), 
       x = "Time (hours)", 
       color = "", 
       linetype = expression(paste("Predator density (", m^{-3}, ")", sep = ""))) +
  scale_y_continuous(labels = scales::label_number(big.mark = " ")) +
  scale_x_continuous(breaks = c(7,19,31,43),
                     labels = c("8 AM Day 1", "8 PM", "8 AM Day 2", "8 PM")) + 
  theme3 +
  theme(legend.position = "")

ggsave("eco_pred_new_legend.png",
       device="png",
       width = 12,
       height = 10,
       dpi = 1200,
       units = "cm")

ggsave("eco_pred_new.png",
       device="png",
       width = 12,
       height = 10,
       dpi = 1200,
       units = "cm")

# Percent removed ##########################

# Parameters
# Parameters
initial_prey_pool <- 12900  
prey_addition <- 6571       

feeding_start <- 8          
feeding_end <- 20         
total_hours <- 48           

params <- list(
  normoxia = list(a = 0.147465, h = 0.0001015652),
  hypoxia  = list(a = 0.07696762, h = 0.007773594)
)

predation_rate_modifier <- function(t) {
  hour_in_day <- ((t - 1) %% 24) + 1 
  ifelse(hour_in_day >= feeding_start & hour_in_day < feeding_end, 1, 0)
}

simulate_predation <- function(predator_density, total_hours, total_prey_pool, prey_addition, a, h) {
  times <- 1:total_hours
  prey_over_time <- numeric(total_hours)
  
  for (t in times) {
    hour_in_day <- ((t - 1) %% 24) + 1
    
    if (hour_in_day == 1 && t > 24) {
      total_prey_pool <- total_prey_pool + prey_addition
    }
    
    modifier <- predation_rate_modifier(t)
    
    prey_density_per_12_5L <- (total_prey_pool / 1000) * 12.5
    
    if (modifier > 0 && total_prey_pool > 0) {
      predation_rate <- (a * predator_density * prey_density_per_12_5L) /
        (1 + h * prey_density_per_12_5L)
      total_prey_pool <- total_prey_pool - predation_rate
      if (total_prey_pool < 0) total_prey_pool <- 0
    }
    
    prey_over_time[t] <- total_prey_pool  
  }
  
  return(prey_over_time)  
}

results <- data.frame()

for (condition in names(params)) {
  a <- params[[condition]]$a
  h <- params[[condition]]$h
  
  for (predator_density in seq(0, 300, by = 1)) {
    # Run the simulation for each predator density
    prey_over_time <- simulate_predation(
      predator_density,
      total_hours,
      initial_prey_pool,
      prey_addition,
      a,
      h
    )
    
    prey_at_8am_day1 <- prey_over_time[8]
    
    prey_at_8pm_day2 <- prey_over_time[44]
    
    percent_increase <- ((prey_at_8pm_day2 - prey_at_8am_day1) / prey_at_8am_day1) * 100
    
    results <- rbind(
      results,
      data.frame(
        Predator_Density = predator_density,
        Condition = condition,
        Percent_Increase = percent_increase
      )
    )
  }
}

results$Condition <- factor(results$Condition, levels = c("normoxia", "hypoxia"))

ggplot(results, aes(x = Predator_Density, y = Percent_Increase, color = Condition)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", size = 0.8) +
  scale_color_manual(values = c("normoxia" = "cornflowerblue", "hypoxia" = "red4"),
                     labels = c("Normoxia", "Hypoxia")) +
  geom_segment(aes(x = 12.09, xend = 12.09, y = -Inf, yend = 0), 
               color = "cornflowerblue", linetype = "dotted", size = 0.8) +
  geom_segment(aes(x = 52.04, xend = 52.04, y = -Inf, yend = 0), 
               color = "red4", linetype = "dotted", size = 0.8) +
  scale_x_continuous(breaks = c(0,12, 52,100,200,300),  # Add ticks at 12 and 52
                     labels = c(0,12, 52,100,200,300)) +  # Ensure labels appear as well
  labs(
    x = bquote('Predator density '('sticklebacks'~m^-3)),
    y = "Prey population 36 h development (%)",
    color = "") +
  theme3 +
  theme(legend.position = "none",
        axis.title = element_text(size = 12, color = "black"),
        axis.text = element_text(size = 12, color = "black"))


# 12 needed for hypoxia
# 52 needed for normoxia

ggsave("eco_perc_new.png",
       device="png",
       width = 12.85,
       height = 10,
       dpi = 1200,
       units = "cm")

############################################################
############################################################

find_zero_increase_density <- function(data) {
  data <- data[order(data$Predator_Density), ]
  
  data$Sign <- sign(data$Percent_Increase)
  crossing_points <- which(diff(data$Sign) != 0)
  
  zero_crossings <- numeric(length(crossing_points))
  
  for (i in seq_along(crossing_points)) {
    idx1 <- crossing_points[i]
    idx2 <- idx1 + 1
    
    x1 <- data$Predator_Density[idx1]
    x2 <- data$Predator_Density[idx2]
    y1 <- data$Percent_Increase[idx1]
    y2 <- data$Percent_Increase[idx2]
    
    predator_density_zero <- x1 - y1 * (x2 - x1) / (y2 - y1)
    zero_crossings[i] <- predator_density_zero
  }
  return(min(zero_crossings))
}

normoxia_results <- subset(results, Condition == "normoxia")
hypoxia_results <- subset(results, Condition == "hypoxia")

normoxia_zero_density <- find_zero_increase_density(normoxia_results)
hypoxia_zero_density <- find_zero_increase_density(hypoxia_results)

cat("Predator density where prey population no longer increases:\n")
cat(sprintf("Normoxia: %.2f predators\n", normoxia_zero_density))
cat(sprintf("Hypoxia: %.2f predators\n", hypoxia_zero_density))


