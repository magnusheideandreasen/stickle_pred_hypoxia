#### Functional response models for manuscript on stickleback predation

## To do:

### double check data origin and correction for fish size

library(gridExtra)
library(grid)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(cowplot)
library(magick)
library(png)
library(devtools)
library(scales)
library(frair)
library(patchwork)
library(installr)

setwd("C:/Users/mhean/OneDrive - Danmarks Tekniske Universitet/Dokumenter/Manuscripts/m2_sticklebackpredation/m2_data/FR/")

# files =================================
d <- read.csv("fr_2024_4.csv") # feeding rates from cylindrical tanks

d30 <- d[d$condition=="hypoxia",]
d100 <- d[d$condition=="normoxia",]

# mnemiopsis #################
table(subset(d, species=="mnemiopsis")$duration_min) #55 obs at 90 min

modeT <-  with(subset(d, species=="mnemiopsis"), as.numeric(names(which.max(table(duration_min)))))

ggplot(data=subset(d, species=="mnemiopsis" & duration_min==modeT), aes(prey_no_total, prey_remov, colour=condition))+
	geom_point()+geom_smooth()


### fitting  ------------
ml100_fit <- frair_fit(formula = prey_remov ~ prey_no_total, T=duration_min,
                       data = d100[d100$species=="mnemiopsis",],
                       response = "rogersII", # Hollings II with prey depletion
                       start = list(a = 0.0020, h = 0.05))

ml30_fit <- frair_fit(formula = prey_remov ~ prey_no_total, T=duration_min,
													data = d30[d30$species=="mnemiopsis",],
													response = "rogersII", # Hollings II with prey depletion
													start = list(a = 0.0020, h = 0.05))

ml100_fit$coefficients
ml30_fit$coefficients
													
##########################################################
### bootstrapping  ---------------
boot_ml100_fit <- frair_boot(ml100_fit, nboot=2000)
boot_ml30_fit <- frair_boot(ml30_fit, nboot=2000) 

ml100_a_boots <- boot_ml100_fit$bootcoefs[,1] 
ml100_h_boots <- boot_ml100_fit$bootcoefs[,2] 

ml30_a_boots <- boot_ml30_fit$bootcoefs[,1] 
ml30_h_boots <- boot_ml30_fit$bootcoefs[,2] 


quantile(ml100_a_boots/ml100_h_boots, c(0.025, 0.975)) # FRR
quantile(1/ml100_h_boots, c(0.025, 0.975)) # maxfeed
quantile(ml30_a_boots/ml30_h_boots, c(0.025, 0.975))
quantile(1/ml30_h_boots, c(0.025, 0.975))


confint(boot_ml100_fit, citypes = "bca")
confint(boot_ml30_fit, citypes = "bca")

### comparing hypoxia and normoxia ###############
quantile(ml30_a_boots- ml100_a_boots, c(0.025, 0.975)) 
quantile(ml30_h_boots- ml100_h_boots, c(0.025, 0.975))

frair_compare(ml30_fit, ml100_fit)


### plotting predictions -----------
# modified from Bolker on stackoverflow

pframe0_ml30 <- with(d30[d30$species=="mnemiopsis",],
								data.frame(prey_no_total = seq(0, max(prey_no_total), length = 101), duration_min = 1))
pframe0_ml100 <- with(d100[d100$species=="mnemiopsis",],
										data.frame(prey_no_total = seq(0, max(prey_no_total), length = 101), duration_min = 1))

## basic (predicted value only)
pframe_ml30 <- data.frame(pframe0_ml30, prey_remov= predict(ml30_fit, newdata=pframe0_ml30))
pframe_ml100 <- data.frame(pframe0_ml100, prey_remov= predict(ml100_fit, newdata=pframe0_ml100))

## with confidence intervals
pframe_b_ml30 <- data.frame(pframe0_ml30, predict(ml30_fit, newdata=pframe_ml30, boot = boot_ml30_fit))
pframe_b_ml100 <- data.frame(pframe0_ml100, predict(ml100_fit, newdata=pframe_ml100, boot = boot_ml100_fit))

## combine norm- and hypoxia data
pframe_b_ml30$condition <- "hypoxia"
pframe_b_ml100$condition <- "normoxia"

pframe_b_ml <- rbind(pframe_b_ml30, pframe_b_ml100)
pframe_b_ml$species <- "mnemiopsis"
#### plot 

# Define the specific scaled x-axis values you want to display
custom_breaks <- c(2, 8, 40, 70, 96) * 12.5 
custom_labels <- c(2, 8, 40, 70, 96)  

ggplot(subset(d, species == "mnemiopsis" & duration_min == 1), 
			 aes(prey_no_total, prey_remov, colour = condition, fill = condition)) +
	geom_point(alpha=0.4, shape=16) +
	geom_line(data = pframe_b_ml) +
	geom_ribbon(data = pframe_b_ml, aes(ymin = lwr, ymax = upr), 
							alpha = 0.2, color = NA) +  
	scale_color_manual(values = c("normoxia" = "cornflowerblue", "hypoxia" = "red4"), 
										 labels = c("Normoxia", "Hypoxia")) +
	scale_fill_manual(values = c("normoxia" = "cornflowerblue", "hypoxia" = "red4"), 
										labels = c("Normoxia", "Hypoxia")) +
	scale_x_continuous(breaks = custom_breaks, labels = custom_labels) +  
	scale_y_continuous(breaks = scales::pretty_breaks(),
										 sec.axis = sec_axis(
										 	trans = ~ .*0.2959,
										 	breaks = c(0,0.3,0.6,0.9),
										 	name = bquote('Ingestion rate  '(ingested ~mu*gC~ ~ind^-1 ~min^-1))))+
	labs(x = expression(paste("Initial number of prey", ~L^"-1", sep = "")),
			 y = bquote('Ingestion rate '("ingested prey" ~ind^-1 ~min^-1)),
			 colour = "Oxygen level",   
			 fill = "Oxygen level") +
	theme_minimal(base_family = "Arial", base_size = 10) + 
	theme(
		plot.background = element_rect(fill = "white", color = "black", size = 0.8),  
		panel.background = element_rect(fill = "white", color = "black", size = 0.8),
		panel.grid = element_blank(),  
		axis.title = element_text(size = 10, color = "black"), 
		axis.text = element_text(size = 10, color = "black"),   
		legend.title = element_text(size = 10, color = "black"), 
		legend.text = element_text(size = 10, color = "black"),   
		axis.ticks = element_line(color = "black", size = 0.5), 
		axis.ticks.length = unit(0.3, "cm"),
		legend.position = "none"
	)

ggsave("ml_fr2024_min.png",
			 device="png",
			 width = 12,
			 height = 10,
			 dpi = 1200,
			 units = "cm")

# acartia ##############
d$prey_no_total <- round(d$prey_no_total)
d100$prey_no_total <- round(d100$prey_no_total)
d30$prey_no_total <- round(d30$prey_no_total)

with(subset(d, species=="acartia"), table(duration_min, condition)) #27 obs at 30 minutes

modeT <-  with(subset(d, species=="acartia"), as.numeric(names(which.max(table(duration_min)))))

ggplot(data=subset(d, species=="acartia" & duration_min==modeT), aes(prey_no_total, prey_remov, colour=condition))+
	geom_point()+geom_smooth()


### fitting  ------------
ac100_fit <- frair_fit(formula = prey_remov ~ prey_no_total, T=duration_min,
											 data = d100[d100$species=="acartia",],
											 response = "rogersII", # Hollings II with prey depletion
											 start = list(a = 0.005, h = 0.2))

ac30_fit <- frair_fit(formula = prey_remov ~ prey_no_total, T=duration_min,
											data = d30[d30$species=="acartia",],
											response = "rogersII", # Hollings II with prey depletion
											start = list(a = 0.0020, h = 0.05))

ac100_fit$coefficients
ac30_fit$coefficients

### bootstrapping  ---------------
boot_ac100_fit <- frair_boot(ac100_fit, nboot=2000) 
boot_ac30_fit <- frair_boot(ac30_fit, nboot=2000)

ac100_a_boots <- boot_ac100_fit$bootcoefs[,1] 
ac100_h_boots <- boot_ac100_fit$bootcoefs[,2] 

ac30_a_boots <- boot_ac30_fit$bootcoefs[,1] 
ac30_h_boots <- boot_ac30_fit$bootcoefs[,2] 


quantile(ac100_a_boots/ac100_h_boots, c(0.025, 0.975))
quantile(1/ac100_h_boots, c(0.025, 0.975))
quantile(ac30_a_boots/ac30_h_boots, c(0.025, 0.975))
quantile(1/ac30_h_boots, c(0.025, 0.975))


confint(boot_ac100_fit, citypes = "bca")
confint(boot_ac30_fit, citypes = "bca")

### comparing hypoxia and normoxia ###############
quantile(ac30_a_boots- ac100_a_boots, c(0.025, 0.975))
quantile(ac30_h_boots- ac100_h_boots, c(0.025, 0.975)) 

frair_compare(ac30_fit, ac100_fit)


### plotting predictions -----------
# modified from Bolker on stackoverflow

pframe0_ac30 <- with(d30[d30$species=="acartia",],
										 data.frame(prey_no_total = seq(0, max(prey_no_total), length = 101), duration_min = 1))
pframe0_ac100 <- with(d100[d100$species=="acartia",],
											data.frame(prey_no_total = seq(0, max(prey_no_total), length = 101), duration_min = 1))

## basic (predicted value only)
pframe_ac30 <- data.frame(pframe0_ac30, prey_remov= predict(ac30_fit, newdata=pframe0_ac30))
pframe_ac100 <- data.frame(pframe0_ac100, prey_remov= predict(ac100_fit, newdata=pframe0_ac100))

## with confidence intervals
pframe_b_ac30 <- data.frame(pframe0_ac30, predict(ac30_fit, newdata=pframe_ac30, boot = boot_ac30_fit))
pframe_b_ac100 <- data.frame(pframe0_ac100, predict(ac100_fit, newdata=pframe_ac100, boot = boot_ac100_fit))

## combine norm- and hypoxia data
pframe_b_ac30$condition <- "hypoxia"
pframe_b_ac100$condition <- "normoxia"

pframe_b_ac1 <- rbind(pframe_b_ac30, pframe_b_ac100)
pframe_b_ac1$species <- "acartia"

#### plot 

# Define the specific scaled x-axis values you want to display
custom_breaks <- c(2, 8, 40, 70, 96, 120, 228) * 12.5  
custom_labels <- c(2, 8, 40, 70, 96, 120, 228)  

ggplot(subset(d, species == "acartia" & duration_min == 1), 
			 aes(prey_no_total, prey_remov, colour = condition, fill = condition)) +
	geom_point(alpha=0.4, shape=16) +
	geom_line(data = pframe_b_ac1, linetype = "dotted") +
	geom_ribbon(data = pframe_b_ac1, aes(ymin = lwr, ymax = upr), 
							alpha = 0.2, color = NA) +  
	scale_color_manual(values = c("normoxia" = "cornflowerblue", "hypoxia" = "red4"), 
										 labels = c("Normoxia", "Hypoxia")) +
	scale_fill_manual(values = c("normoxia" = "cornflowerblue", "hypoxia" = "red4"), 
										labels = c("Normoxia", "Hypoxia")) +
	scale_x_continuous(breaks = custom_breaks, labels = custom_labels) +  
	scale_y_continuous(breaks = scales::pretty_breaks(),
										 sec.axis = sec_axis(
										 	trans = ~ .*2.651,
										 	breaks = c(0,10,20,30,40),
										 	name = bquote('Ingestion rate  '(ingested ~mu*gC~ ~ind^-1 ~min^-1))))+
	labs(x = expression(paste("Initial number of prey", ~L^"-1", sep = "")),
			 y = bquote('Ingestion rate '("ingested prey" ~ind^-1 ~min^-1)),
			 colour = "Oxygen level",   
			 fill = "Oxygen level") +
	theme_minimal(base_family = "Arial", base_size = 10) +  
	theme(
		plot.background = element_rect(fill = "white", color = "black", size = 0.8), 
		panel.background = element_rect(fill = "white", color = "black", size = 0.8),
		panel.grid = element_blank(), 
		axis.title = element_text(size = 10, color = "black"), 
		axis.text = element_text(size = 10, color = "black"),
		legend.title = element_text(size = 10, color = "black"),
		legend.text = element_text(size = 10, color = "black"),  
		axis.ticks = element_line(color = "black", size = 0.5),  
		axis.ticks.length = unit(0.3, "cm"),
		legend.position = "none"
	)

ggsave("at_fr2024_min.png",
			 device="png",
			 width = 12,
			 height = 10,
			 dpi = 1200,
			 units = "cm")




# artemia ##############
with(subset(d, species=="artemia"), table(duration_min, condition))

modeT <-  with(subset(d, species=="artemia"), as.numeric(names(which.max(table(duration_min)))))
  
ggplot(data=subset(d, species=="artemia"), aes(prey_no_total, prey_remov/duration_min))+
	geom_point()

ggplot(data=subset(d, species=="artemia"), aes(prey_no_total, prey_remov/duration_min, colour=condition))+
	geom_point()+geom_smooth()

### fitting  ------------
as100_fit <- frair_fit(formula = prey_remov ~ prey_no_total, T=duration_min,
                       data = d100[d100$species=="artemia",],
                       response = "rogersII", # Hollings II with prey depletion
                       start = list(a = 0.005, h = 0.2))

as100_fit$coefficients

### bootstrapping  ---------------
boot_as100_fit<- frair_boot(as100_fit, nboot=2000)
as100_a_boots <- boot_as100_fit$bootcoefs[,1] 
as100_h_boots <- boot_as100_fit$bootcoefs[,2] 

quantile(as100_a_boots/as100_h_boots, c(0.025, 0.975))
quantile(1/as100_h_boots, c(0.025, 0.975))

confint(boot_as100_fit, citypes = "bca")

### plotting predictions -----------
# modified from Bolker on stackoverflow
pframe0_as100 <- with(d100[d100$species=="artemia",],
                      data.frame(prey_no_total = seq(0, max(prey_no_total), length = 101), duration_min = 1)) 
## basic (predicted value only)
pframe_as100 <- data.frame(pframe0_as100, prey_remov= predict(as100_fit, newdata=pframe0_as100))

## with confidence intervals
pframe_b_as100 <- data.frame(pframe0_as100, predict(as100_fit, newdata=pframe_as100, boot = boot_as100_fit))

## combine norm- and hypoxia data
pframe_b_as100$condition <- "normoxia"

pframe_b_as <- rbind(pframe_b_as100)
pframe_b_as$species <- "artemia"
# Define the specific scaled x-axis values you want to display
custom_breaks <- c(2, 8, 40, 70, 96, 192) * 12.5  
custom_labels <- c(2, 8, 40, 70, 96, 192)  

ggplot(subset(d, species == "artemia" & duration_min == 1), 
			 aes(prey_no_total, prey_remov, colour = condition, fill = condition)) +
	geom_line(data = pframe_b_as, linetype = "dashed") +
	geom_ribbon(data = pframe_b_as, aes(ymin = lwr, ymax = upr), 
							alpha = 0.2, color = NA) +  
	scale_color_manual(values = c("normoxia" = "cornflowerblue", "hypoxia" = "red4"), 
										 labels = c("Normoxia", "Hypoxia")) +
	scale_fill_manual(values = c("normoxia" = "cornflowerblue", "hypoxia" = "red4"), 
										labels = c("Normoxia", "Hypoxia")) +
	scale_x_continuous(breaks = custom_breaks, labels = custom_labels) +  
	scale_y_continuous(breaks = scales::pretty_breaks(),
										 sec.axis = sec_axis(
										 	trans = ~ .*0.7514,
										 	breaks = c(0,10,20,30, 40),
										 	name = bquote('Ingestion rate  '(ingested ~mu*gC~ ~ind^-1 ~min^-1))))+
	labs(x = expression(paste("Initial number of prey", ~L^"-1", sep = "")),
			 y = bquote('Ingestion rate '("ingested prey" ~ind^-1 ~min^-1)),
			 colour = "Oxygen level",   
			 fill = "Oxygen level") +
	theme_minimal(base_family = "Arial", base_size = 10) +  
	theme(
		plot.background = element_rect(fill = "white", color = "black", size = 0.8),  
		panel.background = element_rect(fill = "white", color = "black", size = 0.8), 
		panel.grid = element_blank(), 
		axis.title = element_text(size = 10, color = "black"),  
		axis.text = element_text(size = 10, color = "black"),  
		legend.title = element_text(size = 10, color = "black"), 
		legend.text = element_text(size = 10, color = "black"),    
		axis.ticks = element_line(color = "black", size = 0.5), 
		axis.ticks.length = unit(0.3, "cm"),
		legend.position = "none"
	)

ggsave("as_fr2024_min.png",
			 device="png",
			 width = 12,
			 height = 10,
			 dpi = 1200,
			 units = "cm")


# coefficients #####################

a_ml100 <- ml100_fit$coefficients[1] # 0.002457751 
h_ml100 <- ml100_fit$coefficients[2] # 0.006093913 

a_ml30 <- ml30_fit$coefficients[1] # 0.001282794
h_ml30 <- ml30_fit$coefficients[2] # 0.4664157 

a_ac100 <- ac100_fit$coefficients[1] # 0.01585419 
h_ac100 <- ac100_fit$coefficients[2] # 0.05168801 

a_ac30 <- ac30_fit$coefficients[1] # 0.004910787 
h_ac30 <- ac30_fit$coefficients[2] # 0.1553486 

a_as100 <- as100_fit$coefficients[1] # 0.1321918 
h_as100 <- as100_fit$coefficients[2] # 0.02239383 


# figures publication ##########################################################

## joint figures ################### 

joint_frame <- rbind(pframe_b_ml,pframe_b_ac1,pframe_b_as)

joint_frame$prey_remov <- joint_frame$prey_remov*60
joint_frame$upr <- joint_frame$upr*60
joint_frame$lwr <- joint_frame$lwr*60

# Define the specific scaled x-axis values you want to display
custom_breaks <- c(2, 8, 40, 70, 96, 120, 192, 228) * 12.5 
custom_labels <- c(2, 8, 40, 70, 96, 120, 192, 228) 

ggplot(subset(d, duration_min == 60), 
       aes(prey_no_total, prey_remov, colour = condition, fill = condition, linetype = species)) +
  geom_line(data = joint_frame) +
  geom_ribbon(data = joint_frame[joint_frame$species=="mnemiopsis",], aes(ymin = lwr, ymax = upr), 
              alpha = 0.2, color = NA) +
  geom_ribbon(data = joint_frame[joint_frame$species=="acartia",], aes(ymin = lwr, ymax = upr), 
              alpha = 0.2, color = NA) +
  geom_ribbon(data = joint_frame[joint_frame$species=="artemia",], aes(ymin = lwr, ymax = upr), 
              alpha = 0.2, color = NA) +
  scale_color_manual(values = c("normoxia" = "cornflowerblue", "hypoxia" = "red4"), 
                     labels = c("Normoxia", "Hypoxia")) +
  scale_fill_manual(values = c("normoxia" = "cornflowerblue", "hypoxia" = "red4"), 
                    labels = c("Normoxia", "Hypoxia")) +
  scale_linetype_manual(values = c("mnemiopsis" = "solid", "acartia" = "dotted", "artemia" = "dashed")) +  
  scale_x_continuous(breaks = custom_breaks, labels = custom_labels) +
  scale_y_log10(breaks = c(1, 10, 100, 1000, 2000)) + 
  labs(x = expression(paste("Initial number of prey", ~L^"-1", sep = "")),
       y = bquote('Ingestion rate '("ingested prey" ~ind^-1 ~h^-1)), 
       colour = "Oxygen level",   
       fill = "Oxygen level") +
  theme_minimal(base_family = "Arial", base_size = 10) +
  theme(
    plot.background = element_rect(fill = "white", color = "black", size = 0.8),
    panel.background = element_rect(fill = "white", color = "black", size = 0.8),
    panel.grid = element_blank(),
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 10, color = "black"),
    legend.title = element_text(size = 10, color = "black"),
    legend.text = element_text(size = 10, color = "black"),
    axis.ticks = element_line(color = "black", size = 0.5),
    axis.ticks.length = unit(0.3, "cm"),
    legend.position = "none"
  )


ggsave("joint_fr2024_min.png",
			 device="png",
			 width = 12,
			 height = 10,
			 dpi = 1200,
			 units = "cm")

## carbon ################## 
# Scale the prey_remov, lwr, and upr values based on species
joint_frame$prey_remov_scaled <- with(joint_frame, 
																			ifelse(species == "mnemiopsis", prey_remov * 0.2959,
																						 ifelse(species == "acartia", prey_remov * 2.651,
																						 			 ifelse(species == "artemia", prey_remov * 0.7514, prey_remov))))

joint_frame$lwr_scaled <- with(joint_frame, 
															 ifelse(species == "mnemiopsis", lwr * 0.2959,
															 			 ifelse(species == "acartia", lwr * 2.651,
															 			 			 ifelse(species == "artemia", lwr * 0.7514, lwr))))

joint_frame$upr_scaled <- with(joint_frame, 
															 ifelse(species == "mnemiopsis", upr * 0.2959,
															 			 ifelse(species == "acartia", upr * 2.651,
															 			 			 ifelse(species == "artemia", upr * 0.7514, upr))))

# Update the plot to use the scaled values
ggplot(subset(d, duration_min == 60), 
			 aes(prey_no_total, prey_remov_scaled, colour = condition, fill = condition, linetype = species)) +
	geom_line(data = joint_frame) +
	geom_ribbon(data = joint_frame[joint_frame$species=="mnemiopsis",], 
							aes(ymin = lwr_scaled, ymax = upr_scaled), 
							alpha = 0.2, color = NA) + 
	geom_ribbon(data = joint_frame[joint_frame$species=="acartia",], 
							aes(ymin = lwr_scaled, ymax = upr_scaled), 
							alpha = 0.2, color = NA) + 
	geom_ribbon(data = joint_frame[joint_frame$species=="artemia",], 
							aes(ymin = lwr_scaled, ymax = upr_scaled), 
							alpha = 0.2, color = NA) +  
	scale_color_manual(values = c("normoxia" = "cornflowerblue", "hypoxia" = "red4"), 
										 labels = c("Normoxia", "Hypoxia")) +
	scale_fill_manual(values = c("normoxia" = "cornflowerblue", "hypoxia" = "red4"), 
										labels = c("Normoxia", "Hypoxia")) +
	scale_linetype_manual(values = c("mnemiopsis" = "solid", "acartia" = "dotted", "artemia" = "dashed")) +  
	scale_x_continuous(breaks = custom_breaks, labels = custom_labels) +  
	scale_y_log10(breaks = c(1, 10, 100,1000), position = "left") +
	labs(x = expression(paste("Initial number of prey", ~L^"-1", sep = "")),
			 y = bquote('Ingestion rate  '(ingested ~mu*gC~ ~ind^-1 ~hr^-1)),
			 colour = "Oxygen level",   
			 fill = "Oxygen level") +
	theme_minimal(base_family = "Arial", base_size = 10) +  
	theme(
		plot.background = element_rect(fill = "white", color = "black", size = 0.8), 
		panel.background = element_rect(fill = "white", color = "black", size = 0.8),
		panel.grid = element_blank(),  
		axis.title = element_text(size = 10, color = "black"),
		axis.text = element_text(size = 10, color = "black"),  
		legend.title = element_text(size = 10, color = "black"), 
		legend.text = element_text(size = 10, color = "black"),   
		axis.ticks = element_line(color = "black", size = 0.5),
		axis.ticks.length = unit(0.3, "cm"),
		legend.position = "none" 
	)

ggsave("joint_fr2024_min_carb.png",
			 device="png",
			 width = 12,
			 height = 10,
			 dpi = 1200,
			 units = "cm")

# theme ##############
theme3 <- theme(
	plot.background = element_rect(fill = "white", color = "white", size = 0.8),
	panel.background = element_rect(fill = "white", color = "black", size = 0.8),
	panel.grid = element_blank(),
	axis.title = element_text(size = 10, color = "black"),
	axis.text = element_text(size = 10, color = "black"),
	legend.title = element_text(size = 10, color = "black"),
	legend.text = element_text(size = 10, color = "black"),
	plot.tag = element_text(face = "bold"),
	legend.key = element_rect(fill = "white", color = NA), 
	legend.background = element_rect(fill = "white", color = NA), 
	axis.ticks = element_line(color = "black", size = 0.5),
	axis.ticks.length = unit(0.3, "cm"),
	legend.position = "right"
)


# estimates ######################################

e <- read_csv("fr_estimates.csv") # feeding rates from cylindrical tanks

e$species <- factor(e$species, levels = c("mnemiopsis", "acartia", "artemia"))
e$oxy <- factor(e$oxy, levels = c("normoxia", "hypoxia"))
species_labels <- c("mnemiopsis" = expression(paste(italic("M. leidyi"))), "acartia" = expression(paste(italic("A. tonsa"))), "artemia" = expression(paste(italic("A. salina"))))

oxy_colors <- c("hypoxia" = "red4", "normoxia" = "cornflowerblue")

# Plotting the estimates with error bars

e <- e %>%
	mutate(a = a / 12.5,
				 a_lo = a_lo / 12.5,
				 a_up = a_up / 12.5)

# Plot with the converted values
a <- 
	ggplot(e, aes(x = species, y = a, color = oxy)) +
	geom_point(size = 3, position = position_dodge(width = 0.3)) +
	geom_linerange(aes(ymin = a - a_lo, ymax = a + a_up), 
								 position = position_dodge(width = 0.3)) +
	scale_y_log10(labels = scales::label_number(),
								oob = scales::squish) + 
	labs(x = "", y = bquote('a'~(L~min^-1)), 
			 color = "") +
	scale_color_manual(values = oxy_colors,
										 labels = c("Normoxia","Hypoxia")) +  
	theme3 +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	scale_x_discrete(labels = species_labels)

a

# handling time
e <- e %>%
	mutate(h = h * 60,
				 h_lo = h_lo * 60,
				 h_up = h_up * 60)

b <- 
ggplot(e, aes(x = species, y = h, color = oxy)) +
	geom_point(size = 3, position = position_dodge(width = 0.3)) +
	geom_linerange(aes(ymin = h - h_lo, ymax = h + h_up), 
								 position = position_dodge(width = 0.3)) +
	labs(x = "", y = "Handling time (sec)", color = "") +
	scale_y_log10()+
	scale_color_manual(values = oxy_colors,
										 labels = c("Normoxia","Hypoxia")) +  
	theme3 +
	theme(axis.text.x = element_text(angle = 45, hjust = 1),
				theme(plot.title = element_text(face = "bold"))) +
	scale_x_discrete(labels = species_labels) 
b

# maximum feeding rate (MFR) = 1/h # used only for 
c <- 
	ggplot(e, aes(x = species, y = maxfeed, color = oxy)) +
	geom_point(size = 3, position = position_dodge(width = 0.3)) +
	geom_linerange(aes(ymin = maxfeed - mf_lo, ymax = maxfeed + mf_up), 
								 position = position_dodge(width = 0.3)) +
	labs(x = "", y = bquote('Log maximum feeding rate '(hr^-1)), color = "") +
	scale_color_manual(values = oxy_colors) + 
	theme3 +
	scale_y_log10()+
	theme(axis.text.x = element_text(angle = 45, hjust = 1))+
	theme(legend.position = "none") +
	scale_x_discrete(labels = species_labels) 
c
# functional response rate (FRR) = a/h # without artemia, just mention in caption/text
d <- 
	ggplot(e, aes(x = species, y = fr_ratio, color = oxy)) +
	geom_point(size = 3, position = position_dodge(width = 0.3)) +
	geom_linerange(aes(ymin = fr_ratio - fr_lo, ymax = fr_ratio + fr_up), 
								 position = position_dodge(width = 0.3)) +
	labs(x = "", y = "Functional response ratio", color = "") +
	scale_color_manual(values = oxy_colors) +  
	theme3 +
	scale_y_log10()+
	theme(axis.text.x = element_text(angle = 45, hjust = 1))+
	theme(legend.position = "none") +
	scale_x_discrete(labels = species_labels) 
d
a+b+plot_layout(ncol = 2, byrow = FALSE,
										guides = "collect", widths = c(2,2))+
	plot_annotation(tag_levels = 'a')+
	theme3

ggsave("fr_estimates.png", dpi=1200,dev='png',height=8,width = 18,units = 'cm')

# Bootstrapped coefficients ################

## mnemiopsis ########
a_vector <- c(ml100_a_boots, ml30_a_boots)
h_vector <- c(ml100_h_boots, ml30_h_boots)
condition <- c(rep("normoxic", length(ml100_a_boots)), rep("hypoxic", length(ml30_a_boots)))
df_ml_boot <- data.frame(a = a_vector, h = h_vector, condition = condition)
head(df_ml_boot)

df_ml_boot$condition <- factor(df_ml_boot$condition, levels = c("normoxic", "hypoxic"))

i <- ggplot(df_ml_boot, aes(x=a, fill=condition)) +
	geom_density(alpha=0.8, color=NA, linewidth = 0)+
	ggtitle("Mnemiopsis")+
	theme3+
	labs(x = "Attack rate (a)",
			 y = "Frequency")+
	scale_fill_manual(values = c("cornflowerblue", "red4"),
										labels = c("Normoxia","Hypoxia")) +  
	theme(legend.title = element_blank())+
	xlim(0.0002,0.005)
i
ii <- ggplot(df_ml_boot, aes(x=h, fill=condition)) +
	geom_density(alpha=0.8, color=NA, linewidth = 0)+
	ggtitle("")+
	theme3+
	labs(x = "Handling time (h)",
			 y = "Frequency")+
	scale_fill_manual(values = c("cornflowerblue", "red4"),
										labels = c("Normoxia","Hypoxia")) +  
	theme(legend.title = element_blank())+
	xlim(0,1.4)
ii

## acartia ########
a_vector <- c(ac100_a_boots, ac30_a_boots)
h_vector <- c(ac100_h_boots, ac30_h_boots)
condition <- c(rep("normoxic", length(ac100_a_boots)), rep("hypoxic", length(ac30_a_boots)))
df_at_boot <- data.frame(a = a_vector, h = h_vector, condition = condition)
head(df_at_boot)

df_at_boot$condition <- factor(df_at_boot$condition, levels = c("normoxic", "hypoxic"))

iii <- ggplot(df_at_boot, aes(x=a, fill=condition)) +
	geom_density(alpha=0.8, color=NA, linewidth = 0)+
	ggtitle("Acartia")+
	theme3+
	labs(x = "Attack rate (a)",
			 y = "")+
	scale_fill_manual(values = c("cornflowerblue", "red4"),
										labels = c("Normoxia","Hypoxia")) +  
	theme(legend.title = element_blank())+
	xlim(0,0.075)
iii

iv <- ggplot(df_at_boot, aes(x=h, fill=condition)) +
	geom_density(alpha=0.8, color=NA, linewidth = 0)+
	ggtitle("")+
	theme3+
	labs(x = "Handling time (h)",
			 y = "")+
	scale_fill_manual(values = c("cornflowerblue", "red4"),
										labels = c("Normoxia","Hypoxia")) +  
	theme(legend.title = element_blank())+
	xlim(0,0.65)
iv

## artemia ########
a_vector <- c(as100_a_boots)
h_vector <- c(as100_h_boots)
condition <- c(rep("normoxic", length(as100_a_boots)))
df_as_boot <- data.frame(a = a_vector, h = h_vector, condition = condition)
head(df_as_boot)

v <- ggplot(df_as_boot, aes(x=a, fill=condition)) +
	geom_density(alpha=0.8, color=NA, linewidth = 0, show.legend = F)+
	ggtitle("Artemia")+
	theme3+
	labs(x = "Attack rate (a)",
			 y = "")+
	scale_fill_manual(values = c("cornflowerblue"),
										labels = c("Normoxia")) +  
	theme(legend.title = element_blank())+
	xlim(0,0.5)
v
vi <- ggplot(df_as_boot, aes(x=h, fill=condition)) +
	geom_density(alpha=0.8, color="white", linewidth = 0, show.legend = F)+
	ggtitle("")+
	theme3+
	labs(x = "Handling time (h)",
			 y = "")+
	scale_fill_manual(values = c("cornflowerblue"),
										labels = c("Normoxia")) +  
	theme(legend.title = element_blank())
vi
i+ii+iii+iv+v+vi+plot_layout(ncol = 4, byrow = FALSE,
														 guides = "collect", widths = c(1,1,1,0.2))+
	plot_annotation(tag_levels = 'a')
ggsave("fr_coeffs_dens.png", dpi=1200,dev='png',height=12.5,width = 32,units = 'cm')

# CI tests ####################

# mean((A-B)<0) 
mean((ml100_a_boots - ml30_a_boots)<0) # 0.020
mean((ml30_h_boots - ml100_h_boots)<0) # 0.006
frair_compare(ml100_fit,ml30_fit)

mean((ac100_a_boots - ac30_a_boots)<0) # 0.168
mean((ac30_h_boots - ac100_h_boots)<0) # 0.254
frair_compare(ac100_fit,ac30_fit)

mean((ac30_a_boots - ml100_a_boots)<0) # 0.154
mean((ac30_a_boots - ml30_a_boots)<0) # 0.016
frair_compare(ac100_fit, ml100_fit)

mean((ac100_a_boots - as100_a_boots)<0) # 0
mean((ac100_h_boots - as100_h_boots)<0) # 0.002

mean((ml100_a_boots - as100_a_boots)<0) # 0
mean((as100_h_boots - ml100_h_boots)<0) # 0.480

mean((ac100_h_boots - ml100_h_boots)<0) # 0.358
mean((ac30_h_boots - ml30_h_boots)<0) # 0.358

# predictions ########################
															 			 			
ml100_a <- ml100_fit$coefficients[1]*60 
ml100_h <- ml100_fit$coefficients[2]/60 
ml30_a <- ml30_fit$coefficients[1]*60
ml30_h <- ml30_fit$coefficients[2]/60 

1/ml30_h

ml30_h/ml100_h

predicted_predation <- (ml100_a * 1200) / (1 + ml100_a * ml100_h * 1200)
predicted_predation
predicted_predation*0.2959

predicted_predation <- (ml100_a * 154.8) / (1 + ml100_a * ml100_h * 154.8)
predicted_predation
22.8 * 3

predicted_predation <- (ml30_a * 1200) / (1 + ml30_a * ml30_h * 1200)
predicted_predation
predicted_predation*0.2959

at100_a <- ac100_fit$coefficients[1]*60 
at100_h <- ac100_fit$coefficients[2]/60 
at30_a <- ac30_fit$coefficients[1]*60
at30_h <- ac30_fit$coefficients[2]/60 

at30_h/at100_h

predicted_predation <- (at100_a * 1200) / (1 + at100_a * at100_h * 1200)
predicted_predation
predicted_predation*2.651
predicted_predation <- (at30_a * 1200) / (1 + at30_a * at30_h * 1200)
predicted_predation
predicted_predation*2.651

as100_a <- as100_fit$coefficients[1]*60
as100_h <- as100_fit$coefficients[2]/60 

predicted_predation <- (as100_a * 1200) / (1 + as100_a * as100_h * 1200)
predicted_predation
predicted_predation*0.7514
