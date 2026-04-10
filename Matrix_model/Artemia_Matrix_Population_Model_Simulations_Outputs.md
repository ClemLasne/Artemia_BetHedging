### In R

```ruby
library(tidyverse)
library(ggpubr)

# Imports simulation output data
pathway = "./Simulation_Outputs/"
files <- list.files(path = pathway)

file.list <- list()
for(i in 1:length(files)){
  file.list[[i]] <- readRDS(file = paste0(pathway,files[i]))
}

simdata <- do.call(rbind, file.list)
rm(file.list)

# Scenario 2.4 and 3.5 were in fact using the same parameters and are duplicated
# Scenario parameters
simdata$scen.par <- paste0(simdata$Mean.Cat, ".",
                           simdata$SD.Cat, ".",
                           simdata$Mean.Switch, ".",
                           simdata$SD.Switch, ".",
                           simdata$demog.stoch, ".",
                           simdata$env.stoch, ".",
                           simdata$cat.stoch, "_",
                           simdata$BH_value)

# Duplicated scenario simulations removed
simdata$duplicated <- duplicated(simdata[,c("Time","Simulation", "scen.par")])
simdata <- simdata[which(simdata$duplicated == FALSE),]

# Summarises data for mean population growth rate and confidence intervals
agg.data <-
  simdata %>% group_by(.,scen.par) %>% 
    summarise(mean_stoch_lambda = mean(Stoch.Lambda),
              LCI = quantile(Stoch.Lambda, probs = c(0.025)),
              UCI = quantile(Stoch.Lambda, probs = c(0.975)),
              n=n()
              ) %>%
    mutate(mean_stoch_lambda = format(mean_stoch_lambda, digits = 7, scientific = FALSE),
           LCI = format(LCI, digits = 7, scientific = FALSE),
           UCI = format(UCI, digits = 7, scientific = FALSE))

parameters <- 
  unique(simdata[simdata$Time == 0 & simdata$Simulation == 1,])

agg.data <- merge(agg.data, parameters, all.x = TRUE, by.x = "scen.par", by.y = "scen.par", all.y = FALSE)

agg.data <- agg.data[,c("scen.par", "mean_stoch_lambda", "UCI", "LCI", "Mean.Cat","SD.Cat", "Mean.Switch",
                        "SD.Switch", "demog.stoch","env.stoch","cat.stoch","BH_value")]

# Selects subset of data for plot
plot.data <- 
  agg.data %>% 
  filter(demog.stoch == 1) %>% 
  filter(env.stoch == 1) %>%
  filter(cat.stoch == 1) %>% 
  filter(Mean.Switch == 6) %>%
  filter(SD.Switch == 0) %>%
  filter(SD.Cat == 0)

plot.data$mean_stoch_lambda <- plot.data$mean_stoch_lambda %>% as.numeric()
plot.data$UCI <- plot.data$UCI %>% as.numeric()
plot.data$LCI <- plot.data$LCI %>% as.numeric()
plot.data$BH_value <- plot.data$BH_value %>% as.numeric()
plot.data$Mean.Cat <- plot.data$Mean.Cat %>% as.factor()

plot.1a <- 
  ggplot(plot.data, aes(x = Mean.Cat, y = mean_stoch_lambda, group = BH_value, colour = BH_value))+
    geom_line(position= position_dodge(width = 0.2))+
    geom_pointrange(aes(y = mean_stoch_lambda, ymax = UCI, ymin = LCI), position= position_dodge(width = 0.2))+
    #scale_y_continuous(limits = c(1.135,1.205))+
    labs(title = "1a. High Catastrophe Predictability (SD = 0)",
         y = expression("Mean Stochastic\nFinite Population Growth Rate   " * bgroup("(", lambda[S], ")")),
         x = "Mean Inter-catastrophic Period \n(n 5-day brood cycles)",
         colour = "Bet-Hedging Degree \n(Proportion Cystic Females \nin favourable conditions)\n")+
    theme_bw()+
    theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))
     
# Selects subset of data for plot
plot.data <- 
  agg.data %>% 
  filter(demog.stoch == 1) %>% 
  filter(env.stoch == 1) %>%
  filter(cat.stoch == 1) %>% 
  filter(Mean.Switch == 6) %>%
  filter(SD.Switch == 0) %>%
  filter(SD.Cat == 3)

plot.data$mean_stoch_lambda <- plot.data$mean_stoch_lambda %>% as.numeric()
plot.data$UCI <- plot.data$UCI %>% as.numeric()
plot.data$LCI <- plot.data$LCI %>% as.numeric()
plot.data$BH_value <- plot.data$BH_value %>% as.numeric()
plot.data$Mean.Cat <- plot.data$Mean.Cat %>% as.factor()

plot.1b <- 
  ggplot(plot.data, aes(x = Mean.Cat, y = mean_stoch_lambda, group = BH_value, colour = BH_value))+
  geom_line(position= position_dodge(width = 0.2))+
  geom_pointrange(aes(y = mean_stoch_lambda, ymax = UCI, ymin = LCI), position= position_dodge(width = 0.2))+
  #scale_y_continuous(limits = c(1.135,1.205))+
  labs(title = "1b. Medium Catastrophe Predictability (SD = 3)",
       y = expression("Mean Stochastic\nFinite Population Growth Rate   " * bgroup("(", lambda[S], ")")),
       x = "Mean Inter-catastrophic Period \n(n 5-day brood cycles)",
       colour = "Bet-Hedging Degree \n(Proportion Cystic Females \nin favourable conditions)\n")+
  theme_bw()+
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))

# Selects subset of data for plot
plot.data <- 
  agg.data %>% 
  filter(demog.stoch == 1) %>% 
  filter(env.stoch == 1) %>%
  filter(cat.stoch == 1) %>% 
  filter(Mean.Switch == 6) %>%
  filter(SD.Switch == 0) %>%
  filter(SD.Cat == 6)

plot.data$mean_stoch_lambda <- plot.data$mean_stoch_lambda %>% as.numeric()
plot.data$UCI <- plot.data$UCI %>% as.numeric()
plot.data$LCI <- plot.data$LCI %>% as.numeric()
plot.data$BH_value <- plot.data$BH_value %>% as.numeric()
plot.data$Mean.Cat <- plot.data$Mean.Cat %>% as.factor()

plot.1c <- 
  ggplot(plot.data, aes(x = Mean.Cat, y = mean_stoch_lambda, group = BH_value, colour = BH_value))+
  geom_line(position= position_dodge(width = 0.2))+
  geom_pointrange(aes(y = mean_stoch_lambda, ymax = UCI, ymin = LCI), position= position_dodge(width = 0.2))+
  #scale_y_continuous(limits = c(1.135,1.205))+
  labs(title = "1c. Low Catastrophe Predictability (SD = 6)",
       y = expression("Mean Stochastic\nFinite Population Growth Rate   " * bgroup("(", lambda[S], ")")),
       x = "Mean Inter-catastrophic Period \n(n 5-day brood cycles)",
       colour = "Bet-Hedging Degree \n(Proportion Cystic Females \nin favourable conditions)\n")+
  theme_bw()+
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))


legend <- get_legend(plot.1c)
plot.1a <- plot.1a + scale_colour_continuous(guide = "none")
plot.1b <- plot.1b + scale_colour_continuous(guide = "none")
plot.1c <- plot.1c + scale_colour_continuous(guide = "none")

jpeg("Simulation_plots_Catastrophe_Interval_v6.0.jpg", width = 24, height = 18, units = "cm", res = 300)
ggarrange(plot.1a, plot.1b, plot.1c, legend, nrow=2,ncol=2)
dev.off()

#ggarrange(plot.1a, plot.1b, plot.1c, legend, nrow=2,ncol=2, common.legend = TRUE, legend="right")



# Selects subset of data for plot
plot.data <- 
  agg.data %>% 
  filter(demog.stoch == 1) %>% 
  filter(env.stoch == 1) %>%
  filter(cat.stoch == 1) %>% 
  filter(Mean.Cat == 24) %>%
  filter(SD.Cat == 0) %>%
  filter(SD.Switch == 0)

plot.data$mean_stoch_lambda <- plot.data$mean_stoch_lambda %>% as.numeric()
plot.data$UCI <- plot.data$UCI %>% as.numeric()
plot.data$LCI <- plot.data$LCI %>% as.numeric()
plot.data$BH_value <- plot.data$BH_value %>% as.numeric()
plot.data$Mean.Switch <- plot.data$Mean.Switch %>% as.factor()

plot.2a <- 
  ggplot(plot.data, aes(x = Mean.Switch, y = mean_stoch_lambda, group = BH_value, colour = BH_value))+
  geom_line(position= position_dodge(width = 0.2))+
  geom_pointrange(aes(y = mean_stoch_lambda, ymax = UCI, ymin = LCI), position= position_dodge(width = 0.2))+
  #scale_y_continuous(limits = c(1.15,1.405))+
  labs(title = "2a. High Fav. Period Predictability (SD = 0)",
       y = expression("Mean Stochastic\nFinite Population Growth Rate   " * bgroup("(", lambda[S], ")")),
       x = "Mean Switching Interval between \nFavourable and Unfavourable Conditions \n(brood cycles)",
       colour = "Bet-Hedging Degree \n(Proportion Cystic Females \nin favourable conditions)\n")+
  theme_bw()+
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))

# Selects subset of data for plot
plot.data <- 
  agg.data %>% 
  filter(demog.stoch == 1) %>% 
  filter(env.stoch == 1) %>%
  filter(cat.stoch == 1) %>% 
  filter(Mean.Cat == 24) %>%
  filter(SD.Cat == 0) %>%
  filter(SD.Switch == 1)

plot.data$mean_stoch_lambda <- plot.data$mean_stoch_lambda %>% as.numeric()
plot.data$UCI <- plot.data$UCI %>% as.numeric()
plot.data$LCI <- plot.data$LCI %>% as.numeric()
plot.data$BH_value <- plot.data$BH_value %>% as.numeric()
plot.data$Mean.Switch <- plot.data$Mean.Switch %>% as.factor()

plot.2b <- 
  ggplot(plot.data, aes(x = Mean.Switch, y = mean_stoch_lambda, group = BH_value, colour = BH_value))+
  geom_line(position= position_dodge(width = 0.2))+
  geom_pointrange(aes(y = mean_stoch_lambda, ymax = UCI, ymin = LCI), position= position_dodge(width = 0.2))+
  #scale_y_continuous(limits = c(1.15,1.405))+
  labs(title = "2b. Medium Fav. Period Predictability (SD = 1)",
       y = expression("Mean Stochastic\nFinite Population Growth Rate   " * bgroup("(", lambda[S], ")")),
       x = "Mean Switching Interval between \nFavourable and Unfavourable Conditions \n(brood cycles)",
       colour = "Bet-Hedging Degree \n(Proportion Cystic Females \nin favourable conditions)\n")+
  theme_bw()+
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))

# Selects subset of data for plot
plot.data <- 
  agg.data %>% 
  filter(demog.stoch == 1) %>% 
  filter(env.stoch == 1) %>%
  filter(cat.stoch == 1) %>% 
  filter(Mean.Cat == 24) %>%
  filter(SD.Cat == 0) %>%
  filter(SD.Switch == 2)

plot.data$mean_stoch_lambda <- plot.data$mean_stoch_lambda %>% as.numeric()
plot.data$UCI <- plot.data$UCI %>% as.numeric()
plot.data$LCI <- plot.data$LCI %>% as.numeric()
plot.data$BH_value <- plot.data$BH_value %>% as.numeric()
plot.data$Mean.Switch <- plot.data$Mean.Switch %>% as.factor()

plot.2c<- 
  ggplot(plot.data, aes(x = Mean.Switch, y = mean_stoch_lambda, group = BH_value, colour = BH_value))+
  geom_line(position= position_dodge(width = 0.2))+
  geom_pointrange(aes(y = mean_stoch_lambda, ymax = UCI, ymin = LCI), position= position_dodge(width = 0.2))+
  #scale_y_continuous(limits = c(1.15,1.405))+
  labs(title = "2c. Low Fav. Period Predictability (SD = 2)",
       y = expression("Mean Stochastic\nFinite Population Growth Rate   " * bgroup("(", lambda[S], ")")),
       x = "Mean Switching Interval between \nFavourable and Unfavourable Conditions \n(brood cycles)",
       colour = "Bet-Hedging Degree \n(Proportion Cystic Females \nin favourable conditions)\n")+
  theme_bw()+
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))

legend <- get_legend(plot.2c)
plot.2a <- plot.2a + scale_colour_continuous(guide = "none")
plot.2b <- plot.2b + scale_colour_continuous(guide = "none")
plot.2c <- plot.2c + scale_colour_continuous(guide = "none")

jpeg("Simulation_plots_Switching_Interval_v6.0.jpg", width = 24, height = 18, units = "cm", res = 300)
ggarrange(plot.2a, plot.2b, plot.2c, legend, nrow=2,ncol=2)
dev.off()

#ggarrange(plot.1a, plot.1b, plot.1c, legend, nrow=2,ncol=2, common.legend = TRUE, legend="right")



# Selects subset of data for plot
plot.data <- 
  agg.data %>% 
  filter(demog.stoch == 1) %>% 
  filter(env.stoch == 1) %>%
  filter(cat.stoch == 1) %>% 
  filter(Mean.Switch == 6) %>%
  filter(SD.Switch == 0) %>%
  filter(SD.Cat == 0)

plot.data$mean_stoch_lambda <- plot.data$mean_stoch_lambda %>% as.numeric()
plot.data$UCI <- plot.data$UCI %>% as.numeric()
plot.data$LCI <- plot.data$LCI %>% as.numeric()
plot.data$BH_value <- plot.data$BH_value %>% as.numeric()
plot.data$Mean.Cat <- plot.data$Mean.Cat %>% as.numeric()
plot.data$SD.Cat <- plot.data$SD.Cat %>% as.factor()

plot.3a <- 
  ggplot(plot.data, aes(x = BH_value, y = mean_stoch_lambda, group = Mean.Cat, colour = Mean.Cat))+
  geom_line(position= position_dodge(width = 0.02))+
  geom_pointrange(aes(y = mean_stoch_lambda, ymax = UCI, ymin = LCI), position= position_dodge(width = 0.02))+
  scale_x_continuous(limits = c(-0.02,1.02), breaks = c(0,0.2,0.4,0.6,0.8,1))+
  scale_colour_gradientn(breaks = c(12,24,36), labels = c("12 (High variability)", "24 (Medium variability)", "36 (Low variability)"), colours = c("#03045E", "#0077B6", "#90E0EF"))+
  #scale_y_continuous(limits = c(1.135,1.205))+
  labs(title = "a. High Catastrophe Predictability (SD = 0)",
       y = expression("Mean Stochastic\nFinite Population Growth Rate   " * bgroup("(", lambda[S], ")")),
       colour = "Mean Catastrophic \nPeriod Interval \n(brood cycles)\n ",
       x = "Bet-Hedging Degree \n(Proportion Cystic Females in favourable conditions)\n")+
  theme_bw()+
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        panel.grid.minor = element_blank())


# Selects subset of data for plot
plot.data <- 
  agg.data %>% 
  filter(demog.stoch == 1) %>% 
  filter(env.stoch == 1) %>%
  filter(cat.stoch == 1) %>% 
  filter(Mean.Switch == 6) %>%
  filter(SD.Switch == 0) %>%
  filter(SD.Cat == 3)

plot.data$mean_stoch_lambda <- plot.data$mean_stoch_lambda %>% as.numeric()
plot.data$UCI <- plot.data$UCI %>% as.numeric()
plot.data$LCI <- plot.data$LCI %>% as.numeric()
plot.data$BH_value <- plot.data$BH_value %>% as.numeric()
plot.data$Mean.Cat <- plot.data$Mean.Cat %>% as.numeric()

plot.3b <- 
  ggplot(plot.data, aes(x = BH_value, y = mean_stoch_lambda, group = Mean.Cat, colour = Mean.Cat))+
  geom_line(position= position_dodge(width = 0.02))+
  geom_pointrange(aes(y = mean_stoch_lambda, ymax = UCI, ymin = LCI), position= position_dodge(width = 0.02))+
  scale_x_continuous(limits = c(-0.02,1.02), breaks = c(0,0.2,0.4,0.6,0.8,1))+
  scale_colour_gradientn(breaks = c(12,24,36), labels = c("12 (High variability)", "24 (Medium variability)", "36 (Low variability)"), colours = c("#03045E", "#0077B6", "#90E0EF"))+
  #scale_y_continuous(limits = c(1.135,1.205))+
  labs(title = "b. Medium Catastrophe Predictability (SD = 3)",
       y = expression("Mean Stochastic\nFinite Population Growth Rate   " * bgroup("(", lambda[S], ")")),
       colour = "Mean Catastrophic \nPeriod Interval \n(brood cycles)\n ",
       x = "Bet-Hedging Degree \n(Proportion Cystic Females in favourable conditions)\n")+
  theme_bw()+
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        panel.grid.minor = element_blank())

# Selects subset of data for plot
plot.data <- 
  agg.data %>% 
  filter(demog.stoch == 1) %>% 
  filter(env.stoch == 1) %>%
  filter(cat.stoch == 1) %>% 
  filter(Mean.Switch == 6) %>%
  filter(SD.Switch == 0) %>%
  filter(SD.Cat == 6)

plot.data$mean_stoch_lambda <- plot.data$mean_stoch_lambda %>% as.numeric()
plot.data$UCI <- plot.data$UCI %>% as.numeric()
plot.data$LCI <- plot.data$LCI %>% as.numeric()
plot.data$BH_value <- plot.data$BH_value %>% as.numeric()
plot.data$Mean.Cat <- plot.data$Mean.Cat %>% as.numeric()

plot.3c <- 
  ggplot(plot.data, aes(x = BH_value, y = mean_stoch_lambda, group = Mean.Cat, colour = Mean.Cat))+
  geom_line(position= position_dodge(width = 0.02))+
  geom_pointrange(aes(y = mean_stoch_lambda, ymax = UCI, ymin = LCI), position= position_dodge(width = 0.02))+
  scale_x_continuous(limits = c(-0.02,1.02), breaks = c(0,0.2,0.4,0.6,0.8,1))+
  scale_colour_gradientn(breaks = c(12,24,36), labels = c("12 (High variability)", "24 (Medium variability)", "36 (Low variability)"), colours = c("#03045E", "#0077B6", "#90E0EF"))+
  #scale_y_continuous(limits = c(1.135,1.205))+
  labs(title = "c. Low Catastrophe Predictability (SD = 6)",
       y = expression("Mean Stochastic\nFinite Population Growth Rate   " * bgroup("(", lambda[S], ")")),
       colour = "Mean Catastrophic \nPeriod Interval \n(brood cycles)\n ",
       x = "Bet-Hedging Degree \n(Proportion Cystic Females in favourable conditions)\n")+
  theme_bw()+
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        panel.grid.minor = element_blank())


legend <- get_legend(plot.3c)
plot.3a <- plot.3a + scale_colour_continuous(guide = "none")
plot.3b <- plot.3b + scale_colour_continuous(guide = "none")
plot.3c <- plot.3c + scale_colour_continuous(guide = "none")

jpeg("Simulation_plots_Catastrophe_Interval_v6.0_BHX.jpg", width = 24, height = 18, units = "cm", res = 300)
ggarrange(plot.3a, plot.3b, plot.3c, legend, nrow=2,ncol=2)
dev.off()


#ggarrange(plot.1a, plot.1b, plot.1c, legend, nrow=2,ncol=2, common.legend = TRUE, legend="right")





# Selects subset of data for plot
plot.data <- 
  agg.data %>% 
  filter(demog.stoch == 1) %>% 
  filter(env.stoch == 1) %>%
  filter(cat.stoch == 1) %>% 
  filter(Mean.Cat == 24) %>%
  filter(SD.Cat == 0) %>%
  filter(SD.Switch == 0)

plot.data$mean_stoch_lambda <- plot.data$mean_stoch_lambda %>% as.numeric()
plot.data$UCI <- plot.data$UCI %>% as.numeric()
plot.data$LCI <- plot.data$LCI %>% as.numeric()
plot.data$BH_value <- plot.data$BH_value %>% as.numeric()
plot.data$Mean.Cat <- plot.data$Mean.Cat %>% as.numeric()
plot.data$SD.Cat <- plot.data$SD.Cat %>% as.factor()
plot.data$SD.Switch <- plot.data$SD.Switch %>% as.factor()

plot.4a <- 
  ggplot(plot.data, aes(x = BH_value, y = mean_stoch_lambda, group = Mean.Switch, colour = Mean.Switch))+
  geom_line(position= position_dodge(width = 0.02))+
  geom_pointrange(aes(y = mean_stoch_lambda, ymax = UCI, ymin = LCI), position= position_dodge(width = 0.02))+
  scale_x_continuous(limits = c(-0.02,1.02), breaks = c(0,0.2,0.4,0.6,0.8,1))+
  scale_colour_gradientn(breaks = c(3,6,9), labels = c("3 (High variability)", "6 (Medium variability)", "9 (Low variability)"), colours = c("#03045E", "#0077B6", "#90E0EF"))+
  #scale_y_continuous(limits = c(1.135,1.205))+
  labs(title = "a. High Predictability of \nEnvironment Condition (SD = 0)",
       y = expression("Mean Stochastic\nFinite Population Growth Rate   " * bgroup("(", lambda[S], ")")),
       colour = "Mean Switching Interval between \nFavourable and Unfavourable Conditions \n(brood cycles) \n ",
       x = "Bet-Hedging Degree \n(Proportion Cystic Females in favourable conditions)\n")+
  theme_bw()+
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        panel.grid.minor = element_blank())

# Selects subset of data for plot
plot.data <- 
  agg.data %>% 
  filter(demog.stoch == 1) %>% 
  filter(env.stoch == 1) %>%
  filter(cat.stoch == 1) %>% 
  filter(Mean.Cat == 24) %>%
  filter(SD.Cat == 0) %>%
  filter(SD.Switch == 1)

plot.data$mean_stoch_lambda <- plot.data$mean_stoch_lambda %>% as.numeric()
plot.data$UCI <- plot.data$UCI %>% as.numeric()
plot.data$LCI <- plot.data$LCI %>% as.numeric()
plot.data$BH_value <- plot.data$BH_value %>% as.numeric()
plot.data$Mean.Cat <- plot.data$Mean.Cat %>% as.numeric()

plot.4b <- 
  ggplot(plot.data, aes(x = BH_value, y = mean_stoch_lambda, group = Mean.Switch, colour = Mean.Switch))+
  geom_line(position= position_dodge(width = 0.02))+
  geom_pointrange(aes(y = mean_stoch_lambda, ymax = UCI, ymin = LCI), position= position_dodge(width = 0.02))+
  scale_x_continuous(limits = c(-0.02,1.02), breaks = c(0,0.2,0.4,0.6,0.8,1))+
  scale_colour_gradientn(breaks = c(3,6,9), labels = c("3 (High variability)", "6 (Medium variability)", "9 (Low variability)"), colours = c("#03045E", "#0077B6", "#90E0EF"))+
  #scale_y_continuous(limits = c(1.135,1.205))+
  labs(title = "b. Medium Predictability of \nEnvironment Condition (SD = 1)",
       y = expression("Mean Stochastic\nFinite Population Growth Rate   " * bgroup("(", lambda[S], ")")),
       colour = "Mean Switching Interval between \nFavourable and Unfavourable Conditions \n(brood cycles) \n ",
       x = "Bet-Hedging Degree \n(Proportion Cystic Females in favourable conditions)\n")+
  theme_bw()+
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        panel.grid.minor = element_blank())

# Selects subset of data for plot
plot.data <- 
  agg.data %>% 
  filter(demog.stoch == 1) %>% 
  filter(env.stoch == 1) %>%
  filter(cat.stoch == 1) %>% 
  filter(Mean.Cat == 24) %>%
  filter(SD.Cat == 0) %>%
  filter(SD.Switch == 2)

plot.data$mean_stoch_lambda <- plot.data$mean_stoch_lambda %>% as.numeric()
plot.data$UCI <- plot.data$UCI %>% as.numeric()
plot.data$LCI <- plot.data$LCI %>% as.numeric()
plot.data$BH_value <- plot.data$BH_value %>% as.numeric()
plot.data$Mean.Cat <- plot.data$Mean.Cat %>% as.numeric()

plot.4c <- 
  ggplot(plot.data, aes(x = BH_value, y = mean_stoch_lambda, group = Mean.Switch, colour = Mean.Switch))+
  geom_line(position= position_dodge(width = 0.02))+
  geom_pointrange(aes(y = mean_stoch_lambda, ymax = UCI, ymin = LCI), position= position_dodge(width = 0.02))+
  scale_x_continuous(limits = c(-0.02,1.02), breaks = c(0,0.2,0.4,0.6,0.8,1))+
  scale_colour_gradientn(breaks = c(3,6,9), labels = c("3 (High variability)", "6 (Medium variability)", "9 (Low variability)"), colours = c("#03045E", "#0077B6", "#90E0EF"))+
  #scale_y_continuous(limits = c(1.135,1.205))+
  labs(title = "c. Low Predictability of \nEnvironment Condition (SD = 2)",
       y = expression("Mean Stochastic\nFinite Population Growth Rate   " * bgroup("(", lambda[S], ")")),
       colour = "Mean Switching Interval between \nFavourable and Unfavourable Conditions \n(brood cycles) \n ",
       x = "Bet-Hedging Degree \n(Proportion Cystic Females in favourable conditions)\n")+
  theme_bw()+
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        panel.grid.minor = element_blank())


legend <- get_legend(plot.4c)
plot.4a <- plot.4a + scale_colour_continuous(guide = "none")
plot.4b <- plot.4b + scale_colour_continuous(guide = "none")
plot.4c <- plot.4c + scale_colour_continuous(guide = "none")

jpeg("Simulation_plots_Switching_Interval_v6.0_BHX.jpg", width = 24, height = 18, units = "cm", res = 300)
ggarrange(plot.4a, plot.4b, plot.4c, legend, nrow=2,ncol=2)
dev.off()


ggarrange(plot.1a, plot.1b, plot.1c, legend, nrow=2,ncol=2, common.legend = TRUE, legend="right")
```







