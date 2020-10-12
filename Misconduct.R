## Data Visualization (GOV 16 - QSS17) Fall 2017
## Lab 1 
##
## Name: James Wen
## Date: October 16, 2017 

# Inital Settings  --------------------------------------------------------

rm(list = ls())
library(tidyverse)
library(ggthemes)


# Load Data ---------------------------------------------------------------

wave.1 <- read.csv("General_Public_Survey/BLW_wave1_expert.csv", na.strings = c("", "NA")) 

wave.2 <- read.csv("General_Public_Survey/BLW_wave2_expert.csv", na.strings = c("", "NA")) 
  
wave.3 <- read.csv("General_Public_Survey/BLW_wave3_expert.csv", na.strings = c("", "NA")) 

# Wrangle Data ------------------------------------------------------------
wave.1.percent <- data.frame(prop.table(table(wave.1$perf_misconduct))) %>%
  mutate(month = "February") %>% 
  mutate(percent = Freq * 100) %>%
  rename(opinion = Var1)

wave.2.percent <- data.frame(prop.table(table(wave.2$perf_misconduct))) %>%
  mutate(month = "May") %>% 
  mutate(percent = Freq * 100) %>%
  rename(opinion = Var1)

wave.3.percent <- data.frame(prop.table(table(wave.3$perf_misconduct))) %>%
  mutate(month = "September") %>% 
  mutate(percent = Freq * 100) %>%
  rename(opinion = Var1)

# Combine Percent Data Frames ---------------------------------------------

waves <- bind_rows(wave.1.percent, wave.2.percent, wave.3.percent) 

# Make plot ---------------------------------------------------------------

ggplot(data = waves, aes(opinion, percent)) +
  geom_col(aes(fill = opinion)) +
  theme_minimal() +
  guides(fill = FALSE) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_discrete(limits = c("Does not meet", 
                              "Partly meets", 
                              "Mostly meets", 
                              "Fully meets"))+
  facet_wrap(~ month, nrow = 3) +
  labs(
    x = "How well does the following statement describe the United States as of today?
         Government officials are legally sanctioned for misconduct",
    y = "Percent",
    title = "The Trump Administration and Misconduct",
    subtitle = "Data Source: Bright Line Watch")
ggsave("figure/Lab1.pdf", scale = 0.8, width = 12, height = 8) 
