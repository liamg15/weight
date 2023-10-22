#load packages into environment
library(tidyverse)
library(here)
library(ggeffects)
library(data.table)
library(lubridate)
library(ggsci)

# Load and pre-process data
wbd <- read.delim(here("weight_data.txt"), sep = " ", header = F, col.names = c("Date", "Time", "Liam")) %>% 
  mutate(Date = as.numeric(as.Date(Date)-19492),
         Time = as.ITime(x = Time)) 

# Model weight change by day
mod <- lm(Liam ~ Date + Time, wbd)
summary(mod)

# Predict weight by day from linear model
pred <- ggpredict(mod, terms = c("Date")) %>% 
  rename(Liam = predicted)

# Return dates back to date formats
pred$Date <- as_date(pred$x+19492)
wbd$Date <- as_date(wbd$Date+19492)

# Plot weight by day
pdf("weight.pdf", width = 7, height = 4)
wbd %>% mutate(Time = as.numeric(Time),
               Time = case_when(Time < 34200 ~ 'Morning (5:30-9:29AM)',
                                Time < 43200 ~ 'Morning (9:30-11:59AM)',
                                Time < 54000 ~ "Afternoon (12:30-2:59PM)",
                                Time < 64800 ~ 'Afternoon (3:00-5:59PM)',
                                Time < 75600 ~ "Night (6:00-8:59PM)",
                                Time <= 86400 ~ 'Night (9:00-11:59PM)'),
               Time = factor(Time, c('Morning (5:30-9:29AM)','Morning (9:30-11:59AM)', "Afternoon (12:30-2:59PM)",'Afternoon (3:00-5:59PM)',"Night (6:00-8:59PM)",'Night (9:00-11:59PM)'))) %>% 
  ggplot(aes(Date, Liam)) +
  geom_line(data = pred, aes(y = conf.low), colour = "grey50", linetype = "dashed") +
  geom_line(data = pred,aes(y = conf.high), colour = "grey50", linetype = "dashed") +
  geom_point(aes(size = Time, fill = Time), pch = 21) +
  geom_line(data = pred, color = "darkred") +
  labs(x = "", y = "Weight (lbs)") +
  guides(fill = guide_legend(title = ""), size = guide_legend(title = "")) + 
  scale_fill_jama() +
  theme_light() +
  theme(legend.position = "bottom")
 dev.off()

# Results:
# According to this model I gained 0.0098 lbs everyday since May 16, 2023. 
# At that rate, I would reach my goal of 170 lbs in 711 days (~ 2 years).
# I need to increase calories and muscle growth to reach this goal in a reasonable time frame.
 
 