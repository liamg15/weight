#load packages into environment
library(tidyverse)
library(here)
library(ggeffects)
library(data.table)
library(lubridate)

# Load and pre-process data
wbd <- read.delim(here("weight_data.txt"), sep = " ", header = F, col.names = c("Date", "Time", "Liam")) %>% 
  mutate(Date = as.numeric(as.Date(Date)-19492),
         Time = as.ITime(x = Time)) 

# Model weight change by day
mod <- lm(Liam ~ Date + Time, wbd)
summary(mod)
plot(mod)

# Predict weight by day from linear model
pred <- ggpredict(mod, terms = c("Date")) %>% 
  rename(Liam = predicted)

# Return dates back to date formats
pred$Date <- as_date(pred$x+19492)
wbd$Date <- as_date(wbd$Date+19492)

# Plot weight by day
pdf("weight.pdf", width = 4, height = 3.5)
wbd %>% mutate(Time = as.numeric(Time),
               Time = case_when(Time < 43200 ~ 'Morning',
                                Time < 61200 ~ "Afternoon",
                                Time < 75600 ~ "Evening",
                                Time >= 75600 ~ "Night"),
               Time = factor(Time, c("Morning", "Afternoon", "Evening", "Night"))) %>% 
  ggplot(aes(Date, Liam)) +
  geom_ribbon(data = pred, aes(ymin = conf.low, ymax = conf.high), alpha = 0.05) +
  geom_point(aes(size = Time, fill = Time), pch = 21) +
  geom_line(data = pred, color = "darkred") +
  labs(x = "Date", y = "Weight (lbs)") +
  guides(fill = guide_legend(title = "Time"), size = guide_legend(title = "Time")) + 
  theme_light() +
  theme(legend.position = "bottom")
 dev.off()

# Results:
# According to this model I gained 0.0098 lbs everyday since May 16, 2023. 
# At that rate, I would reach my goal of 170 lbs in 711 days (~ 2 years).
# I need to increase calories and muscle growth to reach this goal in a reasonable time frame.