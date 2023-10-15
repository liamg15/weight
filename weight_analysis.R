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
mod <- lm(Liam ~ Date, wbd)
summary(mod)
plot(mod)

# Predict weight by day from linear model
pred <- ggpredict(mod, terms = c("Date")) %>% 
  rename(Liam = predicted)

# Return dates back to date formats
pred$Date <- as_date(pred$x+19492)
wbd$Date <- as_date(wbd$Date+19492)

# Plot weight by day
#pdf("weight.pdf", width = 4, height = 3.5)
wbd %>% ggplot(aes(Date, Liam)) +
  geom_point(fill = "darkblue", pch = 21, size = 2, alpha = 0.75) +
  geom_line(data = pred, color = "darkred") +
  geom_ribbon(data = pred, aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  labs(x = "Date", y = "Weight (lbs)") +
  theme_light() 
#dev.off()

# Results:

# According to this model I gained 0.009 lbs everyday since May 16, 2023. 
# At that rate, I would reach my goal of 170 lbs in 711 days (~ 2 years).
# I need to increase calories and muscle growth to reach this goal in a reasonable time frame.