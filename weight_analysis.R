#load packages into environment
library(tidyverse)
library(here)
library(ggeffects)
library(data.table)
library(lubridate)
library(ggsci)
library(ggpubr)

# Load and pre-process data
wbd <- read.delim(here("weight_data.txt"), sep = " ", header = F, col.names = c("Date", "Time", "Liam")) %>% 
  mutate(Date = as.numeric(as.Date(Date)-19492),
         Time = as.ITime(x = Time)) 

# Model weight change by day
mod <- lm(Liam ~ Date + Time, wbd)
sumy <- summary(mod)
sumy
m_date <- paste0("Slope~(se) == ",paste(round(sumy[["coefficients"]][2], digits = 4),"~(", round(sumy[["coefficients"]][5], digits = 4), ")", sep = " "))
ar2 <- round(sumy[["adj.r.squared"]],digits = 2)
# Predict weight by day from linear model
pred <- ggpredict(mod, terms = c("Date")) %>% 
  rename(Liam = predicted)

# Return dates back to date formats
pred$Date <- as_date(pred$x+19492)
wbd$Date <- as_date(wbd$Date+19492)

wbd1 <- wbd %>% 
  mutate(Time = as.numeric(Time),
       Time = case_when(Time < 34200 ~ 'Morning (5:30-9:29AM)',
                        Time < 43200 ~ 'Morning (9:30-11:59AM)',
                        Time < 54000 ~ "Afternoon (12:30-2:59PM)",
                        Time < 64800 ~ 'Afternoon (3:00-5:59PM)',
                        Time < 75600 ~ "Night (6:00-8:59PM)",
                        Time <= 86400 ~ 'Night (9:00-11:59PM)'),
       
       Time = factor(Time, c('Morning (5:30-9:29AM)','Morning (9:30-11:59AM)', 
                             "Afternoon (12:30-2:59PM)",'Afternoon (3:00-5:59PM)',
                             "Night (6:00-8:59PM)",'Night (9:00-11:59PM)'))) 

# Distribution of weight
plot.new()
hist(wbd1$Liam, 
     breaks = 20,
     col="grey",
     border="black",
     prob = TRUE,
     xlab = "weight",
     main = "Weight Distribution")
lines(density(wbd1$Liam),
      lwd = 2,
      col = "blue")

shapiro.test(wbd1$Liam) # Roughly normally distributed? according the Shapiro-Wilks test for normality although it looks skewed. 

# Plot weight by day

a <- wbd1 %>% 
  ggplot(aes(Date, Liam)) +
  geom_line(data = pred, aes(y = conf.low), colour = "grey50", linetype = "dashed") +
  geom_line(data = pred,aes(y = conf.high), colour = "grey50", linetype = "dashed") +
  geom_point(aes(size = Time, fill = Time), pch = 21) +
  geom_line(data = pred, color = "darkred") +
  annotate("text", label = paste(m_date, "~lbs/day"), x = as.Date("2023-06-12"), y = 168.25, parse = T, size = 2) +
  annotate("text", label = paste("aR^2 == ", ar2), x = as.Date("2023-06-12"), y = 167.5, parse = T, size = 2) +
  labs(x = "", y = "Weight (lbs)") +
  guides(fill = guide_legend(title = ""), size = guide_legend(title = "")) + 
  scale_fill_jama() +
  theme_light() +
  theme(legend.position = "bottom")

b <- wbd1 %>% 
  ggplot(aes(Date, "0")) +
  geom_point(pch = "|", size = 7) +
  theme_void() 

c <- wbd1 %>% 
  ggplot(aes("0", Liam)) +
  geom_point(pch = "-", size = 7) +
  theme_void() 

pdf("weight.pdf", width = 7, height = 4)
ggarrange(b,NULL, a, c, ncol = 2, nrow = 2, 
          widths = c(4, 1), heights = c(1, 4), align = "hv", common.legend = T, legend = "bottom")
dev.off()
# Results:
# According to this model I gained 0.0098 lbs everyday since May 16, 2023. 
# At that rate, I would reach my goal of 170 lbs in 711 days (~ 2 years).
# I need to increase calories and muscle growth to reach this goal in a reasonable time frame.
 
 