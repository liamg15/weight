# Weight by day (goal: reach 170 lbs)
Monitoring my weight, recording everytime I go to the gym.

Here is the analysis so far from May 16 to October 15, 2023:
-  According to a linear regression model of date and time with weight as the outcome, I gained 0.0098 lbs every day since May 16, 2023.
-  At that rate, I would reach my goal of 170 lbs in 711 days (~ 2 years).
-  Interestingly, my weight in the early mornings is much higher than later in the day, the beta for time (hours) was -0.11 lbs. Assuming a linear response, my weight decreases by 0.11 lbs per hour, however, this doesn't take into account the rhythmic nature of weight by hour/minute.

The next iteration of the model will include a mathematical adjustment for circadian rhythms.
