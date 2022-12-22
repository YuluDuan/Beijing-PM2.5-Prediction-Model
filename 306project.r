#load library
library(broom)
library(tidyverse)
library(dplyr)
library(lubridate)
library(grid)

# loading data
pm2.5 <- read.csv("~/project.csv", header=TRUE)
pm2.5 <- as_tibble(pm2.5)
head(pm2.5)

#remove the all the moments of a day with at least one hour missing data
#remove the line with missing data
pm2.5_n<- na.omit(pm2.5)
#number of data witout na
nrow(pm2.5_n)

pm2.5_tidy<- pm2.5_n%>%add_count(year,month,day)%>%
  filter(n==24)
# number of remined data
nrow(pm2.5_tidy)

# view the dataframe to make sure it is processed properly
head(pm2.5_tidy)
idx_pos <- pm2.5_tidy$pm2.5>0
pm2.5_tidy <- pm2.5_tidy[idx_pos,]
pm2.5_tidy$pm2.5 <- log(pm2.5_tidy$pm2.5)

sum(pm2.5_tidy$pm2.5<30)

#Summarize the data, counting how many occurences of each year
pm2.5_tidy%>%
  group_by(year)%>%
  summarize(count=n())

#classify with season
pm2.5_s <- pm2.5_tidy %>%
  mutate(
    season = case_when(
      month %in% 9:11 ~ "Fall",
      month %in%  3:5  ~ "Spring",
      month %in%  6:8  ~ "Summer",
      TRUE ~ "Winter"))
pm2.5_s$season <- as.factor(pm2.5_s$season)
pm2.5_s$cbwd <- as.factor(pm2.5_s$cbwd)
head(pm2.5_s)
#season check
season_check<-pm2.5_s%>% group_by(season)%>% summarize(n=n())
season_check
#use box plot to explore the seasonal characteristics of pm2.5 index 
options(repr.plot.width = 8, repr.plot.height = 10)
season_p<-ggplot(pm2.5_s, aes(x=season, y=pm2.5, color=season)) +
  geom_boxplot() +
  xlab("Season") +
  ylab("PM2.5 concentration (ug/m^3)") +
  theme(text = element_text(size = 18))+
  # Box plot with mean points
  stat_summary(fun.y=mean, geom="point", shape=23, size=4)

season_p

options(repr.plot.width = 12, repr.plot.height = 6)
idx_s <- pm2.5_s$season=="Fall"
fall<- pm2.5_s[idx_s,]
dewp_season_fall <- ggplot(fall, aes(x = DEWP, y = pm2.5)) +
  geom_point(
    color = 'orange',
    size = 2,
    alpha = 0.3
  )+
  xlab("Dew Point °C Td") +
  ylab("PM2.5 concentration (ug/m^3)") +
  ggtitle("Dew Point VS PM2.5 concentration in Fall")+
  theme(text = element_text(size = 18))


options(repr.plot.width = 12, repr.plot.height = 6)
idx_s1 <- pm2.5_s$season=="Winter"
winter<- pm2.5_s[idx_s1,]
dewp_season_winter <- ggplot(winter, aes(x = DEWP, y = pm2.5)) +
  geom_point(
    color = 'dodgerblue',
    size = 2,
    alpha = 0.3
  )+
  xlab("Dew Point °C Td") +
  ylab("PM2.5 concentration (ug/m^3)") +
  ggtitle("Dew Point VS PM2.5 concentration in Winter")+
  theme(text = element_text(size = 18))

options(repr.plot.width = 12, repr.plot.height = 6)
idx_s2 <- pm2.5_s$season=="Spring"
spring<- pm2.5_s[idx_s2,]
dewp_season_spring <- ggplot(spring, aes(x = DEWP, y = pm2.5)) +
  geom_point(
    color = 'chartreuse4',
    size = 2,
    alpha = 0.3
  )+
  xlab("Dew Point °C Td") +
  ylab("PM2.5 concentration (ug/m^3)") +
  ggtitle("Dew Point VS PM2.5 concentration in Spring")+
  theme(text = element_text(size = 18))



options(repr.plot.width = 12, repr.plot.height = 6)
idx_s3 <- pm2.5_s$season=="Summer"
summer<- pm2.5_s[idx_s3,]
dewp_season_summer <- ggplot(summer, aes(x = DEWP, y = pm2.5)) +
  geom_point(
    color = 'red',
    size = 2,
    alpha = 0.3
  )+
  xlab("Dew Point °C Td") +
  ylab("PM2.5 concentration (ug/m^3)") +
  ggtitle("Dew Point VS PM2.5 concentration in Summer")+
  theme(text = element_text(size = 18))

dewp_season_fall
dewp_season_winter
dewp_season_spring
dewp_season_summer

options(repr.plot.width = 12, repr.plot.height = 6)
idx_cv <- pm2.5_s$cbwd=="cv"
cv<- pm2.5_s[idx_cv,]
cv_1 <- ggplot(cv, aes(x = DEWP, y = pm2.5)) +
  geom_point(
    color = 'orange',
    size = 2,
    alpha = 0.3
  )+
  xlab("Dew Point °C Td") +
  ylab("PM2.5 concentration (ug/m^3)") +
  ggtitle("Dew Point VS PM2.5 concentration with calm and variable wind")+
  theme(text = element_text(size = 18))


options(repr.plot.width = 12, repr.plot.height = 6)
idx_ne <- pm2.5_s$cbwd=="NE"
ne<- pm2.5_s[idx_ne,]
ne_1 <- ggplot(ne, aes(x = DEWP, y = pm2.5)) +
  geom_point(
    color = 'dodgerblue',
    size = 2,
    alpha = 0.3
  )+
  xlab("Dew Point °C Td") +
  ylab("PM2.5 concentration (ug/m^3)") +
  ggtitle("Dew Point VS PM2.5 concentration with NE wind")+
  theme(text = element_text(size = 18))

options(repr.plot.width = 12, repr.plot.height = 6)
idx_nw <- pm2.5_s$cbwd=="NW"
nw<- pm2.5_s[idx_nw,]
nw_1 <- ggplot(nw, aes(x = DEWP, y = pm2.5)) +
  geom_point(
    color = 'chartreuse4',
    size = 2,
    alpha = 0.3
  )+
  xlab("Dew Point °C Td") +
  ylab("PM2.5 concentration (ug/m^3)") +
  ggtitle("Dew Point VS PM2.5 concentration with NW wind")+
  theme(text = element_text(size = 18))



options(repr.plot.width = 12, repr.plot.height = 6)
idx_se <- pm2.5_s$cbwd=="SE"
se<- pm2.5_s[idx_se,]
se_1 <- ggplot(se, aes(x = DEWP, y = pm2.5)) +
  geom_point(
    color = 'red',
    size = 2,
    alpha = 0.3
  )+
  xlab("Dew Point °C Td") +
  ylab("PM2.5 concentration (ug/m^3)") +
  ggtitle("Dew Point VS PM2.5 concentration with SE wind")+
  theme(text = element_text(size = 18))

cv_1
ne_1
nw_1
se_1

options(repr.plot.width = 8, repr.plot.height = 10)
wind_direction<-ggplot(pm2.5_s, aes(x=cbwd, y=pm2.5, color=cbwd)) +
  geom_boxplot() +
  xlab("Wind Direction") +
  ylab("PM2.5 concentration (ug/m^3)") +
  theme(text = element_text(size = 18))+
  # Box plot with mean points
  stat_summary(fun.y=mean, geom="point", shape=23, size=4)

wind_direction

# not important variables
options(repr.plot.width = 12, repr.plot.height = 6)
# season
snows <- ggplot(pm2.5_s, aes(x =Is , y = pm2.5)) +
  geom_point(
    aes(color = season, shape = season),
    size = 4,
    alpha = 0.5
  )+
  xlab("Cumulated hours of snow") +
  ylab("PM2.5 concentration (ug/m^3)") +
  theme(text = element_text(size = 18))

rains <- ggplot(pm2.5_s, aes(x =Ir , y = pm2.5)) +
  geom_point(
    aes(color = season, shape = season),
    size = 4,
    alpha = 0.5
  )+
  xlab("Cumulated hours of rain") +
  ylab("PM2.5 concentration (ug/m^3)") +
  theme(text = element_text(size = 18))

# wind direction
snow <- ggplot(pm2.5_s, aes(x =Is , y = pm2.5)) +
  geom_point(
    aes(color = cbwd, shape = cbwd),
    size = 4,
    alpha = 0.5
  )+
  xlab("Cumulated hours of snow") +
  ylab("PM2.5 concentration (ug/m^3)") +
  theme(text = element_text(size = 18))

rain <- ggplot(pm2.5_s, aes(x =Ir , y = pm2.5)) +
  geom_point(
    aes(color = cbwd, shape = cbwd),
    size = 4,
    alpha = 0.5
  )+
  xlab("Cumulated hours of rain") +
  ylab("PM2.5 concentration (ug/m^3)") +
  theme(text = element_text(size = 18))
snows
rains
snow
rain

# with one categorical variable : season
season_c<-lm(pm2.5~DEWP+TEMP+PRES+season, data=pm2.5_s)
season_s <- summary(season_c)
season_s
levels(pm2.5_s$cbwd)

#The contrasts() function returns the coding that R have used to create the dummy variables
contrasts(pm2.5_s$season)

#residual plot one categorical variable : season
options(repr.plot.width = 15, repr.plot.height = 8)
plot(x = fitted(season_c), 
     y = residuals(season_c),
     xlab = "Fitted values",
     ylab = "Residual",
     main = "Residual plot of one categorical variable : Season")
abline(h = 0, lty = 2)

# with one categorical variable : wind direction
wdirection_c<-lm(pm2.5~DEWP+TEMP+PRES+cbwd, data=pm2.5_s)
wdirection_s <- summary(wdirection_c)
wdirection_s

#residual plot one categorical variable : wind direction
options(repr.plot.width = 15, repr.plot.height = 8)
plot(x = fitted(wdirection_c), 
     y = residuals(wdirection_c),
     xlab = "Fitted values",
     ylab = "Residual",
     main = "Residual plot of one categorical variable : Wind Direction")
abline(h = 0, lty = 2)

# with two categorical variable : wind direction, season
two_c <-lm(pm2.5~DEWP+TEMP+PRES+cbwd+season, data=pm2.5_s)
two <- summary(two_c)
two

contrasts(pm2.5_s$season)
contrasts(pm2.5_s$cbwd)

#residual plot v5
options(repr.plot.width = 15, repr.plot.height = 8)
plot(x = fitted(two_c), 
     y = residuals(two_c),
     xlab = "Fitted values",
     ylab = "Residual",
     main = "Residual plot of two categorical variables: wind direction and season")
abline(h = 0, lty = 2)

#covariates residual plots for the best model
options(repr.plot.width = 15, repr.plot.height = 8)
dew<-plot(x = pm2.5_s$DEWP, 
     y = residuals(two_c),
     xlab = "Dew point",
     ylab = "Residual",
     main = "Residuals vs Dew point")
abline(h = 0, lty = 2)

temp<-plot(x = pm2.5_s$TEMP, 
     y = residuals(two_c),
     xlab = "Temperature",
     ylab = "Residual",
     main = "Residuals vs Temperature")
abline(h = 0, lty = 2)

pres<-plot(x = pm2.5_s$PRES, 
     y = residuals(two_c),
     xlab = "Pressure",
     ylab = "Residual",
     main = "Residuals vs Pressure")
abline(h = 0, lty = 2)
