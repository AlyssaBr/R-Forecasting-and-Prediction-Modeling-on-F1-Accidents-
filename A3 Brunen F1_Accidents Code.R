# Accidents Code
# Alyssa Brunen
# Hult International Business School 
# New York Summer 1 2023
# Forecasting and Predicting the future using data class



# Purpose: 
# Predicting the next stage of a Formula 1 Session where a fatal accident will occur
# Why: Safety for drivers, safety regulation changes if necessary

#################################

#Submission Code: 


#Load Libraries 
library(rpart)
library(ggplot2)
library(lattice)
library(rpart.plot)
library(caret)
library(tidyverse)
library(tseries)
library(rugarch)
library(dplyr)

#Load DataFrame that is being used 
Acci_df <-read.csv("/Users/alyssabrunen/Desktop/Hult Docs/Summer 1 NY/Forecasting/Individual submission/fatal_accidents_drivers.csv")

# Changing ages from integer into numeric
Acci_df$Age<- as.numeric(Acci_df$Age)
# Chaning dates from character into date format
Acci_df$Date.Of.Accident<- as.Date(Acci_df$Date.Of.Accident, format= "%m/%d/%Y")

################################################################
##################   Not Used in Report    #####################
################################################################
#Personal Trial: Seeing what ages that had fatal accidents were 
Age <- Acci_df %>%
  group_by(Date.Of.Accident) %>%
  summarize(avg_Age = mean(Age, na.rm = TRUE))
#plotting the data
ggplot(data=Age)+
  geom_line(aes(x=Date.Of.Accident, y=avg_Age))
# Personal trial if p value below 0.05 (True)
adf.test(Age$avg_Age)

acf(Age$avg_Age)

pacf(Age$avg_Age)
# Predicting the Age of the next driver with a fatal accident 
Age_arima <- arima(Age$avg_Age, 
                       order=c(0,1,0)) 
predict(Age_arima, n.ahead =5) 

# Result: 
#$pred
#Time Series:
#Start = 50 
#End = 54 
#Frequency = 1 
#[1] 25 25 25 25 25

#$se
#Time Series:
#Start = 50 
#End = 54 
#Frequency = 1 
#[1]  9.506577 13.444330 16.465874 19.013153 21.257352

###########################################################
##################  Used in Report  #######################
##########   Car (Team) not used in Report  ###############
###########################################################
#Setting Car (Team) and Session type as factors and numeric in different variables
Acci_df$Car.fac <- as.factor(Acci_df$Car)
Acci_df$Car.fac.x <- as.numeric(Acci_df$Car.fac)
Acci_df$Session.fac <- as.factor(Acci_df$Session)
Acci_df$Session.fac.x <- as.numeric(Acci_df$Session.fac)

#Printing the level names for both Car (Team) and also Session Type 
print(Acci_df$Session.fac)
print(Acci_df$Car.fac)

#Creating Correlation Matrix 
cor_matrix <- cor(Acci_df[,c("Age", "Car.fac.x", "Session.fac.x"
)])
corrplot(cor_matrix, method = "circle", tl.col = "black", tl.srt = 45)


##############Prediction for Session 
#Grouping session type by date
Session.fac.x <- Acci_df %>%
  group_by(Date.Of.Accident) %>%
  summarize(avg_Session = mean(Session.fac.x, na.rm = TRUE))

#Plotting session type by date 
ggplot(data=Session.fac.x)+
  geom_line(aes(x=Date.Of.Accident, y=avg_Session))

#ADF test to see if Data is Stationary for Session Type 
adf.test(Session.fac.x$avg_Session)
#p value below 0.05 = Data stationary 
 
# ACF and PACF for Session  
acf(Session.fac.x$avg_Session)

pacf(Session.fac.x$avg_Session)

# Creating Time Series & Plot 
ts_Session <- ts(Session.fac.x[,c("Date.Of.Accident", "avg_Session")],frequency = 5, start=c(1950))
dec_Session <- decompose(ts_Session)
plot(dec_Session)

#Creating ARMA Model for Stationary Data 

Session_arma <- arma(Session.fac.x$avg_Session, order=c(0,0)) 
summary(Session_arma)
#however, to use the predict() function we need to use the arima function 
# we use a 0 for i as a forecast
Session_arima <- arima(Session.fac.x$avg_Session, 
                       order=c(0,1,0)) 
predict(Session_arima, n.ahead =5) 

#ARMA Model output 
#$pred
#Time Series:
#Start = 50 
#End = 54 
#Frequency = 1 
#[1] 5 5 5 5 5

#$se
#Time Series:
#Start = 50 
#End = 54 
#Frequency = 1 
#[1] 2.222049 3.142451 3.848701 4.444097 4.968652


################################################
###### Prediction for Car (Team) ###############
#############  Not used in Report ##############
# Doing the same for car (Team)
Car.fac.x <- Acci_df %>%
  group_by(Date.Of.Accident) %>%
  summarize(avg_Car = mean(Car.fac.x, na.rm = TRUE))

ggplot(data=Car.fac.x)+
  geom_line(aes(x=Date.Of.Accident, y=avg_Car))

adf.test(Car.fac.x$avg_Car)
# Adf test p value above 0.05 = Non Stationary data

# Creating Time series for Car(Team)
ts_Car <- ts(Car.fac.x[,c("Date.Of.Accident", "avg_Car")],frequency = 5, start=c(1950))
dec_Car <- decompose(ts_Car)
plot(dec_Car)

#ACF and PACF for Car (team)
acf(Car.fac.x$avg_Car)

pacf(Car.fac.x$avg_Car)

#ARIMA forecasting for Cars (teams)
Car_arima <- arima(Car.fac.x$avg_Car, 
                           order=c(1,1,2)) 
predict(Car_arima, n.ahead =5) 

#ARIMA Model Output for Cars (Teams)
#$pred
#Time Series:
#Start = 50 
#End = 54 
#Frequency = 1 
#[1] 14.17018 13.68226 13.42870 13.29692 13.22844

#$se
#Time Series:
#Start = 50 
#End = 54 
#Frequency = 1 
#[1] 6.516498 6.874050 6.996722 7.047257 7.072723

### Unuseful data for Cars(Teams) as the predicted teams do not exsist anymore 
#########   Therefore not used in the report ###############
############################################################


########## Data for visualization in Report 
# Counting Race Fatalities
count.Race.Fat <- sum(Acci_df$Session.fac.x == 5, na.rm = TRUE)
# Print the count
print(count.Race.Fat)


#### Visualization of Fatalities per Session Type Code 
# Count the frequency of each factor level in session type 
frequency <- table(Acci_df$Session.fac.x)

# create a new dataframe for that 
df <- data.frame(Number = as.numeric(names(frequency)),
                 Frequency = as.numeric(frequency))

# Create the bar plot
ggplot(df, aes(x = Number, y = Frequency)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Session Type", y = "Frequency", title = "Frequency of Incidents per Session Type") +
  theme_minimal()
