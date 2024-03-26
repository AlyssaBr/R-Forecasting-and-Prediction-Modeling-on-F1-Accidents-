# Race Winners Code
# Alyssa Brunen
# Hult International Business School 
# New York Summer 1 2023
# Forecasting and Predicting the future using data class

# Purpose: 
# Forecasting what creates a Race Winner in Formula 1 
# Why: Team and Driver Selection for my Fanatsy League Team 

########################################################################################

###################   IMPORTANT MESSAGE IF RUNNING CODE ################################

#### EACH SECTION HAVE TO RUN SEPERATLY #################################################

#### This is because a seperate DF was created to filter active drivers ONLY ############
#### When normalizing data that is added to this filtered df        #####################
#### it gives error message as there are 868 obsv. in original DF vs 20 in filtered #####
#### After that, continue as usual ######################################################

###############   END OF IMPORTANT MESSAGE ##############################################

#########################################################################################

#bringing in all libraries/ installing packages 
#install.packages('corrplot') 
library(rpart)
library(ggplot2)
library(lattice)
library(rpart.plot)
library(caret)
library(tidyverse)
library(tseries)
library(rugarch)
library(dplyr)
library(corrplot)

#Loading the dataset, attached to submission 

F1_df <-read.csv("/Users/alyssabrunen/Desktop/Hult Docs/Summer 1 NY/Forecasting/Individual submission/F1DriversDataset.csv")

# Massaging data

# Dataframe only with active drivers
F1_df$binary <- gsub("True", 1, F1_df$Active)
F1_df$binary <- gsub("False", 0, F1_df$binary)
F1_df$binary <- as.numeric(F1_df$binary)



for(i in 1:ncol(F1_df)){
  my_min<- try(min(F1_df[,i],na.rm=TRUE))
  my_max<- try(max(F1_df[,i],na.rm=TRUE))
  my_mean<- try(mean(F1_df[,i],na.rm=TRUE))
  my_sd<- try(sd(F1_df[,i],na.rm=TRUE))
  print(c(my_min,my_max,my_mean,my_sd))
}

#normalizing data 

min_max <- function(x){
  normalize <- (x-min(x))/(max(x)-min(x))
  return(normalize)
}

F1_df$Race_Entries_norm <- min_max(F1_df$Race_Entries)
F1_df$Race_Starts_norm <- min_max(F1_df$Race_Starts)
F1_df$Pole_Positions_norm <- min_max(F1_df$Pole_Positions)
F1_df$Race_Wins_norm <- min_max(F1_df$Race_Wins)
F1_df$Points_norm <- min_max(F1_df$Points)
F1_df$Points_per_Entry_norm <- min_max(F1_df$Points_Per_Entry)
F1_df$Pole_Rate_norm <- min_max(F1_df$Pole_Rate)
F1_df$Podium_Rate_norm <- min_max(F1_df$Podium_Rate)
F1_df$Podiums_norm <- min_max(F1_df$Podiums)
F1_df$FastLap_Rate_norm <- min_max(F1_df$FastLap_Rate)
F1_df$Fastest_Laps_norm <- min_max(F1_df$Fastest_Laps)
F1_df$Win_Rate_norm <- min_max(F1_df$Win_Rate)

#
cor_matrix<- cor(F1_df[,c( "Race_Entries","Race_Starts","Pole_Positions", 
             "Race_Wins","Points","Points_Per_Entry","Pole_Rate", "Podium_Rate", 
             "Podiums", "FastLap_Rate", "Fastest_Laps", "Win_Rate")])

corrplot(cor_matrix, method = "circle", tl.col = "black", tl.srt = 45)


################## From here run sections individually ###################


# filter out all non active drivers in a new df
filtered_df <- subset(F1_df, binary != 0)

# Output the filtered data frame
print(filtered_df) 

# adding normalized data to the active driver only dataframe 
filtered_df$Race_Entries_norm <- min_max(F1_df$Race_Entries)
filtered_df$Race_Starts_norm <- min_max(F1_df$Race_Starts)
filtered_df$Pole_Positions_norm <- min_max(F1_df$Pole_Positions)
filtered_df$Race_Wins_norm <- min_max(F1_df$Race_Wins)
filtered_df$Points_norm <- min_max(F1_df$Points)
filtered_df$Points_per_Entry_norm <- min_max(F1_df$Points_Per_Entry)
filtered_df$Pole_Rate_norm <- min_max(F1_df$Pole_Rate)
filtered_df$Podium_Rate_norm <- min_max(F1_df$Podium_Rate)
filtered_df$Podiums_norm <- min_max(F1_df$Podiums)
filtered_df$FastLap_Rate_norm <- min_max(F1_df$FastLap_Rate)
filtered_df$Fastest_Laps_norm <- min_max(F1_df$Fastest_Laps)
filtered_df$Win_Rate_norm <- min_max(F1_df$Win_Rate)


############## Continue still running each section individually , ignore error#######

#change Race Winners to 1 and 0 (1 True, 0 False)
filtered_df$Race_Winners <- ifelse(filtered_df$Race_Wins_norm > 0, 1, 0)
F1_df$Race_Winners <- ifelse(F1_df$Race_Wins_norm > 0, 1, 0)

############################ NOW ALL CAN BE RUN AGAIN ###############################



# building a decision tree

my_tree <- rpart(Race_Winners ~ Pole_Positions_norm+ 
                   Pole_Rate_norm+Podium_Rate_norm+Fastest_Laps_norm+FastLap_Rate_norm, 
                 data=F1_df, method="class")

#Plot the decision tree 
rpart.plot(my_tree, box.palette = "Blues")



#Testing Accuracy of tree
my_df_tree_predict <- predict(my_tree, F1_df, type="prob")

confusionMatrix(data = as.factor(as.numeric(my_df_tree_predict[,2]>0.5)) ,
                reference= as.factor(as.numeric(F1_df$Race_Winners)))

#Confusion Matrix and Statistics Output 

#               Reference
#Prediction      0   1
#0              749  23
#1               6  90

#Accuracy : 0.9666          
#95% CI : (0.9524, 0.9775)
#No Information Rate : 0.8698          
#P-Value [Acc > NIR] : < 2.2e-16     


# Count the occurrences of the number 1 in the 'Race_Winners' variable of the dataframe
count <- sum(F1_df$Race_Winners == 1, na.rm = TRUE)

# Print the count
print(count)


###################################################
### Following not used in Report #################
##################################################

## Trial of forecasting 3 variables with higer influence on Race Wins
## Podium_Rate, Fastest_Laps and Pole_Rate

#Group by variable by year
Podium_Rate <- F1_df %>%
  group_by(Decade) %>%
  summarize(avg_Podium_Rate = mean(Podium_Rate, na.rm = TRUE))

Fastest_Laps <- F1_df%>%
  group_by(Decade) %>%
  summarize(avg_Fastest_Laps = mean(Fastest_Laps, na.rm = TRUE))

Pole_Rate <- F1_df %>%
  group_by(Decade) %>%
  summarize(avg_Pole_Rate = mean(Pole_Rate, na.rm = TRUE))

# Plot each Data 
ggplot(data=Podium_Rate)+
  geom_line(aes(x=Decade, y=avg_Podium_Rate))

ggplot(data=Fastest_Laps)+
  geom_line(aes(x=Decade, y=avg_Fastest_Laps))

ggplot(data=Pole_Rate)+
  geom_line(aes(x=Decade, y=avg_Pole_Rate))


#adf test for each variable 
adf.test(Podium_Rate$avg_Podium_Rate)

adf.test(Fastest_Laps$avg_Fastest_Laps)

adf.test(Pole_Rate$avg_Pole_Rate)
# All variables hat p-values above .05, we accept the null hypothesis
#time series is non-stationary
#time-dependent structure & does not have constant variance over time

# Decomposition of the non-stationary data
ts_Podi <- ts(Podium_Rate[,c("Decade", "avg_Podium_Rate")],frequency = 5, start=c(1950))
dec_Podi <- decompose(ts_Podi)
plot(dec_Podi)


ts_FL <- ts(Fastest_Laps[,c("Decade", "avg_Fastest_Laps")], frequency = 5, start=c(1950))
dec_FL <- decompose(ts_FL)
plot(dec_FL)

ts_PL <- ts(Pole_Rate[,c("Decade", "avg_Pole_Rate")], frequency = 5, start=c(1950))
dec_PL <- decompose(ts_PL)
plot(dec_PL)

#####
# All three variables were stationary 

#acf and pacf
acf(Podium_Rate$avg_Podium_Rate)

acf(Fastest_Laps$avg_Fastest_Laps)

acf(Pole_Rate$avg_Pole_Rate)

pacf(Podium_Rate$avg_Podium_Rate)

pacf(Fastest_Laps$avg_Fastest_Laps)

pacf(Pole_Rate$avg_Pole_Rate)


