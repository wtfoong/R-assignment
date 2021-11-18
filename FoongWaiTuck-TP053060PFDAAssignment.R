#Foong Wai Tuck
#TP053060

#data import
weather = read.csv("C:\\Users\\Dancing Book\\Desktop\\R language\\assignment\\weather.csv",header=TRUE)
weather

#install libraries
install.packages("ggplot2")
install.packages("skimr")
install.packages("gridExtra")

#libraries

suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(skimr))
suppressPackageStartupMessages(library(corrplot))
library(dplyr)
library(plotrix)
library(gridExtra)

# shows more detailed summary of the dataset with built in histogram
skim(weather)

#to see number of row and columns
dim(weather)
nrow(weather)
ncol(weather)

#to report on the list structure of the dataset
str(weather)

#to see all names of the columns
names(weather)

# for first 6 lines and first 10 lines
head(weather)
head(weather,10)

#for last 6 lines and last 3 lines
tail(weather)
tail(weather,3)

# to view the whole data frame.
View(weather)

# how data is stored
class(weather)

# Number of missing values of each variable of this data set
numberOfNA<-function(){
  colSums(is.na(weather))
}
numberOfNA()

#categorize by windGustDir
windGustDir = factor(weather$WindGustDir)  #to categorise the windGustDir
windGustDir


######test Area#######

x<-inspect_imb(weather)
show_plot(x)
inspect_cat(weather, show_plot(TRUE))

subset(weather, RISK_MM >1 ) # can get any number of sample
min(weather$Sunshine, na.rm = TRUE)

all.equal(weather$Humidity3pm > 44.5, weather$RainToday == "No")

all.equal(weather$Rainfall > 1, weather$RainToday == "Yes")

ggplot(weather, aes(x=Cloud9am, y=WindSpeed9am)) + 
  geom_point(aes(shape = factor(RainToday),colour=factor(RainToday)))+
  ggtitle("Cloud9am vs Cloud3pm based on RainToday")

ggplot(weather, aes(Pressure3pm, Humidity3pm, colour = RainTomorrow)) +
  geom_line()


##########################

# Question 1: the relationship between Rain tmr and RISK_MM, and rain fall with RainTdy.

#analysis 1:if RISK_MM is larger than 1 then RainTomorrow is yes
#this source code is from  (Garziano, 2017)
all.equal(weather$RISK_MM > 1, weather$RainTomorrow == "Yes")

subset(weather, RISK_MM >1 )[10:20,]

c=sample_n(weather,5)
c=arrange(c,RISK_MM)
for ( i in 1:5) {
 if (c[i,]$RISK_MM>1) {
   print(paste(c[i,]$RISK_MM, c[i,]$RainTomorrow))
 }
}

#analysis 2: if rain fall is >1 then rain today = yes
all.equal(weather$Rainfall > 1, weather$RainToday == "Yes")

s=sample_n(weather,5)
i=1
while (i<6) {
  print(ifelse(s[i,]$Rainfall>1,paste(s[i,]$Rainfall,"Rain Today",s[i,]$RainToday),paste(s[i,]$Rainfall," Rain Today",s[i,]$RainToday)))
  i=i+1
}

s=sample_n(weather,5)
i=1
repeat{
  print(ifelse(s[i,]$Rainfall>1,paste(s[i,]$Rainfall,"Rain Today",s[i,]$RainToday),paste(s[i,]$Rainfall," Rain Today",s[i,]$RainToday)))
  i=i+1
  if (i>5) {
    break
  }
}


# Question 2: What is the relationship between raining today, cloud 9am and cloud 3 pm

#analysis 1: the percentage of raining today in pie chart
factor(RainToday)
rainTdyYes=nrow(weather[weather$RainToday=="Yes",])
rainTdyYes
rainTdyNo=nrow(weather[weather$RainToday=="No",])
rainTdyNo
slices=c(rainTdyYes,rainTdyNo)
lbls=c("yes","no")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add pct to labels
lbls <- paste(lbls,"%",sep="")
pie(slices,lbls,radius = 1,main="PERCENTAGE_OF_YES_NO_FOR_RAIN_TODAY",clockwise = TRUE)


# analysis 2 :  bar chart for cloud 9 am and cloud 3 pm 
x=summarise(weather,mean(Cloud9am))
round(x[1,],digit=2)
ggplot(weather, aes(Cloud9am)) +
  geom_bar() +
  scale_x_binned()+ 
  geom_vline(aes(xintercept=x[1,]),color="blue", linetype="dashed", size=1)

x=summarise(weather,mean(Cloud3pm))
round(x[1,],digit=2)
ggplot(weather, aes(Cloud3pm)) +
  geom_bar() +
  scale_x_binned()+ 
  geom_vline(aes(xintercept=x[1,]),color="blue", linetype="dashed", size=1)


#analysis 3: point graph for relationship between this 3
ggplot(weather, aes(x=Cloud9am, y=Cloud3pm)) + 
  geom_point(aes(shape = factor(RainToday),colour=factor(RainToday)))+
  ggtitle("Cloud9am vs Cloud3pm based on RainToday")

#analysis 4: histogram for relationship between Cloud9am and rain today
ggplot(weather, aes(Cloud9am, fill = RainToday)) +
  geom_histogram(binwidth = 0.5)

#analysis 5: histogram chart for relationship between Cloud3pm and rain today

ggplot(weather, aes(Cloud3pm, fill = RainToday)) +
  geom_histogram(binwidth = 0.5)


#Question 3: THe relationship between humidity 3 pm and temperature 3pm with rain tomorrow

#analysis 1: the percentage of raining tomorrow in 3D pie chart
rainTmrYes=nrow(weather[weather$RainTomorrow=="Yes",])
rainTmrYes
rainTmrNo=nrow(weather[weather$RainTomorrow=="No",])
rainTmrNo
slices=c(rainTmrYes,rainTmrNo)
lbls=c("yes","no")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add pct to labels
lbls <- paste(lbls,"%",sep="")
pie3D(slices,labels = lbls,explode = 0.3,main = "PERCENTAGE_OF_YES_NO_FOR_RAIN_TOMORROW")

# analysis 2 :  bar chart for humidity 3 pm and temperature 3 pm
x=summarise(weather,mean(Humidity3pm))
round(x[1,],digit=2)
ggplot(weather, aes(Humidity3pm)) +
  geom_bar() +
  scale_x_binned()+ 
  geom_vline(aes(xintercept=x[1,]),color="blue", linetype="dashed", size=1)

x=summarise(weather,mean(Temp3pm))
round(x[1,],digit=2)
ggplot(weather, aes(Temp3pm)) +
  geom_bar() +
  scale_x_binned()+ 
  geom_vline(aes(xintercept=x[1,]),color="blue", linetype="dashed", size=1)


#analysis 3: density plot for temp 3pm and rain tmr

ggplot(weather, aes(x=Temp3pm,color=RainTomorrow))+
  geom_density()

#analysis 4: density plot for humidity 3pm and rain tmr
ggplot(weather, aes(x=Humidity3pm))+
  geom_density(aes(color=RainTomorrow))

#analysis 5 : Point chart for relationship between this 3
ggplot(weather, aes(x=Temp3pm, y=Humidity3pm)) + 
  geom_point(aes(shape = factor(RainTomorrow),colour=factor(RainTomorrow)))+
  ggtitle("Humidity3pm vs Temp3pm based on RainTomorrow")+
  stat_smooth(se = FALSE, method = lm)

#analysis 6: Point chart for the relation with rain tmr == Yes
filteredData=filter(weather,RainTomorrow=="Yes")
ggplot(filteredData, aes(x=Temp3pm, y=Humidity3pm)) + 
  geom_point()+
  ggtitle("Humidity3pm vs Temp3pm based on RainTomorrow== Yes")+
  stat_smooth(se = FALSE, method = lm)


#Question 4: THe relationship between humidity 3 pm and pressure 3pm with rain tomorrow


# analysis 1 : bar chart for pressure 3 pm
x=summarise(weather,mean(Pressure3pm))
round(x[1,],digit=2)
ggplot(weather, aes(Pressure3pm)) +
  geom_bar() +
  scale_x_binned()+
  geom_vline(aes(xintercept=x[1,]),color="blue", linetype="dashed", size=1)


#analysis 2: density plot for pressure 3pm and rain tmr
ggplot(weather, aes(x=Pressure3pm))+
  geom_density(aes(color=RainTomorrow))

#analysis 3 : point chart for relationship between this 3
ggplot(weather, aes(x=Pressure3pm, y=Humidity3pm, colour = RainTomorrow)) + 
  geom_point()+
  ggtitle("Humidity3pm vs Pressure3pm based on RainTomorrow")+
  stat_smooth(se = FALSE, method = lm)

#Question 5 THe relationship between humidity 9 am and pressure 9am with rain tdy

#analysis 1: density plot for humidity 9am and rain tdy
ggplot(weather, aes(x=Humidity9am))+
  geom_density(aes(color=RainToday))

#analysis 2: density plot for pressure 9am and rain tdy
ggplot(weather, aes(x=Pressure9am))+
  geom_density(aes(color=RainToday))

#analysis 3 : point chart for relationship between this 3
ggplot(weather, aes(x=Pressure9am, y=Humidity9am, colour = RainToday)) + 
  geom_point()+
  ggtitle("Humidity9am vs Pressure9am based on RainToday")+
  stat_smooth(se = FALSE, method = lm)

#Question 6 The relationship between humidity 3pm, rainfall and rain tdy

#analysis 1: point graph for the relationship of these 3
ggplot(weather, aes(x=Humidity3pm, y=Rainfall , colour = RainToday)) + 
  geom_point()+
  ggtitle("Humidity3pm vs Rainfall based on RainToday")+
  stat_smooth(se = FALSE, method = lm)

#Question 7: The relationship between humidity and rain tdy

#analysis 1: box plot graph for the relationship of humidity 9am, humidity 3pm and rain tdy
h9Am=ggplot(weather,aes(y=Humidity9am, X=RainToday, color=RainToday))+
      geom_boxplot() 
h3Pm=ggplot(weather,aes(y=Humidity3pm, X=RainToday, color=RainToday))+
      geom_boxplot() 

marrangeGrob(list(h9Am, h3Pm),nrow=1,ncol = 2,
             top = "Relationship of humidity 9am, humidity 3pm and rain tdy")

#Question 8: The relationship between sunshine and rain tdy

#analysis 1: density plot for sunshine and rain tdy
ggplot(weather, aes(x=Sunshine))+
  geom_density(aes(color=RainToday))

#Question 9: The relationship between Humidity9am and Evaporation and RainToday

#analysis 1: point graph for this 3 relation
ggplot(weather, aes(x=Humidity9am, y=Evaporation , colour = RainToday)) + 
  geom_point()+
  ggtitle("Humidity9am vs Evaporation based on RainToday")+
  stat_smooth(se = FALSE, method = lm)

#Question 10: The relationship between Humidity3pm and Evaporation and RainTomorrow

#analysis 1: point graph for this 3 relation
ggplot(weather, aes(x=Humidity3pm, y=Evaporation , colour = RainTomorrow)) + 
  geom_point()+
  ggtitle("Humidity3pm vs Evaporation based on RainTomorrow")+
  stat_smooth(se = FALSE, method = lm)


#Question 11: The relationship between Cloud3pm and Evaporation and RainTomorrow

#analysis 1: point graph for this 3 relation
ggplot(weather, aes(x=Cloud3pm, y=Evaporation , colour = RainTomorrow)) + 
  geom_point()+
  ggtitle("Cloud3pm vs Evaporation based on RainTomorrow")+
  stat_smooth(se = FALSE, method = lm)

#Question 12: The relationship of temp 9 am and humidity9am

#analysis 1: line graph for this relation
p<-select(weather,c("Temp9am","Humidity9am"))
ggplot(p, aes(x=Temp9am,y=Humidity9am)) + 
  geom_line()+
  ggtitle("Temp9am with Humidity9am")+
  stat_smooth(se = FALSE, method = lm)

#Question 13: The trend of temperatures in the dataframe
#analysis 1: line graph for trend of temp9am and temp 3pm
Index=1:nrow(weather)
ggplot(weather, aes(x=Index)) + 
  geom_line(aes(y=Temp9am,colour = "Temp9am"))+
  geom_line(aes(y=Temp3pm,colour = "Temp3pm"))+
  ggtitle("trend of Temp9am and Temp3pm")+
  labs(y="Temperature (°C)")+
  scale_color_discrete(name = "Temp9am and Temp3pm")

#Question 14: The relationship of rain today to average temperature
#analysis 1: box plot for rain today and average temp
mutateData=mutate(weather,AvgTemp=round((MinTemp+MaxTemp+Temp9am+Temp3pm)/4,1))
ggplot(mutateData,aes(y=AvgTemp, X=RainToday, color=RainToday))+
  geom_boxplot() 
 
#Question 15: The relationship of RainTomorrow to average temperature
#analysis 1: density plot for rain today and average temp
ggplot(mutateData, aes(x=AvgTemp))+
  geom_density(aes(color=RainTomorrow))

#Question 16: correlation relationship among quantitative variables of the data frame
#analysis 1: correlogram 
#this source code is from  (Garziano, 2017)
weather_data=subset(weather, select = -c(WindDir9am, WindDir3pm,WindGustDir,RainToday))# remove unneeded categories
weather_data=weather_data[complete.cases(weather_data),]  #to remove all data with null
nrow(weather_data)
factor_vars <- names(which(sapply(weather_data, class) == "factor"))
numeric_vars <- setdiff(colnames(weather_data), factor_vars)
numeric_vars <- setdiff(numeric_vars, "RainTomorrow")
numeric_vars
numeric_vars_mat <- as.matrix(weather_data[, numeric_vars, drop=FALSE])
numeric_vars_cor <- cor(numeric_vars_mat)
corrplot(numeric_vars_cor)
