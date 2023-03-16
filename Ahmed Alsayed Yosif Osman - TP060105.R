# AHMED ALSAYED YOSIF OSMAN
# TP060105
#~~~~~~~~~~~~~~~~~~~~~~~~~


## Data set Preparation;
#~~~~~~~~~~~~~~~~~~~~~~
# Importing the Data set:
library(reader)
library(readr)
weather <- read_csv("B:/One drive/OneDrive - Asia Pacific University/YEAR 2/SEM 1/PFDA/weather.csv", 
                    col_types = cols(Date = col_date(format = "%m/%d/%Y")))

# Viewing the Imported Data set:
View(weather)

# Identifying the Data Type assigned to the Data Set:
class(weather)


## Preprocessing Transformation & Data Clean up:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Installing Needed Packages
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("plyr")
install.packages("stringr")
install.packages("corrplot")
install.packages("data.table")
install.packages("ggbeeswarm")
install.packages("UsingR")
install.packages("lubridate")
install.packages("circular")
install.packages("IRdisplay")
install.packages("treemap")
install.packages('rstantools')
install.packages("skimr")
install.packages("devtools")
install.packages("psych")
install.packages("hrbrthemes")
install.packages("ggridges")
install.packages("plotly")
library(plotly)
library(ggridges)
library(hrbrthemes)
library(viridis)
library(psych)
library(devtools)
library(skimr)
library(rstantools)
library(tidyr)
library(dplyr)
library(ggplot2)
library(plyr)
library(ggbeeswarm)
library(lubridate)
library(UsingR)
library(stringr)
library(corrplot)
library(data.table)
library(circular)
library(tibble)
library(tidyverse)
library(IRdisplay)
library(crayon)
library(treemap)
library(magrittr)


# Identifying the number of "NA" values
sapply(weather, function(i) sum(is.na(i)))

# Replacing "NA" Values with 0
weather$Sunshine[is.na(weather$Sunshine)] = 0
weather$WindGustSpeed[is.na(weather$WindGustSpeed)] = 0
weather$WindSpeed9am[is.na(weather$WindSpeed9am)] = 0
weather$WindGustDir[is.na(weather$WindGustDir)] = 0
weather$WindDir9am[is.na(weather$WindDir9am)] = 0
weather$WindDir3pm[is.na(weather$WindDir3pm)] = 0
sapply(weather, function(i) sum(is.na(i)))


# Identifying and Removing Outliers
boxplot(weather$MinTemp, plot=FALSE)$out
boxplot(weather$MaxTemp, plot=FALSE)$out
boxplot(weather$Rainfall, plot=FALSE)$out
boxplot(weather$Evaporation, plot=FALSE)$out
boxplot(weather$Sunshine, plot=FALSE)$out
boxplot(weather$WindGustSpeed, plot=FALSE)$out
boxplot(weather$WindSpeed9am, plot=FALSE)$out
boxplot(weather$WindSpeed3pm, plot=FALSE)$out
boxplot(weather$Humidity9am, plot=FALSE)$out
boxplot(weather$Humidity3pm, plot=FALSE)$out
boxplot(weather$Pressure9am, plot=FALSE)$out
boxplot(weather$Pressure3pm, plot=FALSE)$out
boxplot(weather$Cloud9am, plot=FALSE)$out
boxplot(weather$Cloud3pm, plot=FALSE)$out
boxplot(weather$Temp9am, plot=FALSE)$out
boxplot(weather$Temp3pm , plot=FALSE)$out
boxplot(weather$RISK_MM , plot=FALSE)$out

# Saving the outlier values into a vector
outliers = c(boxplot(weather$WindGustSpeed, plot=FALSE)$out,boxplot(weather$Rainfall, plot=FALSE)$out,
             boxplot(weather$Evaporation, plot=FALSE)$out,boxplot(weather$WindSpeed9am,plot=FALSE)$out,
             boxplot(weather$WindSpeed3pm, plot=FALSE)$out,boxplot(weather$Humidity9am, plot=FALSE)$out,
             boxplot(weather$Humidity3pm, plot=FALSE)$out,boxplot(weather$Pressure9am, plot=FALSE)$out,
             boxplot(weather$Pressure3pm, plot=FALSE)$out,boxplot(weather$RISK_MM , plot=FALSE)$out)
print(outliers)


# Wind Gust speed outliers plot as an example
boxplot(weather$WindGustSpeed ,xlab = "Wind Gust Speed Outliers"
        ,ylab = "Wind Gust Speed")$out

# Same Example as above but using "ggplot"
ggplot(weather, mapping = aes(x=0 , y = WindGustSpeed, fill = WindGustSpeed))+
  geom_boxplot(outlier.colour = "red",outlier.shape = 5, outlier.size = 4)+
  labs(title = "Wind Gust Speed Outliers")

# Removal of affected rows
weather = weather[-which(weather$WindGustSpeed %in% outliers),]



## Data Exploration:
#~~~~~~~~~~~~~~~~~~
# Show a Summary of the Data Set Elements:
summary(weather)

# Skim through the data:
skim(weather)

# Visualizing the Data set:
devtools::install_github("ropensci/visdat")
library(visdat)
vis_dat(weather)


# Correlation plot :
weatherNumeric = subset(weather, select = -c(WindGustDir , WindDir9am, WindDir3pm ,
                                              RainToday ,  RainTomorrow ,Date  ))
weatherCor = cor(weatherNumeric)
corrplot(weatherCor, method = "circle")


## Scenarios / Questions:
#~~~~~~~~~~~~~~~~~~~~~~~~
# Question 1: Scenario 1: Farmers seeking help in deciding 
#       the best times to start planting cool and cold season crops.

# Analysis 1.1: Finding the cool season (below 20 °C):
maxtemp = subset(weather, MaxTemp<=20)
count = length(maxtemp$MaxTemp)
print(count)
cat("Cool season days are", count , "days")

# Data Visualization 1.1:
coolSeason = ggplot(maxtemp, aes(x= Date, y = MaxTemp))+
  geom_line(aes(color = MaxTemp) , size = 0.8)+
  scale_x_date(date_labels = ("%b ' %y"), date_break = "1 month")+
  scale_y_continuous(breaks =  c(5, 10 ,15 , 20, 25))+
  ggtitle("Cool season Months during the year")+
  labs(x="Date", y= "Temperature")
coolSeason

# Analysis 1.2: Finding the cold season (below 10 °C):
maxtempcold = subset(weather, MaxTemp<=10)
countL = length(maxtempcold$MaxTemp)
print(countL)
cat("Cold season days are", countL , "days")

# Data Visualization 1.2:
coldSeason = ggplot(maxtempcold, aes(x= Date, y = MaxTemp))+
  geom_line(aes(color = MaxTemp) , size = 0.8)+
  scale_x_date(date_labels = ("%b ' %y"), date_break = "1 month")+
  scale_y_continuous(breaks =  c(5, 10 ,15 , 20, 25))+
  ggtitle("Cold season Months during the year")+
  labs(x="Date", y= "Temperature")
coldSeason

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Question 2: A sport Event planner needs to know 
#           the best time to hold different types of sport event:

# Analysis 2.1: Finding the relation between the general wind gust speed and
#               the wind gust direction:
# Data Visualization 2.1:
ggplot(weather, aes(x=WindGustSpeed ,color =WindGustDir )) + 
  geom_histogram(aes(y=..density..), bins=30 ,colour="yellow", fill="black")+
  geom_density(alpha=.1,size=1, fill="#FFFF00") 

# Analysis 2.2: Finding the relation between the wind speed 
#     and the wind direction in the morning:
# Data Visualization 2.2:
ggplot(data = weather, mapping = aes(x = WindDir9am, y = WindSpeed9am)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(alpha = 0.3, color = "blue")+
  labs(title = "Wind Speed and Direction in the Morning")


# Analysis 2.3: Finding the relation between the wind speed 
#     and the wind direction in the afternoon:
# Data Visualization 2.3:
ggplot(weather, aes(x=WindDir3pm ,y =WindSpeed3pm )) + 
  geom_jitter(alpha = 0.9,  size = 3.3 ,aes(color = WindDir3pm))+
  labs(title = "Wind Speed and Direction in the afternoon")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Question 3: A Delivery business needs to know the optimal Work times for their drivers:
# Analysis 3.1: Finding the visibility rates for the roads in regard to Rainfall:
  
RainfallMean = mean(weather$Rainfall)    
AboveAvgRain = subset(weather, Rainfall > RainfallMean)


# Data Visualization 3.1:
AboveAvgRainplot = ggplot(AboveAvgRain, aes(x= Date, y = Rainfall))+
  geom_line(aes(color = Rainfall) , size = 0.8)+
  scale_x_date(date_labels = ("%b ' %y"), date_break = "1 month")+
  scale_y_continuous(breaks =  c(5, 10 ,15 , 20, 25, 30, 35, 40 , 45))+
  ggtitle("Above average Rain Fall during the year")+
  labs(x="Date", y= "Rain Fall")
AboveAvgRainplot

# Analysis 3.2: Finding the visibility rates for the roads in regard to Fog:


MinTemp=weather$MinTemp
Evaporation=weather$Evaporation

# Data Visualization 3.2:
ggplot(weather, aes(x=Date))+
  geom_line(aes(y = Evaporation),size = 0.8 ,col="blue")+
  geom_line(aes(y = MinTemp),size = 0.8 ,col="red")+
  scale_y_continuous(breaks = c(seq(-10,40,5)))+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
               date_labels = "%b ' %y")+
  labs(title ="Minimum Temperatures and Evapration relation in Fog fomation
       throughout the year", x="Date",
       y = "Evaporation and Temperature change",
       subtitle = "Blue = Evaporation\nRed = Temperature")


# Analysis 3.3: Finding the visibility rates for the roads in regard to sudden Wind Gust:
WindGustMean = mean(weather$WindGustSpeed)    
AboveAvgWindGust = subset(weather, WindGustSpeed > WindGustMean)



# Data Visualization 3.3:

AboveAvg_WindGustplot= ggplot(AboveAvgWindGust, aes(Date, WindGustSpeed)) +
  geom_point()+scale_x_date(date_labels = ("%b ' %y"), date_break = "1 month")+
  scale_y_continuous(breaks =  c(5, 10 ,15 , 20, 25, 30, 35, 40 , 45, 50
                                 ,55, 60, 65, 70))+
  ggtitle("High Wind Gust Speeds during the year")+
  labs(x="Date", y= "Wind Gust Speed")

AboveAvg_WindGustplot + geom_smooth(method = lm)

AboveAvg_WindGustplot + geom_smooth(method = "loess")

# Analysis 3.4: Finding the most comfortable dates to drive
#    in regard to Temperature:

MaxTempMean = mean(weather$MaxTemp)    
AboveAvgMaxTemp = subset(weather, MaxTemp > MaxTempMean)

# Data Visualization 3.4:
AboveAvg_MaxTempPlot= ggplot(AboveAvgMaxTemp , aes(x=Date, y= MaxTemp))+
  geom_segment( aes(x=Date, xend=Date, y =0, yend=MaxTemp),
                color = "purple", size= 1.3 )+
  geom_point(color = "orange", size= 4.5)+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
               date_labels = "%b ' %y")+
  scale_y_continuous(breaks = c(seq(-10,40,5)))+
  ggtitle("Above average Temperature during the year")

AboveAvg_MaxTempPlot


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Question 4: Fast food business enquiring about increased
# sales for specific types of items, and studying customers behavior:

# Analysis 4.1: Finding the percentage of humid days:

HumidMorAvg = mean(weather$Humidity9am)  
HumidNoonAvg = mean(weather$Humidity3pm)
AbovAvgHumMor = which(weather$Humidity9am > HumidMorAvg)
AbovAvgHumNoon = which(weather$Humidity3pm > HumidNoonAvg)
AbovAvgHumMor_Days = length(AbovAvgHumMor)
AbovAvgHumNoon_Days = length(AbovAvgHumNoon)


# Data Visualization 4.1:
x=c(AbovAvgHumMor_Days,AbovAvgHumNoon_Days)
y=c("Humid Mornings", "Humid Afternoons")
percen = round(x/sum(x)*100)
y=paste(y,percen)
y=paste(y,"%", sep = "")
pie(x, labels = y, radius = 1, main = "Humid Times Percentage Throughout The Year",
    col = c("green","steelblue"), clockwise = TRUE)



# Analysis 4.2: Finding the Humid mornings and afternoons intensity
# and its relation to expected sales:

HumidMorAvgSet = subset(weather, Humidity9am > HumidMorAvg)
HumidNoonAvgSet = subset(weather, Humidity3pm > HumidNoonAvg)

# Data Visualization 4.2:

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot(HumidMorAvgSet , aes(x=Date, y= Humidity9am))+
  geom_segment( aes(x=Date, xend=Date, y =0, yend=Humidity9am),
                color = "orange", size= 1.3 )+
  geom_point(color = "darkblue", size= 4.5)+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
               date_labels = "%b ' %y")+
  scale_y_continuous(breaks = c(seq(-10,120,5)))+
  ggtitle("Above average Humidity in the morning during the year")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot(HumidNoonAvgSet , aes(x=Date, y= Humidity3pm))+
  geom_segment( aes(x=Date, xend=Date, y =0, yend=Humidity3pm),
                color = "forestgreen", size= 1.3 )+
  geom_point(color = "skyblue", size= 4.5)+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
               date_labels = "%b ' %y")+
  scale_y_continuous(breaks = c(seq(-10,120,5)))+
  ggtitle("Above average Humidity in the afternoon during the year")


# Analysis 4.3: Finding the air pressure rates in the morning and in the afternoon 
# and its effect on the customers:
# Data Visualization 4.3:

ggplot(weather, aes(x=Pressure9am, y=Pressure3pm)) +
  geom_density2d() +
  labs(y = "Pressure Afternoon",
       x = "Pressure Morning",
       title = "Pressure Density during the morning and afternoon")



# Analysis 4.4: Finding the percentage of rainy days:
rainTodY = which(weather$RainToday == "Yes")
rainTodN = which(weather$RainToday == "No")
rainTodY_Days=length(rainTodY)
rainTodN_Days=length(rainTodN)
print(rainTodY_Days)
print(rainTodN_Days)

# Data Visualization 4.4:
x=c(rainTodY_Days,rainTodN_Days)
y=c("Rainy Days", "Clear Days")
percen = round(x/sum(x)*100)
y=paste(y,percen)
y=paste(y,"%", sep = "")
pie(x, labels = y, radius = 1, main = "Rainy and Clear Days Yearly Percentage",
    col = c("blue","red"), clockwise = TRUE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Question 5: The Coast guard want to warn ships in ports about dangerous sailing conditions:

# Analysis 5.1: When is the sky too cloudy to aid in navigation:

AvgCloudMon = mean(weather$Cloud9am) 
AvgCloudNoon = mean(weather$Cloud3pm)  

HighCloudMon = subset(weather, Cloud9am > AvgCloudMon)
HighCloudNoon = subset(weather, Cloud3pm > AvgCloudNoon)

# Data Visualization 5.1:
#(color = Cloud9am)
HighCloudMonPlot = ggplot(HighCloudMon, aes(x= Date, y = Cloud9am))+
  geom_line(aes(color = Cloud9am), size = 0.8 )+geom_point(color="maroon", size=3)+
  scale_x_date(date_labels = ("%b ' %y"), date_break = "1 month")+
  scale_y_continuous(breaks =  c(1, 2 ,3 , 4, 5, 6, 7, 8 , 9))+
  ggtitle("Morning Cloud formation during the year")+
  labs(x="Date", y= "Cloud (Okta)")
HighCloudMonPlot


HighCloudNoonPlot = ggplot(HighCloudNoon, aes(x= Date, y = Cloud3pm))+
  geom_line(aes(color = Cloud3pm) , size = 0.8)+geom_point(color="maroon", size=3)+
  scale_x_date(date_labels = ("%b ' %y"), date_break = "1 month")+
  scale_y_continuous(breaks =  c(1, 2 ,3 , 4, 5, 6, 7, 8 , 9))+
  ggtitle("Afternoon Cloud formation during the year")+
  labs(x="Date", y= "Cloud (Okta)")
HighCloudNoonPlot

# Analysis 5.2: What are the highest chances for Cyclones to form:

cycloneFactors =dplyr::select(weatherNumeric,WindGustSpeed,Rainfall,Pressure9am,Pressure3pm)

# Data Visualization 5.2:

cycloneFactorsCor = cor(cycloneFactors)
corrplot(cycloneFactorsCor, method = "shade",type = "lower", 
         cl.ratio = 0.1, cl.align = "l",tl.col = "black", tl.srt = 0)


ggplot(cycloneFactors, aes(x=Rainfall, y=WindGustSpeed,
                           height=2, width=2)) +
  geom_tile(aes(fill = Pressure9am) ,size = 100) +
  scale_fill_distiller(palette = "YlGnBu") +
  labs(title = "Likelihood of cyclone formation in the Morning",
       y = "WindGustSpeed (mph)")

ggplot(cycloneFactors, aes(x=Rainfall, y=WindGustSpeed,
                           height=2, width=2)) +
  geom_tile(aes(fill = Pressure3pm) ,size = 100) +
  scale_fill_distiller(palette = "Spectral") +
  labs(title = "Likelihood of cyclone formation in the Afternoon",
       y = "WindGustSpeed (mph)")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## Extra Features:
#~~~~~~~~~~~~~~~~~~~~~

#Extra feature 1 : Identifying and Removing Outliers:
boxplot(weather$MinTemp, plot=FALSE)$out
boxplot(weather$MaxTemp, plot=FALSE)$out
boxplot(weather$Rainfall, plot=FALSE)$out
boxplot(weather$Evaporation, plot=FALSE)$out
boxplot(weather$Sunshine, plot=FALSE)$out
boxplot(weather$WindGustSpeed, plot=FALSE)$out
boxplot(weather$WindSpeed9am, plot=FALSE)$out
boxplot(weather$WindSpeed3pm, plot=FALSE)$out
boxplot(weather$Humidity9am, plot=FALSE)$out
boxplot(weather$Humidity3pm, plot=FALSE)$out
boxplot(weather$Pressure9am, plot=FALSE)$out
boxplot(weather$Pressure3pm, plot=FALSE)$out
boxplot(weather$Cloud9am, plot=FALSE)$out
boxplot(weather$Cloud3pm, plot=FALSE)$out
boxplot(weather$Temp9am, plot=FALSE)$out
boxplot(weather$Temp3pm , plot=FALSE)$out
boxplot(weather$RISK_MM , plot=FALSE)$out

# Saving the outlier values into a vector
outliers = c(boxplot(weather$WindGustSpeed, plot=FALSE)$out,boxplot(weather$Rainfall, plot=FALSE)$out,
             boxplot(weather$Evaporation, plot=FALSE)$out,boxplot(weather$WindSpeed9am,plot=FALSE)$out,
             boxplot(weather$WindSpeed3pm, plot=FALSE)$out,boxplot(weather$Humidity9am, plot=FALSE)$out,
             boxplot(weather$Humidity3pm, plot=FALSE)$out,boxplot(weather$Pressure9am, plot=FALSE)$out,
             boxplot(weather$Pressure3pm, plot=FALSE)$out,boxplot(weather$RISK_MM , plot=FALSE)$out)


weather = weather[-which(weather$WindGustSpeed %in% outliers),]

# Removing outliers Is essential in maintaining an accurate Data set
#by eliminating the extreme values that may skew the overall analysis results

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Extra feature 2 :Skim through the data set with skim():
skim(weather)

# the skim() function allows for greater exploration of the data set
# as well as giving more information that the summary() function

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Extra feature 3 : Visualizing the Data set with vis_dat() :
devtools::install_github("ropensci/visdat")
library(visdat)
vis_dat(weather)

#The vis_dat() function is helpful in giving a graphical demonstration to the 
# data set elements and their respective data types

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Extra feature 4 : Making a Correlation plot with corrplot() :
weatherNumeric = subset(weather, select = -c(WindGustDir , WindDir9am, WindDir3pm ,
                                             RainToday ,  RainTomorrow ,Date  ))
weatherCor = cor(weatherNumeric)
corrplot(weatherCor, method = "circle")

#The corrplot package helps in making a correlation matrix
#it is useful in showcasing details, it has options to choose color, text labels, etc.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Extra feature 5 : The use of geom_histogram() and geom_density():
ggplot(weather, aes(x=WindGustSpeed ,color =WindGustDir )) + 
  geom_histogram(aes(y=..density..), colour="yellow", fill="black")+
  geom_density(alpha=.1,size=1, fill="#FFFF00") 

# geom_histogram() Visualizes the distribution of a single continuous variable
# by dividing the x axis into bins and counting the number of observations in each bin.

#geom_density() can function as a replacement for the histograms or can be added to
# increase the clarity of the distribution of the data.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Extra feature 6 :The use of geom_boxplot() and geom_jitter():
ggplot(data = weather, mapping = aes(x = WindDir9am, y = WindSpeed9am)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(alpha = 0.3, color = "blue")+
  labs(title = "Wind Speed and Direction in the Morning")

#geom_boxplot is used to compare distribution of data in several groups.

#geom__jitter  It adds a small amount of random variation to the location of each point,
#and is a useful way of handling overplotting

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Extra feature 7 :The use of geom_segment() and geom_point() "Lollipop chart": 
AboveAvg_MaxTempPlot= ggplot(AboveAvgMaxTemp , aes(x=Date, y= MaxTemp))+
  geom_segment( aes(x=Date, xend=Date, y =0, yend=MaxTemp),
                color = "purple", size= 1.3 )+
  geom_point(color = "orange", size= 4.5)+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
               date_labels = "%b ' %y")+
  scale_y_continuous(breaks = c(seq(-10,40,5)))+
  ggtitle("Above average Temperature during the year")
AboveAvg_MaxTempPlot

# A Lollipop chart is a hybrid between a bar chart and a dot plot..
# A lollipop chart typically contains categorical variables on the y-axis
# measured against a second (continuous) variable on the x-axis.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Extra feature 8 :The use of geom_density2d():
ggplot(weather, aes(x=Pressure9am, y=Pressure3pm)) +
  geom_density2d() +
  labs(y = "Pressure Afternoon",
       x = "Pressure Morning",
       title = "Pressure Density during the morning and afternoon")


# geom_density2d Performs a 2D kernel density estimation, and display the results with contours.
# can be useful for dealing with overplotting. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Extra feature 9 : refactoring and formatting corrplot():
cycloneFactorsCor = cor(cycloneFactors)
corrplot(cycloneFactorsCor, method = "shade",type = "lower", 
         cl.ratio = 0.1, cl.align = "l",tl.col = "black", tl.srt = 0)

# Useful to make the plot more clear by the use of the "method = ...",
#"type = ...", "cl.ratio = ...", "cl.align = ...","tl.col = ...", 
# and "tl.srt = ..." parameters.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Extra feature 10 :The use of geom_tile():
ggplot(cycloneFactors, aes(x=Rainfall, y=WindGustSpeed,
                           height=2, width=2)) +
  geom_tile(aes(fill = Pressure9am) ,size = 100) +
  scale_fill_distiller(palette = "YlGnBu") +
  labs(title = "Likelihood of cyclone formation in the Morning",
       y = "WindGustSpeed (mph)")

# geom_tile() is used in heat maps and works by 
# using the center of the tile and its size (x, y, width, height).


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~