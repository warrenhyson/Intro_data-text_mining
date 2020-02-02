library(tidyverse)
library(reshape2)
library(data.table)
library(lubridate)
library(RColorBrewer)

data("airquality")
air=airquality


#*******
#Clean Data
#*******
    summary(air)
            # ozone has 37 NA, Solar has 7
    
    air_clean <- data.frame(   #creates a new data.frame that replaces missing values with the mean
        sapply(air, 
               function(x) ifelse(is.na(x), 
                                  mean(x, na.rm = T),
                                  x)
               ))
    
    air=air_clean
    summary(air)   #no more missing values 
    
    setnames(air, "Solar.R", "Solar")

#*******
#Data Distribution
#*******
    
    Ozone_hist <- ggplot(air, aes(x = Ozone))+
      geom_histogram(bins = 7, col = 'black', fill = 'blue')
    
    Solar_hist <- ggplot(air, aes(x = Solar))+
      geom_histogram(bins = 10, col = 'black', fill = 'honeydew')
    
    Wind_hist <- ggplot(air, aes(x = Wind))+
      geom_histogram(bins = 5, col = 'black', fill = 'pink1')
    
    Temp_hist <- ggplot(air, aes(x = Temp))+
      geom_histogram(col = 'black' , fill = 'cyan1', breaks = seq(56, 97, by = 5))
    
    Wind_boxplot <- ggplot(air, aes(x = factor(0), y = Wind))+
      geom_boxplot(outlier.color = 'black', fill = 'dodgerblue')
    
    Ozone_boxplot <- ggplot(air, aes(x = factor(0), y = Ozone))+
      geom_boxplot(outlier.color = 'black', fill = 'powderblue')
    
    print(Ozone_hist)
    print(Solar_hist)
    print(Wind_hist)
    print(Temp_hist)
    print(Wind_boxplot)
    print(Ozone_boxplot)
    
    
#*******
#Change over time 
#*******
    
    air$date <- paste(air$Month, air$Day, "1973", sep = '-')
    
    air_long <- air
    air_long <- air_long[, c(-5, -6)]
    
    str(air_long)
    
    air_long$date <- mdy(air_long$date)
  
    air_long <- air_long %>%
      mutate(Wind = Wind*10)%>%
      gather(key, value, -date)
    
    
    ggplot(air_long, aes( x = date, y = value, group_by(key), col = key))+
      geom_line()+
      scale_x_date("Date")+
      scale_y_continuous("Value")

#*******
#Data via heatmap
#*******
    
    col=brewer.pal(12, 'Paired')
    
    ggplot(air_long, aes(x = date, y = key, fill = value))+
      geom_tile()+
      scale_fill_gradient(low = min(col), high = max(col))
  

#*******
#Data via scatterplot
#*******

    air <- air%>%
      mutate(Wind = Wind*10)
    
    ggplot(air, aes(x = Wind, y = Temp, size = Ozone, col = Solar))+
      geom_point(alpha = .6)

#*******
#Final Analysis 
    # Are there any patterns in the data
    # What was the most usefull visualization
#*******
    
    
    # There is a positive relationship between Ozone~Temp, however the input method 
    # for the missing values to alter the accuracy of the linear model
    
    ggplot(air, aes( x = Temp, y = Ozone))+
      geom_point(alpha = .6, position = 'jitter')+
      geom_smooth(method = 'lm')
    
    # There is also a negative relationship between Temp~Wind, but it does not appear to be very strong
    # There isn't a very week relationship between Solar and Temp

    temp.lm <- lm(Temp~Wind, air)
    
    summary(temp.lm) 
    
    solar.lm <- lm(Solar~Temp, air)

    summary(solar.lm)
    
    