# Air_Quality_Analysis_with_Openair_Package

## Table of Content
- [Project overview](project-overview)
- [Datasets source](datasets-source)
- [Dataset extraction details from NASA](dataset-extraction-details-from-nasa)
- [Tools](tools)
- [Data preparation in Excel](data-preparation-in-excel)
- [Data preparation and manipulation in R](data-preparation-and-manipulation-in-r)
- [Brief summary](brief-summary)
- [References](references)
  
## Project overview
The openair package in R studio is an R tool, which has been designed for analyzing air quality (air pollution) datasets, and these could include meteorological dataset. However, this is dependent on the objective and the type of analysis required. This piece has not been exhaustive, and more information can be sourced from [Dowload here](https://davidcarslaw.com/files/openairmanual.pdf).

### Datasets source
The datasets were daily (average) air quality datasets for Manchester (UK) in 2023, collected from NASA [Link](https://power.larc.nasa.gov/data-access-viewer) and compiled into a csv file, “air_manchester.csv” [Datasets csv file](https://power.larc.nasa.gov/api/temporal/daily/point?parameters=T2M,RH2M,PRECTOTCORR,WS10M,WD10M&community=RE&longitude=-2.2379&latitude=53.4815&start=20230101&end=20231231&format=CSV). The variables included air pollutants - ozone (O3) and nitrogen dioxide (NO2), and meteorological parameters - temperature (temp), relative humidity (rh), wind speed (ws) and wind direction (wd). The collated meteorology datasets were at 2 metres.

### Dataset extraction details from NASA
- Local Authority: Machester City District
- Monitorin site: Manchester Piccadilly
- Lat: 53.48152
- Long: -2.23788
- Site type: Urban background
- Zone: North West & Merseyside
- Agglomeration: Greater Mnachester Area

### Tools
- Excel - Data cleaning
    -[Download here](https://microsoft.com)
- R - Data Analysis and visualization (openair)
    - [Download here](https://cran.r-project.org) for R
    - [Download here](https://rstudio-desktop.en.softonic.com/) for R Studio

### Data preparation in Excel
- Data loading and inspection.
- Handling missing data (changed to NA) in excel before importing to R studio.

### Data preparation and manipulation in R
```R
- #set working directory
setwd("C:/Users/PCUSER/Desktop/PORTFolio")

#read in the data file (.csv) into a dataframe or import.
air_manchester <- read.csv("air_manchester.csv")

str(air_manchester)                #data structure

library(lubridate)                 #install and load library for date manipulation
air_manchester$date <- as.Date(air_manchester$date)    #change date from character to date format
class(air_manchester$date)                       #check/confirm class of date changed

str(air_manchester)                              #confirm structure of updated ddataframe
View(air_manchester)
library(tidyverse)                 #install  and load for multiple packages for data manipulation
library(dplyr)
library(VIM)                       #install and load for missing value imputation visualization
aggr(air_manchester)               #find proportion of missing values
nrow(na.omit(air_manchester))      #omitted rows from number of rows
nrow(air_manchester)

library(mice)                      #install and load mice function for missing values imputation
air_manchester2 <- mice(air_manchester)        #new name for data frame
attributes(air_manchester2)
air_manchester3 <- complete(air_manchester2)   #new name for completed imputation
aggr(air_manchester3)                          #missing data removed as displayed
class(air_manchester3)
View(air_manchester3)

air_manchester3$year <- year(air_manchester3$date)      #create column for year
air_manchester3$month <- month(air_manchester3$date)    #create column for month
air_manchester3$day <- day(air_manchester3$date)        #create column for day
air_manchester3                                         #new columns created

library(psych)                                         #for descriptive analysis
describeBy(air_manchester3~month)                      #statistics by group or month 

#Normality Test (using shapiro-wilk test instaed ofn graphs) 
shapiro.ozone <- shapiro.test(air_manchester3$ozone)
shapiro.ozone

shapiro.no2 <- shapiro.test(air_manchester3$no2)
shapiro.no2

shapiro.temp <- shapiro.test(air_manchester3$temp)
shapiro.temp

shapiro.rh <- shapiro.test(air_manchester3$rh)
shapiro.rh

shapiro.ws <- shapiro.test(air_manchester3$ws)
shapiro.ws

shapiro.wd <- shapiro.test(air_manchester3$wd)
shapiro.wd

#OPENAIR PACKAGE (Visualizations)
library(openair)                 #install and load

##convert date components to factor
air_manchester3$day <- factor(air_manchester3$day)
air_manchester3$month <- factor(air_manchester3$month) 
air_manchester3$year <- factor(air_manchester3$year)

#summary plot for time series and distribution histogram (can also be used to determine normality)
summaryPlot(air_manchester3)

#timeplot for all variables in one frame
timePlot(selectByDate(air_manchester3, month = 1:12), pollutant = c("ozone", "no2", "temp", "rh", "ws", "wd"), y.relation = "free") 

#install and load "latticeExtra" for last plot update
##latticeExtra is used indicate highest point of select variables (only air pollutants which are row 1 & 2 selected).
##Can select points for all variables or rows
library(latticeExtra)  #load function, already installed with openair package
trellis.last.object() +
  layer({maxy <- which.max(y);
  lpoints(x[maxy], y[maxy], col = "black", pch = 16)},
  rows = c(1,2))

#corplot function for correlation matrices between variables
##spearman method for correlation coefficients (datasets failed normality test)
###Repressented in % & 100 = 1
corPlot(air_manchester3, method = "spearman")

corPlot(air_manchester3, type = "season", method = "spearman") #correlation by season

#wind speed and wind direction intervals
windRose(air_manchester3) 

#wind direction and speed by seasons
windRose(air_manchester3, type = "season") 

#wind direction/speed frequencies influence on pollutants in quartiles

windRose(air_manchester3, type = "ozone", layout = c(4,1)) 
windRose(air_manchester3, type = "no2", layout = c(4,1))

#pollutant concentration influence by wind direction only
pollutionRose (air_manchester3, pollutant = "ozone")
pollutionRose (air_manchester3, pollutant = "no2")

#pollutant conditioned by another pollutant under wind direction
pollutionRose (air_manchester3, pollutant = "ozone", type = "no2", layout = c(4,1))

#pollutants concentration influence by wind direction in percentile
percentileRose (air_manchester3, poll = c("ozone", "no2"), col = "brewer1", key.position = "right",
                smooth = TRUE)

#for daily concentration of pollutant
calendarPlot(air_manchester3, pollutant = "ozone")
calendarPlot(air_manchester3, pollutant = "no2")

#calendar plot wind direction/speed for ozone in the month of April
##longer arrow = higher wind speed
calendarPlot(air_manchester3, pollutant = "ozone", month = 4, annotate = "ws")

#different time variations for pollutant(S)
timeVariation(air_manchester3,pollutant = c("ozone", "no2"), normalise = TRUE)

#trend levels of ozone conc across the months
trendLevel(air_manchester3, pollutant = "ozone",
           border = "white", statistic = "max",
           breaks = c(0, 50, 100, 500),
           labels = c("low", "medium", "high"),
           cols = c("forestgreen", "yellow", "red"))

#trend levels of ozone conc by seasons
trendLevel(air_manchester3, pollutant = "ozone", type = "season",
           border = "white", statistic = "max",
           breaks = c(0, 50, 100, 500),
           labels = c("low", "medium", "high"),
           cols = c("forestgreen", "yellow", "red"))
```
###Brief summary
This piece (analysis) is aimed at exploring some of the functions of Openair package in R, and not necessarily to draw results. 
However, the results via visulaizations provided by the openair package could easily be interpreted. 
The components of the codes can be substitued or manipulated to meet research obejectives or variables were relevant.
Results (visualizations) for the above codes in pdf are attached.

### References
1. Carslaw, D.C. and K. Ropkins, (2012). openair — an R package for air quality data analysis. Environmental Modelling & Software. Volume27-28,pp.
52–61.
2. Carslaw, D.C. (2019). The openair manual — open-source tools for
analysing air pollution data. Manual for version 2.6-6, University of York.
