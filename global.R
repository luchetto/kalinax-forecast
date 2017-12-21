
#LIBRARY - check if packages are installed and load them
library(dplyr, warn.conflicts = FALSE)
library(zoo, warn.conflicts = FALSE)
library(forecast, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(dygraphs, warn.conflicts = FALSE)
library(readxl, warn.conflicts = FALSE)


# DATA --------------------------------------------------------------------
#READ DATA
filename <- as.character("data_to_forecast.xlsx")  #  file should be located in wd
if ( all(list.files() != filename))  warning('You are in the wrong folder or the desired file does not exist in the current working folder!')
sheets <- readxl::excel_sheets(filename)
Kalinax_data_sheets <- lapply(sheets, function(X) read_excel(filename, sheet = X))
names(Kalinax_data_sheets) <- sheets

#MERGE DATA INTO DATAFRAME 
support_dataframe <- merge(Kalinax_data_sheets$`Financial data`, Kalinax_data_sheets$`Web IDs`, by.x = "Lead ID", by.y ="Lead ID")
data_frame <- merge(support_dataframe, Kalinax_data_sheets$`Google Analytics data`, by.x = "GA Transaction ID", by.y = "Transaction ID")

# CUSTOM FUNCTIONS --------------------------------------------------------------------

# CREATE TIMESERIES TO PLOT 

funggcast<-function(dn,fcast){ 
	require(zoo) #needed for the 'as.yearmon()' function
 	loc <- Sys.getlocale("LC_TIME")
	Sys.setlocale("LC_TIME", "C")
	en<-max(time(fcast$mean)) #extract the max date used in the forecast
 	#Extract Source and Training Data
	ds<-as.data.frame(window(dn,end=en))
	names(ds)<-'observed'
	ds$date<-as.Date(time(window(dn,end=en)))
 
	#Extract the Fitted Values (need to figure out how to grab confidence intervals)
	dfit<-as.data.frame(fcast$fitted)
	dfit$date<-as.Date(time(fcast$fitted))
	names(dfit)[1]<-'fitted'
 
	ds<-merge(ds,dfit,all.x=T) #Merge fitted values with source and training data
 
	#Exract the Forecast values and confidence intervals
	dfcastn<-as.data.frame(fcast)
	dfcastn$date<-as.Date(as.yearmon(row.names(dfcastn)))
	names(dfcastn)<-c('forecast','lo80','hi80','lo95','hi95','date')
 
	pd<-merge(ds,dfcastn,all = TRUE) #final data.frame
	pd$observed[pd$date == "2017-01-01"] <- pd$forecast[pd$date == "2017-01-01"] # making observed data continue with forecasted data
	pd$fitted[pd$date == "2017-01-01"] <- pd$forecast[pd$date == "2017-01-01"] # making fitted data continue with forecasted data
  
	pd_ts <- ts(pd[, -1], frequency=12, start=c(2014,01)) #final timeseries to use for dygraph (date column is not included) 
	return(pd_ts)
}

