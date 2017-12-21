# Kalinax-forecast
## R project for kalinax assignment
---------------------------------

### Description of problem
	
**Objective**	
Through the financial data of the last 3 years (2014, 2015 and 2016) and Google Analytics (marketing) data they want to do a predictive analysis for the coming years.	
	
Develop an algorithm (using R) capable of calculating the estimated revenue for the year 2017 and later.
Users should be able to turn on/off and isolate the following dimensions: 
- Time (all time / year / quarter / month)
-  Lead Controlling Office (all / specific)
- Geography (destination country / destination region)
- Mode of transport (all / specific)
- Sales person (all / specific)
- Customer segment (all / specific: millennial, quality of lifer, retiree)
- Marketing campaigns (all / channel: paid, referral, organic, direct) 
	
This is a creative challenge! You can add anything you see that adds value to the client.	

### How to run this R app

The forecast is developed using the shiny library. In order to use and interact with the data follow the step below 

- download and unzip kalinax-forecast project

- open R console from the parent directory of "kalinax-forecast" folder 
	```
	> library(shiny)
	> runApp("kalinax-forecast")
	```
- A page with the running app should open on your browser 

### Prerequisite

In order to be able to run this App you need the following:
- R 
- library(dplyr)
- library(zoo)
- library(forecast)
- library(lubridate)
- library(dygraphs)
- library(readxl)

To install a package in R console: 
```
install.packages("package_name")
```
