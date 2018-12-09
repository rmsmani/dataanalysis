#---------------------------------------------------------------------
# File Name    : CommonFunctions.R
# Developed By : Mani M
# Purpose      : This file contains the commonly used R Functions for weather prediction
# To Run       : This need to executed on every session 
#              : From Command Line - RScript CommonFunctions.R
#              : From RStudio - Copy the contents and run
#---------------------------------------------------------------------

#attach the packages to the session
library("weatherData")
library("data.table")
library("lattice")
library("class")
library("plotrix")
library("ggplot2")
library("forecast")
library("xts")
library(zoo)

#Remove the function before recreating
remove (funggcast)

#----------------------
# Function to convert the ARIMA out to df to use in GGPLOT
# Downloaded from http://davenportspatialanalytics.squarespace.com/blog/2012/3/14/plotting-forecast-objects-in-ggplot-part-1-extracting-the-da.html
#----------------------
funggcast <- function(dn, fcast) {
  #en <- max(time(fcast$mean)) # Extract the max date used in the forecast (?)
  # Extract Source and Training Data
  ds <- as.data.frame(dn[,1])
  names(ds) <- 'observed'
  ds$date <- time(dn)
  
  # Extract the Fitted Values (need to figure out how to grab confidence intervals)
  dfit <- as.data.frame(fcast$fitted)
  
  dfit$date <- ds$date
  names(dfit)[1] <- 'fitted'
  
  ds <- merge(ds, dfit, all.x = T) # Merge fitted values with source and training data
  
  # Extract the Forecast values and confidence intervals
  dfcastn <- as.data.frame(fcast)
  dfcastn$date <- time(fcast) + time(dn)[length(dn)]
  
  names(dfcastn) <- c('forecast', 'lo80', 'hi80', 'lo95', 'hi95', 'date')
  
  #pd <- merge(ds, dfcastn, all = T) # final data.frame for use in ggplot
  pd <- merge(ds,dfcastn,all=TRUE)
  return (pd)
 }

#----------------------
# Purpose : Function funggcast does have NA values which is having issues in generating ggplot
# observed,fitted is NA for PROJECTED data
# lo80, hi80, lo95, hi95 is NA for HISTORICAL data
#----------------------
 normalise_pd <- function (pd){
	md <-data.frame(pd$date, pd$observed, pd$fitted, pd$forecast, pd$lo80, pd$hi80, pd$lo95, pd$hi95)
	names(md) = c("date","observed", "fitted","forecast","lo80","hi80","lo95","hi95")
	md$observed[is.null(md$observed)] <-0
	md$fitted[is.null(md$fitted)] <-0
	md$forecast[is.null(md$forecast)] <-0
	md$lo80[is.null(md$lo80)] <-0
	md$hi80[is.null(md$hi80)] <-0
	md$lo95[is.null(md$lo95)] <-0
	md$hi95[is.null(md$hi95)] <-0

	return(md)
}

#----------------------
# Purpose : A Common Function to Get the WEATHER DATA USING weatherData Package by passing the required info
# Param :  
# 	P1 -CITY CODE to pick weatherData from web
#	P2 -Start Date 
#	P3 -end Date 
#	P4 -City Name to append in the DF 
#	P5 -latitude longitude sealevel to append in the df
#	Once the data is extracted, appending the city code, latlongsl, the data will be saved as the .csv file in the working directory (File name <cityname>_Sum_<current time "%d%m%Y_%H%M">.csv)
#   File Name : SYDNEY_Sum_11052017_1515.csv 	
#----------------------
getSummaryData <- function (city_code, stdate, enddate, city_name, latlongsl){
  ct_time=format(Sys.time(), "%d%m%Y_%H%M")
  file_name=paste(city_name,"_Sum_",ct_time,".csv", sep="")
  city_code <- toupper(city_code)
  #getWeatherForDate standard method available in weatherData Package
  dfs <- getSummarizedWeather(city_code, stdate, enddate, opt_all_columns = TRUE)
  dfs$CITY=city_name
  dfs$LAT=latlongsl
  outdf <- data.table(dfs$CITY, dfs$LAT, as.Date(dfs$Date, format="%Y-%m-%d"), 
					dfs$Max_TemperatureC, dfs$Mean_TemperatureC, 
					dfs$Min_TemperatureC, dfs$Max_Sea_Level_PressurehPa, 
					dfs$Mean_Sea_Level_PressurehPa, dfs$Min_Sea_Level_PressurehPa, 
					dfs$Precipitationmm, dfs$Events)
  names(outdf)<-c("CITY","LAT_LONG_SL", "DATE", "MAXTEMPERATUREC" , "MEANTEMPERATUREC", "MINTEMPERATUREC","MAX_SL_PR_HPA", "MEAN_SL_PR_HPA", "MIN_SL_PR_HPA" ,"PRECIPITATIONMM", "EVENTS")
  #Saving the data as the CSV file in the working directory
  write.table(outdf, file=file_name, quote =FALSE, sep = "|", row.names = F)
  #outdf$DATE <- as.Date (outdf$DATE, format="%Y-%m-%d")

	# Checking for the unique values
  unique(outdf$TEMPERATUREC)
  #checking if the rainfall field is null, if it is null appending with zero
  outdf$PRECIPITATIONMM[is.null(outdf$PRECIPITATIONMM)] <-0.00
  print(paste("The data is saved in File in the working directory : ", file_name))
  return(outdf)
}

#----------------------
# Purpose : A Common Function to get the plot based on the Inputdate & the plot title
# Param :  
# 	P1 - Data Frame
#	P2 - Plot title
#	P3 - Y Label
#----------------------
getPlot <- function (fcdata, gtile, ylabel){
	fc <- fcdata
	tplot <- ggplot (data = fc, aes(x=fc$date, y = fc$observed ))+
	geom_line(aes(color = "1")) +
	geom_line(aes(y = fc$fitted, color = "2")) +
	geom_line(aes(y = fc$forecast, color = "3")) +
	scale_colour_manual(values = c("red", "blue", "black"), 
			  labels = c("Observed", "Fitted", "Forecasted"), name = "Data") +
	geom_ribbon(aes(ymin = fc$lo95, ymax = fc$hi95), alpha = .25)+
	labs(x="Date",y=ylabel) +
	ggtitle(gtile)
	
	return (tplot)
}

#----------------------
# Purpose : A Common Function to export the Plot to the pdf file
# Param :  
# 	P1 -Plot
#	P2 -Type (Rainfall - Rain, Temperature =Temp) 
#	P3 -City Name
#   File Name : SYDNEY_FCTemp_13052017_1713.pdf 	
#----------------------
savePlot <- function (pplot, gtype, city_name){
  ptime=format(Sys.time(), "%d%m%Y_%H%M")	
  fname=paste(city_name, "_FC", gtype, "_", ptime, ".pdf", sep="")
  ggsave(filename = fname, plot=pplot, width = 8, height = 6) #Saving the plot to pdf file
  print (paste("Export File Name (in working directory) :", fname))
}
