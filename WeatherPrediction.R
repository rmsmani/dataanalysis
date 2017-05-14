#---------------------------------------------------------------------
# File Name    : WeatherPrediction.R
# Developed By : Mani M
# Purpose      : This file is used to predict the weather for Given Location 
# To Run       : 
#              : From RStudio - Copy the contents and run
#---------------------------------------------------------------------
#Change the below values with the required values before running to run the script
city_cd=""								#City code
st_date="2017-05-01"						#Starting date (YYYY-MM-DD)
end_date="2017-05-10"						#Ending date (YYYY-MM-DD)
city_name=""							#City_name
latlongsl=""		#Latitude, longitude, sea level (m)
ptime=format(Sys.time(), "%d%m%Y_%H%M")		
plot_title= paste("Projected for ", city_name, " at ", ptime, sep="")	# for Graph title value

#Removing the variables before processing, may get an warning message if variable not available
remove (wea_data_sum,wds)

#Calling the get summary data functions for the required CITIES.
wea_data_sum <- getSummaryData(city_cd,st_date, end_date, city_name, latlongsl)
wea_data_sum		#Display the data
wds <- data.table(as.Date(wea_data_sum$DATE, "%Y-%m-%d"),wea_data_sum$MAXTEMPERATUREC,wea_data_sum$PRECIPITATIONMM) #Creating the separate object with requried fields
names(wds)<-c("DATE_TIME","MAX_TEMPERATUREC", "RAINFALLMM") #assigning the fields
wds #Display the data
wds <- na.omit(wds) #Removing NA values from the dataframe

# Getting the date & temperature to seperate object
wea_dt <- as.Date (wds$DATE_TIME, format ="%Y-%m-%d")
wea_temp <- wds$MAX_TEMPERATUREC
wea_ts <- xts (wea_temp, order.by=wea_dt) 				# Applying the Time Series
extt <- wea_ts 											# Assigning to temp variable for easy usage
ext_fit <- arima(extt, c(1,0,0)) 						# fit arima model
est_for <- forecast(ext_fit, h=10, npaths = 5) 			# Forecasting based on the ARIMA
pd_data <- funggcast(wea_ts, est_for)					# Converting the ARIMA forecast output to DF for ggplot
out_data <- normalise_pd(pd_data)						# Normalising the pd_data 
od<-out_data 											# Assigning to temp variable for easy usage
od														# Display the data

myplot <- getPlot(od, plot_title, "Day Max Temperature (C)") 	# - Generating the plot based on ARIMA
myplot             												# Display the contents in the screen
savePlot(myplot, "Temp", city_name)								# Save the plot

############ for rainfall prediction
#Removing the variables before processing, may get an warning message if variable not available
remove (wear_dt, wea_rain, wear_ts, exttr, extr_fit, estr_for,pdr_data, outr_data, odr)
wear_dt <- as.Date (wds$DATE_TIME, format ="%Y-%m-%d")    # Getting the date & rainfall to seperate object
wea_rain <- wds$RAINFALLMM
wear_ts <- xts (wea_rain, order.by=wear_dt)
exttr <- wear_ts
# if the maximum rain fall is zero then arima doesn't works, so included else part for handing zero value
max_rain =max(wea_rain)
#max_rain
if ( max_rain > 0){
  print("inside IF")
  extr_fit <- arima(exttr, c(0,0,0)) # fit arima model  
} else {
  print("inside else")
  xreg_w = fourier(wear_ts, K=0)
  extr_fit <- auto.arima(exttr, c(0,0,0), xreg=xreg_w) # fit arima model
  xreg_w
}
estr_for <- forecast(extr_fit, h=10, npaths = 5) 	# Forecasting based on the ARIMA
pdr_data <- funggcast(wear_ts, estr_for)			# Converting the ARIMA forecast output to DF for ggplot
outr_data <- normalise_pd(pdr_data)					#Normalising the pd_data 
odr<-outr_data										# Assigning to temp variable for easy usage
odr													#Display the data
remove(myplot)

myplot <- getPlot(odr, plot_title, "Day Rainfall (mm)") # - Generating the plot based on ARIMA
myplot             										# Display the contents in the screen
savePlot(myplot, "Rain", city_name)						# Save the plot

#Do the below steps to get data again from WEB for comparison
end_date="2017-05-13"						#Ending date
wea_data_sum <- getSummaryData(city_cd,st_date, end_date, city_name, latlongsl)  #Calling the get summary data functions for the required CITIES.
wds <- data.table(as.Date(wea_data_sum$DATE, "%Y-%m-%d"),wea_data_sum$MAXTEMPERATUREC,wea_data_sum$PRECIPITATIONMM)			#Creating the separate object with requried fields
names(wds)<-c("DATE_TIME","MAX_TEMPERATUREC", "RAINFALLMM")  #assigning the fields
wds				#Display the data
  
 #remove the temp. variables
remove (wea_dt, wea_temp, wea_ts, extt, ext_fit, est_for,pd_data, out_data, od)
remove (wear_dt, wea_rain, wear_ts, exttr, extr_fit, estr_for,pdr_data, outr_data, odr,xreg_w)
remove (latlongsl,city_name,end_date,st_date, ptime, plot_title,city_cd, myplot)
