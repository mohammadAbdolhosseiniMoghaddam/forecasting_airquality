 
# developing Arima model reference: https://robjhyndman.com/hyndsight/tscv/ $
#data:dataframe
#attr: attribute
#sitenumber: sitestation code
#h:time to be forecasted

library(forecast) 
library(dplyr)
Ar_base <- function(data,attr,sitenumber,h,start_year,end_year) {
  
  

  
  #save error for each month
  error.month <- matrix(NA,12,end_year-start_year+1)
  
  years <- start_year:end_year ; months <- 1:12
  
  # set counter 
  k <- 0
  
  for ( i in years) {
    
    k <- k+1
    
    for ( j in months){
      
      
      
      timeseries <- data %>% filter(PM_STATION == sitenumber & year(time_date) == i & month(time_date) == j)  
      
      # find the longest continous chunck https://stackoverflow.com/questions/37447114/find-the-longest-continuous-chunk-of-true-in-a-boolean-vector
      bool <- c(FALSE,diff(timeseries$time_date )==1)
      
      myRle <- rle(bool)$length
      
      uk <- rep(myRle == max(myRle), myRle)
      
      start_day <- which(uk==TRUE)[1]; end_day <- tail(which(uk==TRUE),n=1)
      
      #use data in the longest chunk
      timeseries <- select( timeseries[start_day:end_day, ],attr)
      
      #convert to time series
      timeseries <- ts(timeseries)
      
      #develope a function for the forecasting a time seriesSI
      f_ <- function(t,h){forecast(auto.arima(t,seasonal = TRUE),h=h)}
      
      
      #do time series cross validation
      e <- tsCV(timeseries,f_,h )  
      
      #calculate error fo
      error.month[j,k] <- sqrt(mean(e^2,na.rm = TRUE))
      
      #sum the row
      
    }
  }
  rowMeans(error.month,na.rm = TRUE)
}
