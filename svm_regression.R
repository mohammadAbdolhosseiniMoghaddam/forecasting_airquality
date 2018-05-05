
library(e1071)
library(dplyr)



svm_regression <-  function( df, monthly='yes', start_year, end_year,kernel_fun = 'radial') {
 # start_time <- Sys.time()
  
  a=prepare_data(df,start_year, end_year)
  
  df_train <- a$df_training
 # df_train$neighbour_in_wind_direction_1hrlead <- as.factor(df_train$neighbour_in_wind_direction_1hrlead )
  df_test <- a$df_testing
 # df_test$neighbour_in_wind_direction_1hrlead <- as.factor(df_test$neighbour_in_wind_direction_1hrlead )
  
  #find the best hyper parameter
  tc <- tune.control(cross = 2)
 tune.out <- tune(method = svm,PM25 ~. ,data = df_train, kernel= kernel_fun, 
                  ranges = list(epsilon = c(0.01,0.0001),cost = c(1,100)),tunecontrol = tc)
  
 
  #tune.out = svm(PM_3hrlead ~. ,data = df_train,kernel= 'radial', epsilon = c(0.01),cost = c(1))
  prediction.svm <- predict(tune.out$best.model,df_test)
 #make support vectors 
  if (monthly =='no'){

    
    er <-  sqrt(mean((prediction.svm-df_test$PM25)^2 ))
    predicts <- prediction.svm
    measures <- df_test$PM25
    correlation_m <- cor(predicts,measures)
    cr = correlation_m^2 
  }
  else {
    
    month.error <- matrix(data = NA,nrow = 12,ncol = 1)
    month.r <- matrix(data = NA,nrow = 12,ncol = 1)
    month.cr <- matrix(data = NA,nrow = 12,ncol = 1)
    #save prediction in each month 
    predicts <- c()
    
    measures <- c()
    
    for ( i in 1:12) {
      
      #find indexes for the same month data
      # df_month <- filter(df_test, month == i)
       
      prediction.month_index <- which(df_test$month == i)
      df_month <- df_test [ prediction.month_index,]
      prediction.month <- prediction.svm[prediction.month_index]
      error <- sqrt(mean((prediction.month-df_month$PM25)^2))
      month.error [i] <- error
      measures <- append(measures, df_month$PM25)
      predicts <- append(predicts, prediction.month )
      correlation_m <- cor(prediction.month ,df_month$PM25)
      month.cr[i] <- correlation_m^2
      
    }
    
    
    er = month.error
    cr =month.cr
  }
  #end_time <- Sys.time()
 # k=end_time - start_time
  
  return(list(prediction = predicts , testSet = measures, error_value = er,correlation = cr ) ) 
  
  
  
  
}