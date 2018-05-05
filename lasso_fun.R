# lasso estimation
library(glmnet)
library(lubridate)
library(dplyr)
lasso_estimation <- function(df,monthly="YES",start_year,end_year,interactions="NO")
  
{ # filter out data
  df<- df%>%filter( year(time_date)<end_year+1 & year(time_date) > start_year-1 )
  
  if(interactions== "YES"){
    
    # add first interaction_term
    new_col <- paste0("interaction_term_wind_pm_neighbour",1,"lead")
    df[new_col] <-  df["PM_WIND_NEIGHBOUR_1hrlead"] * df["WIND_VOL"]
    
    # check the number of heading time
    h <- length(grep("RH",names(df1)))
    
    if (h>1) {
      
      for ( i in 2:h) {
      
        wind_col_name <- paste0("WIND_VOL_",i,"hrlead") 
        
        pm_col_name <- paste0("pm_neighbour_","lead",i)
        
        new_col <- paste0("interaction_term_wind_pm_neighbour",i,"lead")
        
        df[new_col ] <- df[wind_col_name] * df[pm_col_name ]
        
      }
         }
    
  }
  
  #remove time stamp from data and station number and na for neighbours
  df <- df[,-grep("PM_STATION",names(df))]
  df <- df[,-grep("time_date",names(df))]
  df <- df[,-grep("VISIBILITY",names(df))]
  df <- df[ , !names(df)%in% c("VISIBILITY","time_date")]
  
  #remove nas
  df[is.na(df)] <- 0
  
  
  #df$month <- month(time_date)
  
  x <- model.matrix(PM25 ~.,df)
  y <- df$PM25
  
  # split data to training and testing 
  training_set <- sample(1:nrow(df),size = floor(0.8*nrow(df)))
  
  # using lasso for the prediction 
  modelfit.lasso <- glmnet(x[training_set ,],y[training_set],alpha =1)
  
  cv.out <- cv.glmnet (x[training_set ,],y[training_set],alpha =1)
  
  if (monthly=="NO"){
    
    # predict value 
    prediction.lasso <- predict(modelfit.lasso, s=cv.out$lambda.min, newx=x[-training_set ,] )
    
    er <-  sqrt(mean((prediction.lasso-y[-training_set])^2 ))
    
    predicts <- prediction.lasso
    
    measures <- y[-training_set]
    
    
    #list(prediction = prediction.lasso, testSet = y[-training_set], error_value = er  )
  } 
  else {
    
    #save error for each month
    error.month <- matrix(NA,12,1)
    
    #save prediction in each month 
    predicts <- c()
    
    measures <- c()
    
    
    for ( j in 1:12)  {
      
      
      #find indexes for the same month data
      ind=row.names(df[-training_set, ])[ which(df[-training_set, ]$month==j,arr.ind = TRUE)]
      
      testing_x <- x[ind, ]
      
      testing_y <- y[as.numeric(ind)]
      
      #measures <- cbind(measures, testing_y )
      measures <- append(measures, testing_y )
      
      prediction.lasso <- predict(modelfit.lasso, s=cv.out$lambda.min, newx = testing_x )
      
      #predicts <- rbind(predicts,prediction.lasso)
      predicts <- append(predicts,prediction.lasso)
      
      error.month[j] <-  sqrt(mean(((prediction.lasso)-testing_y )^2 ))
      
    }
    er = error.month
    
  }
  
  return(list(prediction = predicts , testSet = measures, error_value = er  ) )  }



