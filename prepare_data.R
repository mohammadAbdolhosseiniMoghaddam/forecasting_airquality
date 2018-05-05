
# this code preprocess data 

prepare_data <- function (df,start_year, end_year,trainingset=0.8) {
  #check for producibility of results
  #filter date
  df<- df%>%filter( year(time_date)<end_year+1 & year(time_date) > start_year-1 ) 
  set.seed(800)
  #remove time stamp from data and station number and na for neighbours
  df [,grep("neighbour_Wind_direction",names(df))] <- 
    lapply(df[,grep("neighbour_Wind_direction",names(df))],factor)
  
  # df [,grep("neighbour_in_wind_direction_1hrlead",names(df))] <- 
  #   lapply(df[,grep("neighbour_in_wind_direction_1hrlead",names(df))],factor)
  
  df$neighbour_in_wind_direction_1hrlead <- as.factor(df$neighbour_in_wind_direction_1hrlead)
  
  df <- df[,-grep("PM_STATION",names(df))]
  df <- df[,-grep("time_date",names(df))]
  df <- df[,-grep("VISIBILITY",names(df))]
  df <- df[ , !names(df)%in% c("VISIBILITY","time_date")]
  #remove nas
  df[is.na(df)] <- 0
  
  # split data to training and testing 
  training_set <- sample(1:nrow(df),size = floor(trainingset*nrow(df)))
  
  return(list(df_training=df[training_set,],df_testing=df[-training_set,]))
}