
# 'Define a function to get Data from google query' 

get_data <- function(station_number, project,table){
 
   library(bigrquery)
  #library( useLegacySql)
  
  if ( missing(project))
  {project <- "erudite-poetry-191706"} else
  {project <- project }
  
  db <- paste('`',project,'.',table,'`',sep = "")
  
  if (station_number=="all")
    {
    
    sql <- paste("SELECT * ",
                 "FROM",
                 db
                 ,"ORDER BY","PM_STATION", ", time_date")}   
  else
    
  # station code 
   { sql <- paste("SELECT * ",
                 "FROM",
                          db,"WHERE PM_STATION="
                 ,station_number,"ORDER BY", "time_date")}
  res <- query_exec(sql, project = project, use_legacy_sql = FALSE,max_pages = Inf)}

#1hrlead_final_03122018