# Title: Recruitment Challenge 2020
# Author: Elizabeth Kay
# Date created: 20/5/2020
# Date last edited: 21/5/2020
# Manager: Steve Buckman
# Deliverables:
#  - 2020 predicted demand for FY (section 6)
#  - Daily electricity demand for past 4 weeks (exported in section 4)
#  - Function to visualise electricity demand over specified period to historical average (section 7)


#############################
#     0. Initialisation     #
#############################

setwd("~/OneDrive - UNSW/UNSW/2020/Synengco/recruitment-challenge-2020-master/data")

# Packages
library(dplyr)
library(data.table)
library(tidyverse)
library(lubridate) 
library(reshape2) 
library(ggplot2)
library(magrittr)


#############################
#      1. Import Data       #
#############################

#      1.1 Historical Data

# Import historical annual data
QLD_2015 <- fread("QLD_Demand_2015.csv")
QLD_2016 <- fread("QLD_Demand_2016.csv")
QLD_2017 <- fread("QLD_Demand_2017.csv")
QLD_2018 <- fread("QLD_Demand_2018.csv")
QLD_2019 <- fread("QLD_Demand_2019.csv")


#      1.2 Daily Data

# Create function that reads in date's daily data as data.table
get_demand <- function(date) {
  
  # Define dates
  curr_date <- as.Date(date)
  next_date <- curr_date + 1

  # Reformat as characters for file path
  curr_date_f <- as.character(as.Date(curr_date,"%m/%d/%Y"), "%Y%m%d")
  next_date_f <- as.character(as.Date(next_date,"%m/%d/%Y"), "%Y%m%d")
  
  data_name <- paste("data", curr_date_f, sep="_") # define dataset name for output
  
  # Define file and file path
  file <- paste("PUBLIC_ACTUAL_OPERATIONAL_DEMAND_DAILY_",curr_date_f,"_",next_date_f,"044000.", sep="")
  filepath <- paste("http://nemweb.com.au/Reports/CURRENT/Operational_Demand/ACTUAL_DAILY/",file,"zip",sep="")

  # Some files end in 044000 and some in 044001, so test the link for 044000 (most common) using the try function
  errorcheck <- try(download.file(filepath, tempfile(), mode = "wb", quiet = FALSE))
  
  # If an error occurs when try function attempts to download, reassign file as ending in 044001
  if (class(errorcheck) == 'try-error') {
    
    file <- paste("PUBLIC_ACTUAL_OPERATIONAL_DEMAND_DAILY_",curr_date_f,"_",next_date_f,"044001.", sep="")
    
    # Redefine file path
    filepath <- paste("http://nemweb.com.au/Reports/CURRENT/Operational_Demand/ACTUAL_DAILY/",file,"zip",sep="")
    
    # Print explanation about why download did not work
    print("Attempted to download http://....044000.zip now attempting http://....044001.zip.  Ignore previous download error.")
  } 
  
  # set fread unzip input
  unzip <- paste("curl ",filepath," | funzip", sep="")
  
  # Read in as data table
  data <- fread(cmd = unzip, skip = 1, nrows = 240, header = TRUE)
  
  # Rename to unique data name
  assign(data_name, data, envir = parent.frame())
  
}

# Create function that can generate daily data for one or more days, finish_date default 1 if no finish date is supplied
date_range <- function(start_date,finish_date=1){
  
  # If no finish date is supplied
  if(finish_date==1){
    data_comb <- get_demand(start_date)
    assign("Daily", data_comb, envir = parent.frame())
  
    } else {
  
    # If finish date is supplied
    date_list <- (date = seq(from = as.Date(start_date), to = as.Date(finish_date), by = 1)) # generate list of dates
    # combine data.tables 
    data_comb <- lapply(date_list,get_demand) %>% rbindlist
    # Assign name to combined data.table
    assign("Daily", data_comb, envir = parent.frame())
    
    }
}

# For our second deliverable we want to generate four weeks of data
date_range("2020-04-22","2020-05-20") # (start date, finish date) format as yyyy-mm-dd



#############################
#       2. Clean Data       #
#############################

#   2.1 Historical Data   

# Combine historical data
QLD_hist <- rbindlist(list(QLD_2015,QLD_2016,QLD_2017,QLD_2018,QLD_2019))

# Generate times for historical data column names
times <- seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by = "30 min") %>%
         format(.,"%H:%M", tz="GMT")
times <- times[-max(NROW(times))]
data_colnames <- c("Year", "Month","Day",times)

# Set historical data column names
colnames(QLD_hist) <- data_colnames


#   2.2 Daily Data  

# Data.tables allow for identically named columns so make column names unique
setnames(Daily, make.names(names = names(Daily), unique=TRUE))

# Remove 1 from region ID in daily data
Daily$REGIONID <- Daily$REGIONID %>%
  nchar()                        %>%
  -1                             %>%
  substr(Daily$REGIONID,1,.)

# Filter on QLD
Daily_QLD <- filter(Daily, Daily$REGIONID == "QLD")

# Extract times from daily data in same format
Daily_QLD_times <- Daily_QLD$INTERVAL_DATETIME %>%
  as.ITime()                                   %>%
  strptime('%H:%M')                            %>%
  format('%H:%M')

# Extract date from daily data
Daily_QLD_Date <- data.table(as.Date(Daily_QLD$INTERVAL_DATETIME))

# Extract demand from daily data and combine with date and time
Daily_QLD_dem <- data.table(Daily_QLD$OPERATIONAL_DEMAND.1) 
Daily_QLD_demand <- cbind(Daily_QLD_Date, Daily_QLD_times, Daily_QLD_dem)
colnames(Daily_QLD_demand) <- c("Date", "Time", "Demand")

# Transpose dataset to get times as columns
Daily_QLD_fin <- recast(Daily_QLD_demand, Date + variable ~ Time, id.var = c("Time", "Date"))
Daily_QLD_fin <- subset(Daily_QLD_fin,select = -c(variable)) # remove unnecessary variable


#############################
#   4. Export Data to csv   #
#############################

write.csv(QLD_hist,"QLD_historical_data.csv", row.names = FALSE)
write.csv(Daily_QLD_fin,"2020_Daily_Actuals.csv", row.names = FALSE)


#############################
#   5. Reimport csv files   #
#############################

# going forward code is not dependent on data generated above
four_weeks <- fread("2020_Daily_Actuals.csv")
hist <- fread("QLD_historical_data.csv")


#############################
# 6. 2020 predicted demand  #
#############################

# Remove year column (not needed for averaging)
QLD_hist_noyr <- subset(hist,select = -c(Year))

# Transpose data table
QLD_hist_noyr_t <- melt(QLD_hist_noyr, id.vars = c("Month","Day"), measure.vars = times)

# Summarise data and average
hist_avg <- QLD_hist_noyr_t %>% 
  group_by(Month, Day)      %>% 
  summarise(Pred_value = mean(value))

# Remove Feb 29 since FY2020 is a non-leap-FY
hist_avg <- data.table(hist_avg[!(hist_avg$Month==2 & hist_avg$Day==29),])

# Add year
hist_avg$Year <- ifelse(hist_avg$Month <=6, 2021, 2020)
# Sort by year
hist_avg <- setkey(hist_avg,Year, Month,Day) 
# Set column order
setcolorder(hist_avg, c("Year","Month","Day","Pred_value")) 

# Explort deliverable as csv file
write.csv(hist_avg,"2020_Predicted_Values.csv", row.names = FALSE)




###########################
#    7. Visualisation     #
###########################


visualise <- function(start_date,finish_date=1){
  
  # Generate historical aveages (could be done outside function)
  QLD_hist_demand <- subset(QLD_hist, select = -c(1:3))
  QLD_hist_avg <- data.table(colMeans(QLD_hist_demand))
  colnames(QLD_hist_avg) <- "Avg"
  
  if(finish_date==1){
    
    # If no finish date is provided, then only extract start_date's row
    data_sel <- four_weeks[Date == start_date]
    
    # Since there is only one operation, transpose taking only the numeric demand values
    data_sel_avg <- data_sel       %>%
      sapply(is.numeric)           %>%
      data_sel[,., with = FALSE]   %>%
      t()                          %>%
      data.table()
    colnames(data_sel_avg)<-"Avg"

    } else{
    
      # If a finish_date is supplied then take a subset of the data for date range inclusive
      data_sel <- four_weeks[Date >= start_date & Date <= finish_date]
      
      # Take the average for the half hour intervals
      data_sel_avg <- data_sel       %>%
        sapply(is.numeric)           %>%
        data_sel[,.,, with = FALSE]  %>%
        colMeans(na.rm=TRUE)         %>%
        data.table()
      colnames(data_sel_avg)<-"Avg"
      
      }
  
  # Plot Averages
  x_values<-rownames(QLD_hist_avg)
  ggplot() + geom_point(data=QLD_hist_avg, aes(x=rownames(QLD_hist_avg),y=Avg, color = "red")) +
    geom_point(data = data_sel_avg, aes(x=rownames(data_sel_avg),y=Avg, na.rm=TRUE,color="blue")) +
    scale_x_discrete("Time", breaks = x_values[seq(1, length(x_values), by = 6)]) +
    scale_color_manual(name = "Legend", values = c("red","blue"), labels = c("Historical averages", "Select period averages")) +
    scale_shape_identity() +
    ylab("Demand") +
    ggtitle("Electricity Demand: Historical vs. Selected")
  
  
}

visualise("2020-04-23","2020-05-21")







