# Title: Recruitment Challenge 2020
# Author: Elizabeth Kay
# Date last edited: 20/5/2020
# Manager: Steve Buckman
# Deliverables:
#  - 2020 predicted demand for FY (section 5) - DONE
#  - Daily electricity demand for past 4 weeks (exported in section 4)
#  - Visualisation of electricity demand over specified period to historical average (using functions)

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

# Import historical annual data
QLD_2015 <- fread("QLD_Demand_2015.csv")
QLD_2016 <- fread("QLD_Demand_2016.csv")
QLD_2017 <- fread("QLD_Demand_2017.csv")
QLD_2018 <- fread("QLD_Demand_2018.csv")
QLD_2019 <- fread("QLD_Demand_2019.csv")

# Download daily data in for loop and combine

# USE DATA TABLES 

#REMOVE FOR LOOP AND USE LAPPLY AND FUNCTION - FUNCTION PROVIDES ONE DATE AND THEN LAPPLY PROVIDES MULTIPLE DAYS
for(i in 1:60) {
  startdate <- as.Date("2020-03-21")
  currdate <- startdate + i
  nextcurrdate <- currdate + 1
  date = as.character(as.Date(currdate,"%m/%d/%Y"), "%Y%m%d") # beginning date will be 1 + yyyymmdd
  date1= as.character(as.Date(nextcurrdate,"%m/%d/%Y"), "%Y%m%d")
  
  data_name <- paste("data", date, sep="_") # define dataset name for output
  
  # Some files end in 044000 and some in 044001, so test the link for 044000 (most common) using the try function
  testfile = paste("PUBLIC_ACTUAL_OPERATIONAL_DEMAND_DAILY_",date,"_",date1,"044000.", sep="")
  testfilepath = paste("http://nemweb.com.au/Reports/CURRENT/Operational_Demand/ACTUAL_DAILY/",testfile,"zip",sep="")
  errorcheck<-try(download.file(testfilepath, tempfile(), mode = "wb", quiet = FALSE))
  
  # If an error occurs when try function attempts to download, reassign file as ending in 044001
  if (class(errorcheck) == 'try-error') {
    file = paste("PUBLIC_ACTUAL_OPERATIONAL_DEMAND_DAILY_",date,"_",date1,"044001.", sep="")
    print("Attempted to download http://....044000.zip now attempting http://....044001.zip.  Ignore previous download error")
    } else {
      file = paste("PUBLIC_ACTUAL_OPERATIONAL_DEMAND_DAILY_",date,"_",date1,"044000.", sep="")
  }
  
  # Define file path and csv file name
  filepath = paste("http://nemweb.com.au/Reports/CURRENT/Operational_Demand/ACTUAL_DAILY/",file,"zip",sep="")
  csvfile<-paste(file,"CSV",sep="")
  
  # Download and unzip csv file
  temp <- tempfile()
  download.file(filepath,temp)
  data <- read.csv(unz(temp, csvfile), skip = 1, nrows = 240, header = TRUE)
  assign(data_name,data)
  unlink(temp)
  
  # Combine daily data
  if (i == 1) {
    Daily <- data
    } else {
      Daily <- rbind(Daily,data)
  }
}


#############################
#       2. Clean Data       #
#############################

#   2.1 Historical Data   

# Combine historical data
QLD_hist<-rbindlist(list(QLD_2015,QLD_2016,QLD_2017,QLD_2018,QLD_2019))

# Generate times for historical data column names
times<-seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by = "30 min") %>%
  format(.,"%H:%M", tz="GMT")
times<-times[-max(NROW(times))]
data_colnames<-c("Year", "Month","Day",times)

# Set historical data column names
colnames(QLD_hist)<-data_colnames


#   2.2 Daily Data  

# Remove 1 from region ID in daily data
Daily$REGIONID <- Daily$REGIONID %>%
  as.character     %>%
  nchar()        %>%
  sum(-1)         %>%
  substr(Daily$REGIONID,1,.)

# Filter on QLD
Daily_QLD <- filter(Daily, Daily$REGIONID == "QLD")

# Extract times from daily data in same format
Daily_QLD_times<- Daily_QLD$INTERVAL_DATETIME %>%
  as.ITime()                %>%
  strptime("%H:%M")      %>%
  format('%H:%M')

# Extract date from daily data
Daily_QLD_Date<-data.table(as.Date(Daily_QLD$INTERVAL_DATETIME))

# Extract demand from daily data and combine with date and time
Daily_QLD_dem<-data.table(Daily_QLD$OPERATIONAL_DEMAND.1) 
Daily_QLD_demand<-cbind(Daily_QLD_Date,Daily_QLD_times, Daily_QLD_dem)
colnames(Daily_QLD_demand)<-c("Date", "Time", "Demand")

# Transpose dataset to get times as columns
Daily_QLD_fin<-Daily_QLD_demand %>% recast(., Date + variable ~ Time, id.var = c("Time", "Date"))
Daily_QLD_fin<-subset(Daily_QLD_fin,select = -c(variable)) # remove unnecessary variable


#############################
#   4. Export Data to csv   #
#############################

write.csv(QLD_hist,"QLD_historical_data.csv", row.names = FALSE)
write.csv(Daily_QLD_fin,"2020_Daily_Actuals.csv", row.names = FALSE)


#############################
# 5. 2020 predicted demand  #
#############################

# Remove year column (not needed for averaging)
QLD_hist_noyr <- subset(QLD_hist,select = -c(Year))
# Transpose data table
QLD_hist_noyr_t <- melt(QLD_hist_noyr, id.vars = c("Month","Day"), measure.vars = times)
# Summarise data and average
hist_avg <- QLD_hist_noyr_t %>% 
  group_by(Month, Day) %>% 
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
#       4. Averages       #
###########################

# Average historical demand
QLD_hist_demand<-subset(QLD_hist, select = -c(1:3))
QLD_hist_avg <- data.table(colMeans(QLD_hist_demand))
colnames(QLD_hist_avg)<-"Avg"

# Average Daily demand
Daily_QLD_avg <- data.table(colMeans(Daily_QLD_fin[sapply(Daily_QLD_fin, is.numeric)],na.rm = TRUE)) 
colnames(Daily_QLD_avg)<-"Avg"

# Plot Averages
x_values<-rownames(QLD_hist_avg)
ggplot() + geom_point(data=QLD_hist_avg, aes(x=rownames(QLD_hist_avg),y=Avg, color = "red")) +
  geom_point(data = Daily_QLD_avg, aes(x=rownames(Daily_QLD_avg),y=Avg, na.rm=TRUE,color="blue")) +
  scale_x_discrete("Time", breaks = x_values[seq(1, length(x_values), by = 6)]) +
  scale_color_manual(name = "Legend", values = c("red","blue"), labels = c("Historical averages", "COVID-19 averages")) +
  scale_shape_identity() +
  ylab("Demand") +
  ggtitle("Electricity Demand: Historical vs. COVID-19")


