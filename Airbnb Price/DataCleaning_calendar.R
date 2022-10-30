#master file 
library(dplyr)
library(readr)
library(dplyr)
library(tidyr)

df <- list.files("C:/Users/Admin/Downloads/drive download/Calendar December 2021",
                 "C:/Users/Admin/Downloads/drive download/Calendar September 2021",
                 "C:/Users/Admin/Downloads/drive download/Calendar March 2022",
                 "C:/Users/Admin/Downloads/drive download/Calendar June 2022") %>%
  lapply(read_csv) %>%
  bind_rows()

df

# import and clean up the data for calendar
cal <- read.csv("C:/Users/Admin/Downloads/master_file.csv")
summary(cal)

sapply(cal, class)

#transform date from character to date type data

cal$date <- as.Date(cal$date, format = "%Y-%m-%d-%h-%m-%s")
cal$date <- as.Date(c(cal$date))

#drop the $ sign and change price and adjusted price to numeric type
cal$price <- as.numeric(gsub("\\$", "", cal$price))

cal$adjusted_price <- as.numeric(gsub("\\$", "", cal$adjusted_price))

#drop rows with NAs in price and adjusted_price
sum(is.na(cal$price))
length(cal$price)

sum(is.na(cal$adjusted_price))
length(cal$adjusted_price)

new_cal <- cal[complete.cases(cal), ]
na.omit(cal$adjusted_price)
drop_na(cal)

sum(is.na(cal$adjusted_price))
length(cal$adjusted_price)

# create new date, month, year
library(lubridate)
cal$date <- mdy_hm(cal$date)
cal$date <- as.Date(cal$date)

# Create dummy variable available, from t/f to 1/0
cal$available_dummy <- ifelse(cal$available == "t", 1, 0)
cal$available_dummy
