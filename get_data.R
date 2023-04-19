# Get data
url <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
download.file(url = url, destfile = "covid_data.csv")


# data
library(readr)
library(lubridate)
covid_data <- read_csv("covid_data.csv")


days <- unique(covid_data$date)
weeks <- isoweek(ymd(days))
print(weeks)
months <- month(ymd(days))
years <- year(ymd(days))

# Getting unique weeks, so that they don't repeat in different years
j <- 1
for(i in 2:length(weeks)){
  if(weeks[i-1] != weeks[i]){
    j <- j+1
  } 
  weeks[i] <- j
}

# Getting unique months, so that they don't repeat in different years
j <- 1
for(i in 2:length(months)){
  if(months[i-1] != months[i]){
    j <- j+1
  } 
  months[i] <- j
}
unique(months)

locations <- unique(covid_data$location)
locations


databyday <- data.frame()
for(i in 1:length(days)){
    databyday<- covid_data[covid_data$date == days[i],]
    databyday
}

databyweek <- data.frame()
for(i in 1:length(unique(weeks))){
  week <- days[weeks == i]
  databyweek<- covid_data[covid_data$date %in% week,]
  databyweek
}

databymonth <- data.frame()
for(i in 1:length(unique(months))){
  month <- days[months == i]
  databymonth<- covid_data[covid_data$date %in% month,]
  databymonth
}
