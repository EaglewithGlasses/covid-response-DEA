library(readr)
library(lubridate)
library(dplyr)
library(xlsx) 
library(Benchmarking)
library(ggplot2)
theme_set(theme_bw())
library(PerformanceAnalytics)
library(ggcorrplot)
library(writexl)
library(sf)

# Get data
url <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
download.file(url = url, destfile = "covid_data.csv")


# data

covid_data <- read_csv("covid_data.csv")
covid_data
colnames(covid_data)

# days <- unique(covid_data$date)
# weeks <- isoweek(ymd(days))
# print(weeks)
# months <- month(ymd(days))
# years <- year(ymd(days))

# Getting unique weeks, so that they don't repeat in different years
# j <- 1
# for(i in 2:length(weeks)){
#   if(weeks[i-1] != weeks[i]){
#     j <- j+1
#   } 
#   weeks[i] <- j
# }
# 
# # Getting unique months, so that they don't repeat in different years
# j <- 1
# for(i in 2:length(months)){
#   if(months[i-1] != months[i]){
#     j <- j+1
#   } 
#   months[i] <- j
# }


locations <- unique(covid_data$location)


# databyday <- data.frame()
# for(i in 1:length(days)){
#     databyday<- covid_data[covid_data$date == days[i],]
#     print(databyday)
# }
# 
# databyweek <- data.frame()
# for(i in 1:length(unique(weeks))){
#   week <- days[weeks == i]
#   databyweek<- covid_data[covid_data$date %in% week,]
#   print(databyweek)
# }
# 
# databymonth <- data.frame()
# for(i in 1:length(unique(months))){
#   month <- days[months == i]
#   databymonth<- covid_data[covid_data$date %in% month,]
#   print(databymonth)
# }


# Selecting columns that have enough information
nas <- covid_data %>% summarise(across(everything(), ~ sum(is.na(.))))
# View(nas)
selectedCols <- colnames(nas %>% select_if(~any(. > 100000)))
# selectedCols
#Deleting smoothed and not relative to population size columns
# covid_data <- covid_data %>% select(-one_of('total_cases', 'new_cases', 'new_cases_smoothed', 'total_deaths', 'new_deaths', 'new_deaths_smoothed', 'new_cases_smoothed_per_million',
#                               'new_deaths_smoothed_per_million', 'icu_patients', 'hosp_patients', 'weekly_icu_admissions', 'weekly_hosp_admissions', 'total_tests', 'new_tests',
#                               'new_tests_smoothed', 'new_tests_smoothed_per_thousand', 'total_vaccinations', 'people_vaccinated', 'people_fully_vaccinated', 'total_boosters',
#                               'new_vaccinations', 'new_vaccinations_smoothed', 'new_vaccinations_smoothed_per_million','new_people_vaccinated_smoothed', 
#                               'new_people_vaccinated_smoothed_per_hundred'))
covid_data <- covid_data %>% select(-one_of(selectedCols))
colnames(covid_data)
View(covid_data)

#Selection of valid data (Rows with information in all columns)
covid_data <- covid_data[complete.cases(covid_data), ]
covid_data
write.xlsx( covid_data,"Covid_data.xlsx")
View(covid_data)


# Simple initial model with aggregate information (Latest totals)
mod1 <- covid_data %>%
  select(location, date, population, total_cases, total_deaths) %>%
  group_by(location) %>%
  filter(date == max(date)) %>%
  na.omit
  
# View(mod1)

#DEA
x <- with(mod1, cbind(population, total_cases)) #Inputs
y <- mod1$total_deaths #Outputs


# VRS model
Countries <- dea(x, y, SLACK = TRUE, DUAL = TRUE, RTS = "vrs", ORIENTATION = "out")
DeaResults <- data.frame(mod1$location, Countries$eff, Countries$lambda)
comps <- DeaResults[,3:ncol(DeaResults)]
#View(comps)
spacer <- ": "
Notefficient <- length(which(DeaResults$Countries.eff != 1))

#Comparison countries
for(country in 1:Notefficient){
  if(DeaResults$Countries.eff[country]!= 1){
    aux <- character()
    inds <- which(comps[country,] != 0)
    aux <- paste( mod1$location[inds], sep = ", ")
    cat(mod1$location[country],": ")
    cat(toString(aux),"\n")
  }
}
colnames(DeaResults)[1] ="name"

write_xlsx(DeaResults, 'DeaResults.xlsx')
# Results
View(DeaResults)

# CRS model
Countries <- dea(x, y, SLACK = TRUE, DUAL = TRUE, RTS = "crs", ORIENTATION = "out")
DeaResults <- data.frame(mod1$location, Countries$eff, Countries$lambda)
comps <- DeaResults[,3:ncol(DeaResults)]
#View(comps)
spacer <- ": "
Notefficient <- length(which(DeaResults$Countries.eff != 1))

#Comparison countries
for(country in 1:Notefficient){
  if(DeaResults$Countries.eff[country]!= 1){
    aux <- character()
    inds <- which(comps[country,] != 0)
    aux <- paste( mod1$location[inds], sep = ", ")
    cat(mod1$location[country],": ")
    cat(toString(aux),"\n")
  }
}

write_xlsx(DeaResults, 'DeaResults.xlsx')
# Results
View(DeaResults)

# map
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
joined <- left_join(world, DeaResults, by='name')
View(select(joined, name, Countries.eff))

ggplot(data = world) +
  geom_sf(aes(fill = joined$Countries.eff)) +
  scale_fill_viridis_c(option = "plasma", trans = "log2") +
  labs(title="Efficiency of countries in logarithmic scale",fill = "Efficiency") 


