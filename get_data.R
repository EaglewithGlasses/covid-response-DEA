# Get data
url <- "https://github.com/owid/covid-19-data/archive/refs/heads/master.zip"
download.file(url = url, destfile = "covid_data")
unzip("covid_data")
# data
library(readr)
excess_mortality <- read_csv("covid-19-data-master/public/data/excess_mortality/excess_mortality.csv")
View(excess_mortality)
covid_hospitalizations <- read_csv("covid-19-data-master/public/data/hospitalizations/covid-hospitalizations.csv")
View(covid_hospitalizations)
full_data <- read_csv("covid-19-data-master/public/data/jhu/full_data.csv")
View(full_data)
covid_testing_latest_data_source_details <- read_csv("covid-19-data-master/public/data/testing/covid-testing-latest-data-source-details.csv")
View(covid_testing_latest_data_source_details)
covid_testing_all_observations <- read_csv("covid-19-data-master/public/data/testing/covid-testing-all-observations.csv")
View(covid_testing_all_observations)
vaccinations <- read_csv("covid-19-data-master/public/data/vaccinations/vaccinations.csv")
View(vaccinations)