# Get data
url <- "https://github.com/owid/covid-19-data/archive/refs/heads/master.zip"
download.file(url = url, destfile = "covid_data")
unzip("covid_data")
