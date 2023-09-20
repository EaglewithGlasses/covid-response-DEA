
library(readr)
library(Benchmarking)
library(ggplot2)
library(dplyr)
library(PerformanceAnalytics)
library(ggcorrplot)
library(writexl)



#DEA
x <- with(SelectedCountries, cbind(total_tests_per_thousand, total_vaccinations_per_hundred, people_vaccinated_per_hundred, people_fully_vaccinated_per_hundred,
                                   stringency_index, population, population_density, median_age, gdp_per_capita,
                                   cardiovasc_death_rate, diabetes_prevalence, hospital_beds_per_thousand,
                                   life_expectancy, human_development_index)) #Inputs

y <- with(SelectedCountries, cbind(healthy_per_million_not_infected, alive_not_dead_per_million)) #Outputs

Countries <- dea(x, y, SLACK = TRUE, DUAL = TRUE, RTS = "crs", ORIENTATION = "in")
DeaResults <- data.frame(SelectedCountries$location, Countries$eff, Countries$lambda)
comps <- DeaResults[,3:ncol(DeaResults)]
View(comps)
spacer <- ": "
Notefficient <- length(which(DeaResults$Countries.eff != 1))

#Comparison countries
for(country in 1:Notefficient){
  if(DeaResults$Countries.eff[country]!= 1){
    aux <- character()
    inds <- which(comps[country,] != 0)
    aux <- paste( SelectedCountries$location[inds], sep = ", ")
    cat(SelectedCountries$location[country],": ")
    cat(toString(aux),"\n")
    
  }
  
}

write_xlsx(DeaResults, 'H:\\ITAM\\Modelado y Optimización\\Linear Programming\\DeaResults.xlsx')
# Results
View(DeaResults)


corelations <- cor(x,y)
colnames(corelations)<- c("Personas Sanas (no infectadas)","Personas vivas (Personas no muertas)" )
corelations

corEfiMeasures <- cor(DeaResults$Countries.eff,y)
colnames(corEfiMeasures)<- c("Personas Sanas (no infectadas)","Personas vivas (Personas no muertas)" )
corEfiMeasures


# Regression against not infected
ggplot(data = SelectedCountries, aes(x = healthy_per_million_not_infected, y = total_tests_per_thousand)) + geom_smooth(method='lm') + geom_point() + ggtitle("No infectados vs pruebas") + labs(y="Pruebas por millón", x = "Personas no infectadas por millón")
ggplot(data = SelectedCountries, aes(x = healthy_per_million_not_infected, y = total_vaccinations_per_hundred)) + geom_smooth(method='lm') + geom_point() + ggtitle("No infectados vs vacunas totales por 100") + labs(y="vacunas totales por 100", x = "Personas no infectadas por millón")
ggplot(data = SelectedCountries, aes(x = healthy_per_million_not_infected, y = people_vaccinated_per_hundred)) + geom_smooth(method='lm') + geom_point() + ggtitle("No infectados vs porcentaje de vacunados") + labs(y="vacunados", x = "Personas no infectadas por millón")
ggplot(data = SelectedCountries, aes(x = healthy_per_million_not_infected, y = people_fully_vaccinated_per_hundred)) + geom_smooth(method='lm') + geom_point() + ggtitle("No infectados vs porcentaje de vacunados totalmente") + labs(y="vacunados totalmente", x = "Personas no infectadas por millón")
ggplot(data = SelectedCountries, aes(x = healthy_per_million_not_infected, y = stringency_index)) + geom_smooth(method='lm') + geom_point() + ggtitle("No infectados vs astringencia") + labs(y="Astringencia", x = "Personas no infectadas por millón")
ggplot(data = SelectedCountries, aes(x = healthy_per_million_not_infected, y = population)) + geom_smooth(method='lm') + geom_point() + ggtitle("No infectados vs población") + labs(y="Población", x = "Personas no infectadas por millón")
ggplot(data = SelectedCountries, aes(x = healthy_per_million_not_infected, y = population_density)) + geom_smooth(method='lm') + geom_point() + ggtitle("No infectados vs densidad de población") + labs(y="Densidad de Población", x = "Personas no infectadas por millón")
ggplot(data = SelectedCountries, aes(x = healthy_per_million_not_infected, y = median_age)) + geom_smooth(method='lm') + geom_point() + ggtitle("No infectados vs edad media") + labs(y="Edad Media", x = "Personas no infectadas por millón")
ggplot(data = SelectedCountries, aes(x = healthy_per_million_not_infected, y = gdp_per_capita)) + geom_smooth(method='lm') + geom_point() + ggtitle("No infectados vs PIB per cápita") + labs(y="PIB per cápita", x = "Personas no infectadas por millón")
ggplot(data = SelectedCountries, aes(x = healthy_per_million_not_infected, y = life_expectancy)) + geom_smooth(method='lm') + geom_point() + ggtitle("No infectados vs esperanza de vida") + labs(y="Esperanza de vida", x = "Personas no infectadas por millón")
ggplot(data = SelectedCountries, aes(x = healthy_per_million_not_infected, y = human_development_index)) + geom_smooth(method='lm') + geom_point() + ggtitle("No infectados vs índice de desarrollo humano") + labs(y="Índice de desarrollo humano", x = "Personas no infectadas por millón")

# Regression against alive
ggplot(data = SelectedCountries, aes(x = alive_not_dead_per_million, y = total_tests_per_thousand)) + geom_smooth(method='lm') + geom_point() + ggtitle("Vivos vs pruebas") + labs(y="Pruebas por millón", x = "Vivos")
ggplot(data = SelectedCountries, aes(x = alive_not_dead_per_million, y = total_vaccinations_per_hundred)) + geom_smooth(method='lm') + geom_point() + ggtitle("Vivos vs vacunas totales por 100") + labs(y="vacunas totales por 100", x = "Vivos")
ggplot(data = SelectedCountries, aes(x = alive_not_dead_per_million, y = people_vaccinated_per_hundred)) + geom_smooth(method='lm') + geom_point() + ggtitle("Vivos vs porcentaje de vacunados") + labs(y="vacunados", x = "Vivos")
ggplot(data = SelectedCountries, aes(x = alive_not_dead_per_million, y = people_fully_vaccinated_per_hundred)) + geom_smooth(method='lm') + geom_point() + ggtitle("Vivos vs porcentaje de vacunados totalmente") + labs(y="vacunados totalmente", x = "Vivos")
ggplot(data = SelectedCountries, aes(x = alive_not_dead_per_million, y = stringency_index)) + geom_smooth(method='lm') + geom_point() + ggtitle("Vivos vs astringencia") + labs(y="Astringencia", x = "Vivos")
ggplot(data = SelectedCountries, aes(x = alive_not_dead_per_million, y = population)) + geom_smooth(method='lm') + geom_point() + ggtitle("Vivos vs población") + labs(y="Población", x = "Vivos")
ggplot(data = SelectedCountries, aes(x = alive_not_dead_per_million, y = population_density)) + geom_smooth(method='lm') + geom_point() + ggtitle("Vivos vs densidad de población") + labs(y="Densidad de Población", x = "Vivos")
ggplot(data = SelectedCountries, aes(x = alive_not_dead_per_million, y = median_age)) + geom_smooth(method='lm') + geom_point() + ggtitle("Vivos vs edad media") + labs(y="Edad Media", x = "Vivos")
ggplot(data = SelectedCountries, aes(x = alive_not_dead_per_million, y = gdp_per_capita)) + geom_smooth(method='lm') + geom_point() + ggtitle("Vivos vs PIB per cápita") + labs(y="PIB per cápita", x = "Vivos")
ggplot(data = SelectedCountries, aes(x = alive_not_dead_per_million, y = life_expectancy)) + geom_smooth(method='lm') + geom_point() + ggtitle("Vivos vs esperanza de vida") + labs(y="Esperanza de vida", x = "Vivos")
ggplot(data = SelectedCountries, aes(x = alive_not_dead_per_million, y = human_development_index)) + geom_smooth(method='lm') + geom_point() + ggtitle("Vivos vs índice de desarrollo humano") + labs(y="Índice de desarrollo humano", x = "Vivos")


# 100% efficiency

x2 <- with(SelectedCountries, cbind(total_tests_per_thousand, total_vaccinations_per_hundred, people_vaccinated_per_hundred, people_fully_vaccinated_per_hundred,
                                    stringency_index, population, population_density, median_age, gdp_per_capita,
                                    cardiovasc_death_rate, diabetes_prevalence, hospital_beds_per_thousand,
                                    life_expectancy, human_development_index))*DeaResults$Countries.eff #Inputs
x2df <- data.frame(x2)
write_xlsx(x2df, 'H:\\ITAM\\Modelado y Optimización\\Linear Programming\\efficiencyInputs.xlsx')
y2 <- with(SelectedCountries, cbind(healthy_per_million_not_infected, alive_not_dead_per_million)) #Outputs

Countries2 <- dea(x2, y2, SLACK = TRUE, DUAL = TRUE, RTS = "crs", ORIENTATION = "in")
DeaResults2 <- data.frame(SelectedCountries$location, Countries2$eff, Countries2$lambda)


# Results
View(DeaResults2)