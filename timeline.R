# Timeline
library(vistime)
data <- read.csv(text="event,group,start,end,color
                       Inicio,Covid-19,2019-11-17,2019-12-30,#c8e6c9
                       Descubrimiento,Covid-19,2019-12-31,2020-01-06,#c8e6c9
                       Emergencia pública internacional,Covid-19,2020-01-30,2020-01-30,#c8e6c9
                       Pandemia,Covid-19,2020-03-11,2020-03-11,#c8e6c9
                       Esparcimiento global,Covid-19,2020-01-31,2020-10-30,#c8e6c9
                       Primeras vacunas de prueba,Vacunas,2020-11-09,2020-11-09,#c8e6c9
                       Variante Alpha identidficada en Inglaterra,Covid-19,2020-12-14,2020-12-14,#c8e6c9
                       Variante Alpha identidficada en 33 paises,Covid-19,2021-01-02,2021-01-02,#c8e6c9
                       Variante Gamma identidficada en Japón,Covid-19,2020-12-14,2020-12-14,#c8e6c9
                       Prueba de Coronavac con 50.4% de efectividad,Vacunas,2021-01-29,2021-01-29,#c8e6c9
                       Variante Delta identidficada en India,Covid-19,2021-03-01,2021-03-30,#c8e6c9
                       Alemania advierte contra la vacuna de Moderna por Miocarditis,Vacunas,2021-11-10,2021-11-10,#c8e6c9
                       Variante Omicron identidficada en Sudafrica,Covid-19,2021-11-24,2021-11-24,#c8e6c9
                       Más de 100M de casos en Europa,Covid-19,2022-01-01,2022-01-01,#c8e6c9
                       Alrededor del 57% de la población se infectó de Covid-19,Covid-19,2022-01-24,2022-01-24,#c8e6c9
                       Más de 6M de muertes en el mundo,Covid-19,2022-03-06,2022-03-06,#c8e6c9
                       E.U.A con más de 99M de casos,Covid-19,2022-10-21,2022-10-21,#c8e6c9
                       La OMS declara que la pandemia ya no era una emergencia global,Covid-19,2023-05-05,2023-05-05,#c8e6c9
                       La mayoría de los paises retiran las medidas de prevención,Covid-19,2023-05-01,2023-05-30,#c8e6c9")

gg_vistime(data)

# Using Dplyr for data manipulation
