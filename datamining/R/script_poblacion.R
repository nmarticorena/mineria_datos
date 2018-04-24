setwd("D:/universidad/Septimo Semestre/Mineria de Datos/Proyecto")

library(dplyr)
library(reshape)
library(stringr)
poblacion <- read.csv("population.csv",check.names=FALSE)
gtd <- read.csv("gtd.csv")
gtd_geo=gtd[,c(0, 1, 2, 3, 8,9, 11, 13, 14, 15, 35, 84, 99, 102)]

poblacion$Country_Name <- as.character(poblacion$Country_Name)
gtd_geo$country_txt <- as.character(gtd_geo$country_txt)




poblacion$`Country Code` <- as.character(poblacion$`Country Code`)
poblacion <- poblacion[, -c(2,3,4)]
poblacion_2 <- melt(poblacion, id.vars = "Country_Name", variable_name = "iyear")
colnames(poblacion_2)[3] <- "population"
poblacion_2$iyear <- as.integer(as.character(poblacion_2$iyear))

colnames(poblacion_2)[1] <- "country_txt"

merge_data <- left_join(gtd_geo, poblacion_2, by = c("country_txt", "iyear"))

poblacion_2$country_txt <- str_replace(poblacion_2$country_txt,"Venezuela, RB", replacement = "Venezuela")


list_country_wb <- unique(poblacion_2$country_txt)
list_country_gtd <- unique(gtd_geo$country_txt)

merge_data_2=transform(merge_data,muertos_poblacion=(nkill/population)*100)
