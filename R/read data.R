install.packages("readxl")
library(readxl)

#si lo tuviera en excel... Y con 2 pestañas "brands" y "cars"
excel_sheets(path = "data/mtcars.xlsx")
datos <- read_excel(path = "data/mtcars.xlsx", sheet = "cars")

head(datos, n = 5)

?mtcars

#utilizando mtcars en vez de datos
automaticos <- mtcars[mtcars$am == 0,]

resumen_at <- automaticos[, c("mpg","cyl","hp","gear")]

#en mtcars no hay "model" directamente
datos[datos$model == "Mazda RX4",]
datos[datos$model == "Mazda RX4",]$gear #para acceder una variable especifica


#Leer de una web.
install.packages("rvest")
library(rvest)

url <- "https://en.wikipedia.org/wiki/Men%27s_high_jump_world_record_progression"

page <- read_html(url)
tables <- html_table(html_elements(page, "table"))
table <- tables[[3]]

class(table)
head(table, n=5)
tail(table, n=5)

table$Mark

ny = table[table$Venue == "New York",]
ny
