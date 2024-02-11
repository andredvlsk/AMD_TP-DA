install.packages("rvest")
library(rvest)
library(strigr)
library(lubridate)
library(dplyr)

url <- "https://en.wikipedia.org/wiki/Men%27s_high_jump_world_record_progression"
page <- read_html(url)
tablas <- html_table(html_elements(page, "table"))
raw <- as.data.frame(tablas[3])
summary(raw)
head(raw)

meters <- str_sub(raw$Mark, 1, 4)
head(meters)
class(meters)
meters <- as.numeric(meters)

country <- str_sub(raw$Athlete, -4, -2)
head(country)

athlete <- str_sub(raw$Athlete, 1, -6)
head(athlete)
athlete <- str_trim(athlete)
athlete <- str_to_upper(athlete)

dates <- raw$Date
head(dates)
dates <- str_replace(dates, "\\[[1-9]\\]", "")
class(dates)

dates <- dmy(dates)
dates

year <- year(dates)
month <- month(dates)
day <- day(dates)

record_time_elapsed <- year(today()) - year(dates)
head(record_time_elapsed)

clean_data <- data.frame("Record" = meters,
                         "Athlete" = athlete,
                         "Country" = country,
                         "Record date" = dates,
                         "Record year" = year,
                         "Record month" = month,
                         "Record day" = day,
                         "Record time elapsed" = record_time_elapsed,
                         "City" = raw$Venue)

clean_data

library(dplyr)

info <- clean_data %>% mutate("Multiple Records" = ifelse(duplicated(Athlete), TRUE, FALSE)) %>%
        select(Record, Athlete, Record.year, "Multiple Records", Country)

info <- clean_data %>% select(Record, Athlete, Record.year, Country) %>%
        filter(Record >= 2.30) %>%
        group_by(Country) %>%
        summarise("MaxRecord" = max(Record), "N of Records" = n()) %>%
        arrange(desc(MaxRecord))

info 

##### ggplot2 #####

library(ggplot2)
install.packages("mlbench")
library(mlbench)

?mpg
head(mpg, n=5)

#Histograma
ggplot(mpg, aes(x = cyl)) + 
  geom_histogram(binwidth = 2, color = "red", fill = "blue") +
  labs(title = "Histograma: Coches por cyl",
       x = "Cyl", 
       y = "Num coches")

#Grafico de barras
ggplot(mpg, aes(x = class) )+
  geom_bar(aes(fill = manufacturer)) +
  labs(title = "Dist coches por clase",
       x = "Clase", 
       y = "Num coches") +
  theme(panel.background = element_blank()) +
  theme(plot.title = element_text(color = "red")) +
  theme(legend.position = "none")

#Boxplot
mpg$trans <- mpg$trans %>% str_sub(1, -5)

ggplot(mpg, aes(x = trans, y = cty)) +
  geom_boxplot(aes(fill = trans)) +
  labs(title = "Boxplot: Consumo por tipo de transmisión",
       x = "Transmisión", 
       y = "Consumo") +
  theme(panel.background = element_blank()) +
  theme(legend.position = "none")

#Dispersión

ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point(aes (color = trans, size = cyl))

#facetado
ggplot(mpg, aes(x = cyl, y = hwy)) +
  geom_point(aes (color = class, size = cyl)) +
  facet_wrap(~trans)


#RETO GUIADO ANALISIS ESTADISTICO

data(package = "mlbench", PimaIndiansDiabetes2)

head(PimaIndiansDiabetes2, n = 10)

mean(PimaIndiansDiabetes2$glucose, na.rm = TRUE)
sd(PimaIndiansDiabetes2$glucose, na.rm = TRUE)

summary(PimaIndiansDiabetes2$glucose)

table(PimaIndiansDiabetes2$diabetes)
prop.table(table(PimaIndiansDiabetes2$diabetes))

summary(PimaIndiansDiabetes2)

hist(PimaIndiansDiabetes2$glucose)

t <- t.test(glucose ~ diabetes, data = PimaIndiansDiabetes2)
print(t)

library(dplyr)
datos <- PimaIndiansDiabetes2 %>% na.omit()
cor(datos$glucose, datos$pressure)


#utilizando mtcars
data(mtcars)

head(mtcars, n=5)

summary(mtcars)

clean <- mtcars %>% select(mpg, cyl, disp, hp, wt, am) %>%
  mutate(am = factor(am, levels = c(0,1), labels = c("Automatico", 'Manual')), 
         cyl = as.factor(cyl))

summary(clean)

correlation <- cor(clean[,c("mpg", "disp", "hp", "wt")])
correlation
pairs(clean[,c("mpg", "disp", "hp", "wt")])

library(ggplot2)

ggplot(data = clean, aes(x = am, y = mpg, fill = am)) +
  geom_boxplot() +
  labs(title = "Relación entre consumo y tipo de transmisión", x = "Tipo de transmisión",
       y = "Consumo (millas por galón")

ggplot(data = clean, aes(x = cyl, y = mpg, fill = cyl)) +
  geom_boxplot() +
  labs(title = "Relación entre consumo y tipo de transmisión", x = "Numero de cilidros",
       y = "Consumo (millas por galón")


t <- t.test(mpg ~ am, data = clean)
print(t)


anova <- aov(mpg ~ cyl, data = clean)
summary(anova)


model <- lm(mpg ~ hp + disp + wt, data = clean)
summary(model)
