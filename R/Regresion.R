?diamonds

#regresión lineal

modelo <- lm(price ~ carat, data = diamonds)
#equación y = ax + b
#price = 7756 * carat - 2256

summary(modelo)

ggplot(diamonds, aes(x = carat, y=price)) +
  geom_point() +
  geom_smooth(method = 'lm', color = "red", se=FALSE)

#se = FALSE quita las bandas del intervalo de confianza del grafico.

#regresión logistica
#prob de una variable cualitativa en función de cuatitativas
#ej. si un paciente tendrá una enfermedad x en función de ciertas variables de diagnostico.
#probabilidad de pertenencia a una clase especifica.

library(dplyr)
data("PimaIndiansDiabetes2", package = "mlbench")
head(PimaIndiansDiabetes2)

diabetes <- na.omit(PimaIndiansDiabetes2)
rm(PimaIndiansDiabetes2)

#regresión logistica multiple
modelo <- glm(diabetes  ~ ., data = diabetes, family = "binomial")
summary(modelo)


#regresión logística simple
modelo <- glm(diabetes  ~ glucose, data = diabetes, family = "binomial")
summary(modelo)

#representar el modelo
diabetes <- diabetes %>% mutate(probabilidad = ifelse(diabetes == "pos",1,0))
head(diabetes, n=5)

ggplot(diabetes, aes(x = glucose, y=probabilidad)) +
  geom_point()+
  geom_smooth(method = 'glm', method.args = list(family = "binomial"))

#realizar predicciones
#predice la probabilidad que pertenezca a esta clase, en este caso, que tenga diabetes
nuevos_datos <- data.frame(glucose = c(80,120,230,90,60))
probs<- predict(modelo, nuevos_datos, type="response")
probs*100


#series temporales
install.packages("forecast")
library(forecast)


?AirPassengers
class(AirPassengers)
head(AirPassengers)
AirPassengers

plot(AirPassengers)

desc <- decompose(AirPassengers, "multiplicative")
plot(desc)

start(AirPassengers)
end(AirPassengers)

trainData <- window(AirPassengers, start = c(1949,1), end = c(1959,12))
testData <- window(AirPassengers, start = c(1960,1), end = c(1960,12))

#haciendo las predicciones en base a los datos, utilizando un modelo ARIMA

modelo <- auto.arima(trainData)

pred <- forecast(modelo, h = length(testData))

plot(pred)

#nivel de acierto del modelo
accuracy(pred, testData)

#otros paquetes para series temporales
install.packages("TSA")
install.packages("stats")
install.packages("xts")
install.packages("zoo")

install.packages("caret")

