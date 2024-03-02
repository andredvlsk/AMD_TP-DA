library(caret)

modelos <- getModelInfo()
names(modelos)

modelLookup("knn")
modelLookup("neuralnet")
getModelInfo("knn")

#70/30, 80/20 80 para entrenar el modelo y 20 para validar

data("mtcars")

set.seed(123)
index <- createDataPartition(mtcars$mpg, p=0.7, list=FALSE)

train_set <- mtcars[index, ]
test_set <- mtcars[-index, ]

modelLookup("lm")
modelLookup("neuralnet")
modelLookup("knn")

#Función de control
ctrl <- trainControl(method = "cv", number = 10, verboseIter = TRUE)


#Regresion lineal

lr_model <- train(mpg~.,
                  data = train_set,
                  method = "lm",
                  trControl = ctrl)


lr_model

lr_predict <- predict(lr_model, test_set)

lr_rmse <- sqrt(mean((lr_predict - test_set$mpg)^2))
lr_rmse


#Using knn

knn_model <- train(mpg~.,
                  data = train_set,
                  method = "knn",
                  trControl = ctrl,
                tuneGrid = expand.grid(k = seq(1,20,1)))


knn_model
knn_predict <- predict(knn_model, test_set)

knn_rmse <- sqrt(mean((knn_predict - test_set$mpg)^2))
knn_rmse


#Red neuronal

nn_model <- train(mpg~.,
                   data = train_set,
                   method = "neuralnet",
                   trControl = ctrl,
                   tuneGrid = expand.grid(layer1 = c(1:3),
                                          layer2 = c(1:3),
                                          layer3 = 0))


nn_model
nn_predict <- predict(nn_model, test_set)

nn_rmse <- sqrt(mean((nn_predict - test_set$mpg)^2))
nn_rmse


# comparar los 3 modelos de regresión

cat("Regresión lineal", lr_rmse, "\n")
cat("KNN", knn_rmse, "\n")
cat("Red Neuronal", nn_rmse, "\n")


head(iris)
summary(iris)

set.seed(568)
index <- createDataPartition(iris$Species, p = 0.8, list = FALSE)
train_set <- iris[index,]
test_set <- iris[-index,]

#probando el knn
#la función de control utiliza el metodo de validación cruzada
#10 iteraciones y que queremos que nos demuestre las 10.
ctrl <- trainControl(method = "cv", number = 10, verboseIter = TRUE)

#~. significa que queremos descubrir la espécie de la flor basandonos en todoso los otros parámetros que tenemos en los datos.
# en el resultado de este modelo de vecinos más cercanos, el modelo ha elegido que va a mirar
#los 7 más cercanos para definir la espécie.
knn_model <- train(Species ~ .,
                   data = train_set,
                   method = "knn",
                   trControl = ctrl,
                   tuneGrid = expand.grid(k = seq(2, 15, 1)))

knn_model

knn_predict <- predict(knn_model, test_set)

confusionMatrix(knn_predict, test_set$Species)
