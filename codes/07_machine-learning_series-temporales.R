# Packages ----------------------------------------------------------------

library(tidyverse)
library(caret)
library(mlbench)


# Apuntes -----------------------------------------------------------------

# CROSSVALIDATION
# T?cnica de elecci?n y evaluaci?n de modelos cuya finalidad es la simulaci?n de ir obteniendo datos externos (nuevos) con los cuales ir evaluando el poder predictivo del modelo. Recordar que ML se enfoca en el poder predictivo por lo que ir incorporando estos nuevos sets de datos permite ir evaluando el modelo en este aspecto.

# A pesar de lo robusta que es la t?cnica del CV, no es apropiada para todos los modelos. Si el modelo est? basado en series de tiempo este m?todo no es apropiado.


# Modelaci?n --------------------------------------------------------------

data("BreastCancer")

dplyr::glimpse(BreastCancer) # en reemplazo de str()

# El paquete caret presenta una funci?n que permite explorar los datos designando una variable y.

caret::featurePlot(
  # No se toma la variable ID
  x = BreastCancer[, 2:5],
  y = BreastCancer$Class,
  plot = "pairs",
  auto.key = list(columns = 2)
)


# Error: Al modelar aparece un error producto de que existen NA en la base de datos (16 en la columna). Para solucionar este problema se podr?an eliminar dichas observaciones o trabajar con algoritmos distintos que puedan modelar con NAs presentes en los datos.

# Soluci?n: Eliminar los NAs ssi son pocos en relaci?n al total

BC2 <- BreastCancer[complete.cases(BreastCancer), ]

# CREACI?N BASES DE DATOS

set.seed(1)

Index <- createDataPartition(
  y = BC2$Class,
  list = F,
  p = 0.8
)

Train <- BC2[Index, -1]
Test <- BC2[-Index, -1]

# N-repeated K-fold Crossvalidation

fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10
)

# MODELACI?N

gbmFit1 <- train(
  form = Class ~ .,
  data = Train,
  method = "gbm",
  trControl = fitControl,
  verbose = F
)

# MATRIZ DE CONFUSI?N
# C?mo estamos frente a un modelo de clasificaci?n empleamos una matriz de confusi?n que nos permita analizar las m?tricas de precisi?n. Esta matriz se emplea con la base de datos de testeo externa.

caret::confusionMatrix(
  data = predict(
    object = gbmFit1,
    newdata = Test
  ),
  reference = Test$Class
)

# Prevalencia: % de los datos son benignos
# Detection Rate: % de los datos detectados como benignos

# Vamos a ir modificando y precisando los par?metros del trainControl

# LOOCV: CV del tipo Leave One Out (LOO)

# TIPO DE PREDICCIONES

stats::predict(
  object = gbmFit1,
  newdata = Test,
  # Default (prediccion seg?n el modelo)
  type = "raw"
)

stats::predict(
  object = gbmFit1,
  newdata = Test,
  # Probabilidad de ser alguno de las clases de la variable respuesta
  type = "prob"
)

# En el primer tipo de prediccion ("raw") obtenemos como respuesta la variable "maligno" o "benigno", escogidas en base a una probabilidad. Si deseamos la informaci?n acerca de la probabilidad resultante podemos solicitarla con "prob" dnado como resultado la probabilidad de ocurrencia para cada uno de las clases que componene la variable respuesta. Por default se debe superar el 50% para que una clase sea escogida por sobre la otra pero este es un par?metro que podemos modificar.
# Clasificar a una observaci?n (tumor) como "sospechoso de maligno" con un 20% en vez de con un 50%.


# Series de tiempo --------------------------------------------------------

# Dentro del universo de ML las series de tiempo son la rama que se destaca por ser la m?s diferente en cuanto a c?mo trabajar con ella.

# DATOS

Contaminacion <- read_csv("https://raw.githubusercontent.com/derek-corcoran-barrios/derek-corcoran-barrios.github.io/master/CursoMultiPres/Capitulo_7/Contaminacion.csv");Contaminacion

summary(Contaminacion)

glimpse(Contaminacion)

# No hay valores NAs que pueden perjudicar la modelaci?n

# La aplicaci?n de un CV no es apropiada para la modelaci?n de series de tiempo por las siguientes razones:
#* (1) Sensibilidad a lo que ocurre el d?a anterior. Los modelos de series de tiempo deben modelar las variables teniendo en cuenta el efecto de estas no s?lo hoy en d?a sino el efecto que pudieron haber provocado el d?a de ayer, esta complejidad se ecent?a teniendo en consideraci?n que dicho efecto var?a enormemente por aspectos de temporalidad (no es lo mismo el efecto de precipitaci?n en invierno que en verano, etc).
#* (2) Pronosticar el futuro. El objetivo de las series de tiempo es modelar lo que ha ocurrido hasta ahora y poder obtener conclusiones acerca de lo que se viene (futuro).

# ?C?mo mejorar el modelo?
# Podr?amos ir agregando variables que estimemos conveniente en el modelo. Por ejemplo, observando los gr?ico parece haber un comportamiento estacional que ser?a apropiado de incluir en la forma de variables diarias, semanales y mensuales.
# Tambien podr?amos haber incluido los valores de temperatura de 10 d?as atr?s para observar si es posible predecir esa variable 10 d?as en el futuro.

Contaminacion <- Contaminacion %>%
  mutate(
    Dia = lubridate::day(Fecha),
    Semana = lubridate::week(Fecha),
    Mes = lubridate::month(Fecha)
  )

# CREACI?N DE BASES DE DATOS
# El subseteo de datos de entrenamiento y testo presenta algunas diferencias. El modelo se entrena con la totalidad de los datos que sean previos a determinada fecha mientras que la validaci?n se realiza con la totalidad de datos posterior a la misma. 

# Base de datos de entrenamiento (WINDOW): 2003 a 2015
Train <- Contaminacion %>% dplyr::filter(!(lubridate::year(Fecha)) %in% c(2016, 2017))

# Base de datos de testeo (HORIZON): 2016 y 2017
Test <- Contaminacion %>% dplyr::filter(lubridate::year(Fecha) %in% c(2016, 2017))

# IMPORTANTE: Debemos fijarnos de que los datos esten ordenados de acuerdo a la variable fecha antes de realizar el entrenamiento de la serie de tiempo. Esta acci?n la podemos realizar mediante la funci?n arrange() del paquete dplyr (tidyverse).

# De esta manera, vamos a construir el modelo de series de tiempo con 12 a?os de entrenamiento y lo testearemos en 2.

# La terminolog?a empleada en las series de tiempo es distinta a las dem?s ramas de ML. La base de datos de entrenamiento se denomina ventana inicial ("InitialWindow") mientras que la de testeo se llama horizonte ("horizon").

# Funci?n para graficar y entender el efecto de la definici?n de la ventana y el horizonte en las series de tiempo modeladas

# TRAIN CONTROL EN SERIES TEMPORALES
# No se emplean la t?cnica de CV sino que se usan los time slices, los cuales poseen un efecto similar. Cada time slice es una subdivisi?n del Window (datos de entrenamiento) por cada unidad de d?as indicadas en el skip. En la medida que el skip disminuya se ir?n creando m?s time slices con los cuales evaluar el modelo por lo que resulta eneficioso tener la mayor cantidad de slices posible (similar al LOOCV).
# Podr?amos entender a los time slices como el crossvalidation de las series de tiempo. ?Por qu? no se emplean CV? por el planteamiento de las series de tiempo el objetivo de estas es entrenar una base de datos de a?os anteriores para predecir los valores en a?os del futuro. De ocupar CV no podr?amos hacer esta distinci?n temporal sino que se entrenar?a un modelo con una porci?n del total de datos para predecir otra porci?n, independiente del a?o.

Graph_Slice <- function(Slices = Slices){
  Slice <- list()
  for (i in seq_along(Slices$test)) {
    Window <- Train[Slices$train[[i]], ] %>% mutate(rep = as.character(i), class = "Window (Train)")
    Horizon <- Train[Slices$test[[i]], ] %>% mutate(rep = as.character(i), class = "Horizon (Test)")
    Slice[[i]] <- bind_rows(Window, Horizon)
  }
  Slices <- Slice %>% purrr::reduce(bind_rows)
  ggplot(data = Slices, aes(x = Fecha, y = MP25)) +
    geom_path(aes(color = class)) +
    facet_wrap(~ rep) +
    theme_bw()
}

# ?Qu? significan los 365? Son unidades, no hacen referencia exlusiva a los d?as sino al espaciamiento que hay entre las observaciones que hacen referencia a la misma fecha. Por ejemplo, en los datos se observa el registro para todos los d?as del a?o s/ agrupaciones extras por lo que hace sentido que el n?mero apropiado sea de 365. En otros casos donde s?lo hay una porci?n de los d?as del a?o en la base de datos esto no se cumple. Para designar las unidades apropiadas en estos casos debemos tener en cuenta la cantidad de d?as que existe entre las distintas observaciones para las mismas fechas. Esto tambien se vuelve sumamente relevante cuando tenemos observaciones para los mismos d?as repartidas en lo que ser?an distintas clases de una variable categ?rica donde la frecuencia de las fechas se ver? afectada.

Slices <- caret::createTimeSlices(
  y = Train$MP25,
  # Testear con un 1?o (365 d?as)
  horizon = 365,
  # Entrenar con 3 a?os
  initialWindow = 365 * 3,
  # Ventana incial fija o variable (incrementa)?
  fixedWindow = T,
  # Datos no solapados por al menos 1 a?o
  skip = 365
)

Graph_Slice(Slices)

# Los gr?ficos obtenidos son construidos a trav?s del CV donde el modelo es construido con 3 a?os y evaluado en 1. Que el skip sea de 1 a?o significa que posterior a la primera iteraci?n la siguiente iniciar? con los datos del pr?ximo a?o.

# fixedWindow = F: el n?mero de a?os que se incluyen en la ventana va incrementando (variando)

# FIT CONTROL PARA SERIES TEMPORALES
# Es similar a los argumentos empleados enteriormente para crear los time slices y definir las ventanas junto con los horizontes del modelo.

fitControl <- caret::trainControl(
  method = "timeslice",
  horizon = 365,
  initialWindow = 365 * 3,
  fixedWindow = T,
  skip = 30
)

gbmFitTime <- caret::train(
  form = MP25 ~ .,
  # Con los datos de entrenamiento
  data = Train,
  method = "gbm",
  # Se aplica un m?todo an?logo (timeslice) al cv
  trControl = fitControl,
  verbose = T
)

# RESULTADO

caret::postResample(
  pred = predict(
    object = gbmFitTime, 
    newdata = Test
  ),
  obs = Test$MP25
)

