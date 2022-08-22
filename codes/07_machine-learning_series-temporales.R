# Packages ----------------------------------------------------------------

library(tidyverse)
library(caret)
library(mlbench)


# Apuntes -----------------------------------------------------------------

# CROSSVALIDATION
# Técnica de elección y evaluación de modelos cuya finalidad es la simulación de ir obteniendo datos externos (nuevos) con los cuales ir evaluando el poder predictivo del modelo. Recordar que ML se enfoca en el poder predictivo por lo que ir incorporando estos nuevos sets de datos permite ir evaluando el modelo en este aspecto.

# A pesar de lo robusta que es la técnica del CV, no es apropiada para todos los modelos. Si el modelo está basado en series de tiempo este método no es apropiado.


# Modelación --------------------------------------------------------------

data("BreastCancer")

dplyr::glimpse(BreastCancer) # en reemplazo de str()

# El paquete caret presenta una función que permite explorar los datos designando una variable y.

caret::featurePlot(
  # No se toma la variable ID
  x = BreastCancer[, 2:5],
  y = BreastCancer$Class,
  plot = "pairs",
  auto.key = list(columns = 2)
)


# Error: Al modelar aparece un error producto de que existen NA en la base de datos (16 en la columna). Para solucionar este problema se podrían eliminar dichas observaciones o trabajar con algoritmos distintos que puedan modelar con NAs presentes en los datos.

# Solución: Eliminar los NAs ssi son pocos en relación al total

BC2 <- BreastCancer[complete.cases(BreastCancer), ]

# CREACIÓN BASES DE DATOS

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

# MODELACIÓN

gbmFit1 <- train(
  form = Class ~ .,
  data = Train,
  method = "gbm",
  trControl = fitControl,
  verbose = F
)

# MATRIZ DE CONFUSIÓN
# Cómo estamos frente a un modelo de clasificación empleamos una matriz de confusión que nos permita analizar las métricas de precisión. Esta matriz se emplea con la base de datos de testeo externa.

caret::confusionMatrix(
  data = predict(
    object = gbmFit1,
    newdata = Test
  ),
  reference = Test$Class
)

# Prevalencia: % de los datos son benignos
# Detection Rate: % de los datos detectados como benignos

# Vamos a ir modificando y precisando los parámetros del trainControl

# LOOCV: CV del tipo Leave One Out (LOO)

# TIPO DE PREDICCIONES

stats::predict(
  object = gbmFit1,
  newdata = Test,
  # Default (prediccion según el modelo)
  type = "raw"
)

stats::predict(
  object = gbmFit1,
  newdata = Test,
  # Probabilidad de ser alguno de las clases de la variable respuesta
  type = "prob"
)

# En el primer tipo de prediccion ("raw") obtenemos como respuesta la variable "maligno" o "benigno", escogidas en base a una probabilidad. Si deseamos la información acerca de la probabilidad resultante podemos solicitarla con "prob" dnado como resultado la probabilidad de ocurrencia para cada uno de las clases que componene la variable respuesta. Por default se debe superar el 50% para que una clase sea escogida por sobre la otra pero este es un parámetro que podemos modificar.
# Clasificar a una observación (tumor) como "sospechoso de maligno" con un 20% en vez de con un 50%.


# Series de tiempo --------------------------------------------------------

# Dentro del universo de ML las series de tiempo son la rama que se destaca por ser la más diferente en cuanto a cómo trabajar con ella.

# DATOS

Contaminacion <- read_csv("https://raw.githubusercontent.com/derek-corcoran-barrios/derek-corcoran-barrios.github.io/master/CursoMultiPres/Capitulo_7/Contaminacion.csv");Contaminacion

summary(Contaminacion)

glimpse(Contaminacion)

# No hay valores NAs que pueden perjudicar la modelación

# La aplicación de un CV no es apropiada para la modelación de series de tiempo por las siguientes razones:
#* (1) Sensibilidad a lo que ocurre el día anterior. Los modelos de series de tiempo deben modelar las variables teniendo en cuenta el efecto de estas no sólo hoy en día sino el efecto que pudieron haber provocado el día de ayer, esta complejidad se ecentúa teniendo en consideración que dicho efecto varía enormemente por aspectos de temporalidad (no es lo mismo el efecto de precipitación en invierno que en verano, etc).
#* (2) Pronosticar el futuro. El objetivo de las series de tiempo es modelar lo que ha ocurrido hasta ahora y poder obtener conclusiones acerca de lo que se viene (futuro).

# ¿Cómo mejorar el modelo?
# Podríamos ir agregando variables que estimemos conveniente en el modelo. Por ejemplo, observando los gráico parece haber un comportamiento estacional que sería apropiado de incluir en la forma de variables diarias, semanales y mensuales.
# Tambien podríamos haber incluido los valores de temperatura de 10 días atrás para observar si es posible predecir esa variable 10 días en el futuro.

Contaminacion <- Contaminacion %>%
  mutate(
    Dia = lubridate::day(Fecha),
    Semana = lubridate::week(Fecha),
    Mes = lubridate::month(Fecha)
  )

# CREACIÓN DE BASES DE DATOS
# El subseteo de datos de entrenamiento y testo presenta algunas diferencias. El modelo se entrena con la totalidad de los datos que sean previos a determinada fecha mientras que la validación se realiza con la totalidad de datos posterior a la misma. 

# Base de datos de entrenamiento (WINDOW): 2003 a 2015
Train <- Contaminacion %>% dplyr::filter(!(lubridate::year(Fecha)) %in% c(2016, 2017))

# Base de datos de testeo (HORIZON): 2016 y 2017
Test <- Contaminacion %>% dplyr::filter(lubridate::year(Fecha) %in% c(2016, 2017))

# IMPORTANTE: Debemos fijarnos de que los datos esten ordenados de acuerdo a la variable fecha antes de realizar el entrenamiento de la serie de tiempo. Esta acción la podemos realizar mediante la función arrange() del paquete dplyr (tidyverse).

# De esta manera, vamos a construir el modelo de series de tiempo con 12 años de entrenamiento y lo testearemos en 2.

# La terminología empleada en las series de tiempo es distinta a las demás ramas de ML. La base de datos de entrenamiento se denomina ventana inicial ("InitialWindow") mientras que la de testeo se llama horizonte ("horizon").

# Función para graficar y entender el efecto de la definición de la ventana y el horizonte en las series de tiempo modeladas

# TRAIN CONTROL EN SERIES TEMPORALES
# No se emplean la técnica de CV sino que se usan los time slices, los cuales poseen un efecto similar. Cada time slice es una subdivisión del Window (datos de entrenamiento) por cada unidad de días indicadas en el skip. En la medida que el skip disminuya se irán creando más time slices con los cuales evaluar el modelo por lo que resulta eneficioso tener la mayor cantidad de slices posible (similar al LOOCV).
# Podríamos entender a los time slices como el crossvalidation de las series de tiempo. ¿Por qué no se emplean CV? por el planteamiento de las series de tiempo el objetivo de estas es entrenar una base de datos de años anteriores para predecir los valores en años del futuro. De ocupar CV no podríamos hacer esta distinción temporal sino que se entrenaría un modelo con una porción del total de datos para predecir otra porción, independiente del año.

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

# ¿Qué significan los 365? Son unidades, no hacen referencia exlusiva a los días sino al espaciamiento que hay entre las observaciones que hacen referencia a la misma fecha. Por ejemplo, en los datos se observa el registro para todos los días del año s/ agrupaciones extras por lo que hace sentido que el número apropiado sea de 365. En otros casos donde sólo hay una porción de los días del año en la base de datos esto no se cumple. Para designar las unidades apropiadas en estos casos debemos tener en cuenta la cantidad de días que existe entre las distintas observaciones para las mismas fechas. Esto tambien se vuelve sumamente relevante cuando tenemos observaciones para los mismos días repartidas en lo que serían distintas clases de una variable categórica donde la frecuencia de las fechas se verá afectada.

Slices <- caret::createTimeSlices(
  y = Train$MP25,
  # Testear con un 1ño (365 días)
  horizon = 365,
  # Entrenar con 3 años
  initialWindow = 365 * 3,
  # Ventana incial fija o variable (incrementa)?
  fixedWindow = T,
  # Datos no solapados por al menos 1 año
  skip = 365
)

Graph_Slice(Slices)

# Los gráficos obtenidos son construidos a través del CV donde el modelo es construido con 3 años y evaluado en 1. Que el skip sea de 1 año significa que posterior a la primera iteración la siguiente iniciará con los datos del próximo año.

# fixedWindow = F: el número de años que se incluyen en la ventana va incrementando (variando)

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
  # Se aplica un método análogo (timeslice) al cv
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

