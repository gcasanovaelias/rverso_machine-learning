# Packages ----------------------------------------------------------------

library(tidyverse)
library(MuMIn)
library(broom)
library(broom.mixed)
library(caret)
library(raster)
library(doParallel)
library(parallel)


# Apuntes -----------------------------------------------------------------

# Machine learning (ML) se refiere a una técnica basada en algoritmos de inteligencia artificial (IA) que son capaces de ser entrenados para la identificación de ciertos patrones que pasarían de ser percibidos por el investigador. La ventaja del ML es que el algoritmo es capaz de ser entrenado con un cuantioso volumen de datos para la elaboración del modelo (de regresión o clasificación). Debido a lo anterior, es dificil aplicar algoritmos de ML cuando se tienen menos de 1000 observaciones de un fenómeno (no son suficientes para entrenar el algoritmo de IA).

# En esta misma línea, si el volumen de datos es inmenso (+ 100000) es mejor realizar un deeplearning (caso particular de ML).

# Como se había mencionado anteriormente, los algoritmos de ML son más adecuados cuando el enfoque del estudio es predictivo y no explicativo (en general su desempeño es bastante malo al momento de realizar un estudio explicativo).

# Cuando trabajamos con ML debemos tener en consideración la técnica de crossvalidation (n-repeated k-fold crossvalidation) la cual nos permite crear modelos y métricas de ajuste mucho más robustas en base al promedio de k subseteos y modelaciones repetidas n veces en base a una base de datos.

# ¿Cuales son los mejores algoritmos? ¿Cual emplear? Depende del volumen de datos. En general, los algoritmos de Random Forest, GBM y SVM son los que presentan mejor rendimiento en lo que sería el Machine Learning tradicional con un volumen de datos de alrededor de 1000 a 10000. Conforme esta cantidad va aumentando es que se debe considerar aplicar otros algoritmos relacionados con el Deep Learning tales como las redes neuronales (Neural Networks).


# ML Classification -------------------------------------------------------


data("iris")

attach(iris)

set.seed(seed = 2021)

# BASES DE DATOS

Index <- caret::createDataPartition(
  # Vector de resultados
  y = Species,
  list = F,
  # % de los datos destinados a entrenamiento
  p = 0.5
)

# ¿Por qué es importante poner la variable respuesta en la función anterior? Cuando se crean bases de datos de entrenamiento y testeo para una variable categórica se desea que los distintos niveles de dicha variable tengan igual representación, es decir, que se encuentren en igual cantidad o por lo menos en una misma razón para ambas bases de datos. La función anterior de caret permite obtener este resultado de manera automática.

# Esta base de datos presenta muy pocas observaciones en lo que respecta a ML. Generalmente lo que queremos realizar es que la base de datos de entrenamiento sea lo más grande posible al mismo tiempo de que los datos de testeo sean suficientes.

Train <- iris[Index, ]
Test <- iris[-Index, ]

# N-REPEATED K-FOLD CROSSVALIDATION

fitControl <- caret::trainControl(
  method = "repeatedcv", 
  # k-fold
  number = 10, 
  # n-repeated
  repeats = 10)

# ¿Qué algortimo ocupar?
# http://topepo.github.io/caret/train-models-by-tag.html

names(caret::getModelInfo())

# (1) RPART
# rpart es un algoritmo de ML bastante sencillo que esta basado en árboles de decisión y es empleado para realizar clasificaciones y regresiones. Es uno de los algoritmos de ML más antiguos y prácticamente se emplea unicamente de manera didactica y educativa.

Class <- caret::train(
  Species ~ .,
  data = Train,
  method = "rpart", 
  trControl = fitControl
)

# En este caso, el crossvalidation es empleado para la base de datos de entrenamiento, es decir, la subdivisión de k-fold repetidas n-veces ocurre dentro de este set de datos subseteados y no en el total.

# En general, en los modelos de ML no se realiza una selección de variables sino que se le deja esa tarea al mismo algoritmo.

Class$resample
Class$finalModel

# EVALUACIÓN BASE DE DATOS INTERNA
caret::postResample(
  pred = predict(object = Class, newdata = Train),
  obs = Train$Species
)

# Una vez que ya tenemos el modelo podemos evaluarlo con una base de datos externa, es decir, datos nuevos con los que el modelo nunca ha sido entrenado en ninguna de los modelos evaluados en el crossvalidation.

# EVALUACIÓN BASE DE DATOS EXTERNA 

caret::postResample(
  pred = predict(object = Class, newdata = Test),
  obs = Test$Species
)

# En un modelo de clasificación las métricas que permiten evaluar el desempeño del modelo no son las mismas que para los modelos de regresión. En este caso, las métricas están referidas a la capacidad del modelo de clasificación para determinar correctamente la clase de una observación empleando como por ejemplo el Accuracy, Kappa, Sensitivity o el Specificity. Estas métricas son similares el R2 empleadas en los modelos de regresión en el sentido de que presentan valores entre 0 y 1 donde entre mayor sea esto significa que el modelo es mejor.

# Generalmente se reportan las métricas in (entrenamiento) y out (testeo) of dataset pero tambien es usual que se reporte esta última unicamente. "Cuando se ocupan bases de datos externas a la original así es como se comporta el modelo".

# ¿Qué es el accuracy? Es una medida que cuantifica el % de predicciones correctas con respecto al total (Accuracy = Correctos / Total). De esta manera, un accuracy del 95% se refiere a que el 95% de las predicciones realizadas por el modelo fueron acertadas con respecto al total.

# MATRIZ DE CONFUSIÓN

caret::confusionMatrix(
  data = predict(object = Class, newdata = Test),
  reference = Test$Species
)

# ¿Por qué con la base de datos de prueba? ML es aplicado principalmente cuando queremos realizar predicciones por lo que generalmente se reportarán las estadísticas y métricas obtenidas con datos externos (nuevos) a los que se emplearon para construir el modelo.

# IMPORTANCIA DE LAS VARIABLES (varImp)
# El algoritmo va a determinar cual es la variable que separa de mejor manera las clases.

Class %>% caret::varImp() %>% plot()

# COMPLEXITY PARAMETER
# El complexity parameter indica cuantas divisiones presenta el arbol de decisión creado por el algoritmo teniendo un efecto importante en el accuracy obtenido en el modelo. Importante: el valor en el eje x se entiende al revés (0: complejidad máxima, 1: complejidad mínima).

plot(Class)

# Los puntos empleados para construir el gráfico corresponden a CP igual a 0, 0.44 y 0.5 (default) con sus respectivos valores de Accuracy. Si se desea modificar estos valores se puede indicar los nuevos valores de CP en un argumento de train().

rpartGrid <- expand.grid(cp = c(0.48, 0.8, 1))

Class2 <- caret::train(
  Species ~ .,
  data = Train,
  method = "rpart", 
  trControl = fitControl,
  tuneGrid = rpartGrid
)

plot(Class2)

# Cada algoritmo presenta parámetros distintos
# https://topepo.github.io/caret/available-models.html


# ML Regression -----------------------------------------------------------

# Vamos a realizar modelos de distribución de especies (para el guanaco) a partir de 4 algoritmos de regresión ML distintos.

# Descarga de archivos a partir de github

names <- c("SA", "sp2", "sp")

urls <- {c(
  paste0("https://raw.githubusercontent.com/derek-corcoran-barrios/derek-corcoran-barrios.github.io/master/CursoMultiPres/Capitulo_6/",
         names, 
         ".rds"))}

# Loop para descarga y creación de objetos (names)
{
  purrr::map2(
    .x = urls,
    # Nombre del archivo
    .y = paste0(names, ".rds"),
    # Descarga
    .f = ~ utils::download.file(
      url = .x,
      destfile = .y,
      method = "curl",
      overwrite = T
    )
  ) %>%
    purrr::map2(.y = names,
                # Importar datos
                .f = ~ readr::read_rds(paste0(.y, ".rds"))) %>%
    purrr::map2(.y = names,
                # Asignación de valores a objetos
                .f = ~ base::assign(x = .y,
                                    value = .x,
                                    envir = .GlobalEnv))
}

base::infoRDS("SA.rds") # Lectura del archivo RDS

# REGRESIÓN CON DATOS ESPACIALES

# Los modelos de distribución de especies tienen por objetivo determinar la presencia potencial de una especie en particular. Para esto aplican variables explicativas del tipo ambiental (clima, topografia, vegetación, etc) para entrenar un modelo que permita determinar la presencia o ausencia de dicha especie. De esta manera, la variable respuesta de esta presencia o ausencia es del tipo 1 o 0 (respectivamente) por lo que debiesemos pensar en emplear un modelo lineal generalizado (glm) con una familia de distribución binomial. En este caso aplicaremos nuevamente el algoritmo rpart para determinar la distribución potencial del guanaco.

# CREACIÓN DE LAS BASES DE DATOS

# El n-repeated k-fold crossvalidation puede tener una subvariante denominado como "Holdout method". Previo a la implementación de la técnica podemos subsetear la base de datos totales en lo que serían bases de datos interna (entrenamiento) y externa (testeo). Esta división provoca que el modelo que se somete al CV sea en base únicamente a la base de datos de entrenamiento dejando intacta una porción de los datos. Lo anterior provoca que podamos evaluar el poder predictivo del modelo final en base a una base de datos totalmente nueva (externa) que el modelo nunca ha visto. Como se ha mencionado anteriormente, el ML está enfocado en la capacidad predictiva (y no explicativa) por lo que esta variación del CV permitiría evaluar de mejor manera esta capacidad.

set.seed(2)

Index1 <- caret::createDataPartition(
  y = sp2$presence,
  p = 0.8,
  list = F
)

Train1 <- sp2[Index1, ]
Test1 <- sp2[-Index1, ]


# N-REPEATED K-FOLD CROSSVALIDATION

fitControl <- caret::trainControl(
  method = "repeatedcv", 
  number = 10, 
  repeats = 10)

Model1 <- caret::train(
  presence ~ .,
  # Aplicado únicamente al set de entrenamiento (distinto de la clase anterior)
  data = Train1,
  method = "rpart",
  trControl = fitControl
)

# COMPLEXITY PARAMETER

plot(Model1)

# ¿RMSE? El Root Mean Square Error, al igual que el R2, es una métrica de ajuste del modelo (GOF) donde un menor RMSE implica un mejor ajuste de los datos predichos con respecto a los observados.

# De esta manera, el paramétro de complejidad está indicando que el modelo con mayor complejidad (el más cercano a 0) va a ser el que presente un menor RMSE, es decir, el modelo más complejo presenta el mejor ajuste. Esto es válido para los análisis de regresión realizados con ML que se encuentran enfocados a la predicción de datos.

# EVALUACIÓN CON BASE DE DATOS INTERNA
# Producto de que ahora nos encontramos en un modelo de regresión las métrica de ajuste entregadas por la función postResample() son RMSE, R2 y MAE.

caret::postResample(
  pred = stats::predict(
    # object es el modelo
    object = Model,
    newdata = Train1
  ),
  obs = Train1$presence
)

# EVALUACIÓN CON BASE DE DATOS EXTERNA

caret::postResample(
  pred = stats::predict(
    object = Model,
    newdata = Test1
  ),
  obs = Test1$presence
)

# IMPORTANCIA DE VARIABLES

varImp(Model)
Model %>% varImp() %>% plot()

# La temperatura del mes más frío es la variable más importante para determinar la presencia o ausencia del guanaco.

# A diferencia de los otros modelos que hemos visto en el curso hasta ahora, los algoritmos de ML no toman en cuenta la MULTICOLINEARIDAD que pueda existir en las variables explicativas al momento del entrenamiento.

# MAPAS
# El modelo obtenido puede ser aplicado a mapas ya que estos cuentan con las variables explicativas evaluadas. Para esto se emplea la función predict() del paquete raster la cual presenta argumentos distintos a los que se encuentran en stats::predict().

Map <- raster::predict(
  # object hace referencia al Raster con las variables
  object = SA, 
  model = Model
  )

# Mapa resultado

plot(Map)

# OTROS ALGORITMOS ML PARA REGRESION
# gmb y rf son algoritmos de ML basados en la integración (promedio) de numerosos árboles de decisión por lo que consumen más tiempo y memoria para el entrenamiento de los modelos. Estos dos modelos mencionados anteriormente son considerados como los "mejores". El boosting aplicado en gbm consiste en la aplicación de muchos modelos ineficientes para desarrollar 1 bueno.


# Stochastic Gradient Boosting (boosted regression trees)
Model2 <- caret::train(
  presence ~ .,
  data = Train1,
  method = "gbm",
  trControl = fitControl
)

# Este algoritmo gbm no puede ser representado gráficamente como un único arbol de decisión ya que de por sí está compuesto por muchos de estos árboles de decisión.

# Influencia de las variables

summary(Model2)

# A diferencia del algoritmo de rpart, que sólo presentaba el parámetro de complejidad (CP), gbm muestra 2 parámetros: (1) Boosting iterations ó n.trees (n° de modelos o iteraciones) y (2) Max Tree Depth ó interaction.depth (n° de divisiones).

plot(Model2)

# Del gráfico se puede concluir que con 150 iteraciones de boosting y una profundidad máxima de a´rbol igual a 3 se minimiza el RMSE del modelo, pero estos no son los único parámetros que influyen en la modelación...

# ...Expandiendo el conocimiento de los parámetros con otros GRID:
# Por default sólo observamos 2 de los 4 parámetros que se pueden modificar en el gbm modelado (https://topepo.github.io/caret/available-models.html). Podemos explorar de mejor manera cuales son los valores de estos parámetros que maximizan o minimizan la métrica deseada a través de la función expand.grid() la cual permite obtener todas las combinaciones posibles de parámetros.

gbmGrid <- base::expand.grid(
  # Porfundidad de interacción
  interaction.depth = c(1,5,9),
  # n° arboles = n° iteraciones (cada arbol = 1 iteracion)
  n.trees = (1:30)*50,
  # rapidez de aprendizaje del modelo
  shrinkage = c(0.1, 0.01),
  # n° mínimo de obs por nudo
  n.minobsinnode = c(10,30)
)

# Al indicar estas nuevas combinaciones de parámetros le estamos entregando al modelo otra fuente de variación en el entrenamiento de los datos. Cada una de las iteraciones realizadas en el CV debe realizarse para cada combinación indicada de los parametros. Por default al realizar gbm caret provoca una variación de 9 combinación de parámetros para cada CV. Si aplicamos la grilla expandida anterior el factor multiplicativo es 360 y no 9.

# Random Forest (default)
Model3 <- caret::train(
  presence ~ .,
  data = Train1,
  method = "rf",
  trControl = fitControl
)

plot(Model3)

# ¿Existe alguna función que te permita modelar todos estos algoritmos y que entregue una selección de los mejores? NO. No sería recomendable por lo lento que sería entrenar tal cantidad de algoritmos de ML (los cuales varían mucho en su complejidad).

# MÉTRICAS DE MODELOS ML
{
  metrics <- map(
    .x = list(Model1, Model2, Model3),
    .f = ~ caret::postResample(
      pred = stats::predict(object = .x,
                            newdata = Test1),
      obs = Test1$presence
    )
  ) %>%
    map(.f = ~ as_tibble(.x)) %>%
    reduce(bind_cols) %>%
    t() %>%
    as_tibble() %>%
    rename(RMSE = V1,
           Rsquared = V2,
           MAE = V3) %>%
    mutate(Model = c("Rpart", "GBM", "RF")) %>%
    arrange(MAE)
}
  
# COMPARACIÓN DE ALGORITMOS ML
# Método de colección, análisis y visualización de un set de datos resampleados

# A partir de los resamples se obtienen distintas muestras de cada uno de los n-repeated k-fold crossvalidations realizados (indicado en los argumentos de train)

Comp <- caret::resamples(
  x = list(
    Rpart = Model1,
    GBM = Model2,
    RF = Model3
  )
)

Comp$values

# ¿Cuales son las diferencias entre estos algoritmos?

Difs <- base::diff(Comp)

Difs$difs

# Density plot

lattice::densityplot(
  x = Difs,
  metric = "Rsquared",
  auto.key = T,
  pch = "|"
)

# Este gráfico cuantifica las diferencias (restas) que existen entre los distintos algoritmos. De esto podemos obtener las siguientes conclusiones:
#* (1) RF presenta valores de R2 ligeramente más altos que GBM y es por esto que hay una alta cantidad de datos que se encuentran ligeramente negativos.
#* (2) Rpart, con cualquier de los otros dos algoritmos, presenta valores mucho más bajos de R2 (y es por eso que las gráficas se encuentran más centradas en valores más negativos)

# Boxplot horizontal

lattice::bwplot(x = Difs, metric = "Rsquared")

# Mapas de los algoritmos GBM y RF
{
  # Predicción raster
  map(.x = list(Model2, Model3),
      .f = ~ raster::predict(object = SA,
                             model = .x)) %>%
    # Asignación a symbols
    map2(
      .y = list("Map2", "Map3"),
      .f = ~ assign(x = .y,
                    value = .x,
                    envir = .GlobalEnv)
    ) %>%
    # Plot
    map2(.y = c("GBM", "RF"),
         .f = ~ plot(.x,
                     main = .y))
}

# Podemos observar que mientras que el algoritmo de rpart entregaba como resultado 3 posibles valores de ocupancia (0.012, 0.043 y 0.9) los algoritmos de gbm y rf presentan un análisis de regresión mucho más detallado.


# Regresión GLM binomial --------------------------------------------------


attach(sp2)

# Entrenamiento de modelo base

FitGlob <- glm(
  formula = presence ~ ., 
  family = binomial,
  data = sp2
)

# Prohibir ciertas combinaciones

smat <- sp2[ ,-1] %>% cor() %>% abs() >= 0.7
smat[!lower.tri(smat)] <- NA

K <- nrow(sp2)/10 %>% floor()

# Selección de modelos

options(na.action = "na.fail")

Selected <- dredge(
  global.model = FitGlob,
  subset = smat,
  m.lim = c(0, K)
)

# Mejor modelo

best <- MuMIn::get.models(
  object = Selected,
  subset = delta<=2
)[[1]]

tidy(best)
glance(best)
augment(best)

# Mapa

MapGLM <- raster::predict(
  object = SA,
  model = best,
  # IMPORTANTE: el tipo de predicción obtenida
  type = "response"
)

# El argumento type = "response" indica que la predicción sea con el valor de y transformado según la ecuación enlace (link). Esto es sumamente relevante para los glm y los nls donde la predicción se hace en base a una variable y transformada a algún comportamiento (log, exponencial, potencial, inverso, logit, etc). El argumento por default calcula las predicciones en base a la escala de los predictores lineales (y transformada).

plot(MapGLM)


# Paralelización ----------------------------------------------------------
# (sección de la 10ma clase del curso)

# La gran demanda de computo y tiempo de realizar modelos complejos y en grandes cantidades hacen necesario el recurrir a la paralelización. La mayoría de los computadores que se encuentran disponibles en el mercado presentan más de 1 núcleo (core). En este contexto de programación esto significa que los computadores con múltiples cores pueden realizar los computos y tareas de manera paralela y no secuencial para despues unir los resultados obtenidos por cada uno de los procesamientos.

# ¿Cuantos cores tengo en el computador?

parallel::detectCores()

# Creación de una serie de copias de R corriendo en paralelo y comunicandose entre ellas

parallel::makePSOCKcluster(8) %>% 
  # Registro del trabajo en paralelo
  doParallel::registerDoParallel()
  
# De esta manera, cualquier código que corramos proveniente de caret será paralelizado

Model2_exp <- caret::train(
  presence ~ .,
  data = Train1,
  method = "gbm",
  trControl = fitControl,
  # 360 combinaciones de los valores de parámetros en vez de 9 (default)
  tuneGrid = gbmGrid
)

plot(Model2_exp)

# Podemos observar un gráfico mucho más completo acerca del comportamiento del modelo en función de los distintos parámetros que componen un gbm. De esta manera, la minimización del RMSE ocurre cuando el n° de iteraciones (arboles) es igual a 1500, con una profundidad de modelaje de 9, una tasa de aprendizaje (shrinkage) de 0.01 y un n° mínimo de observaciones por nudo igual a 10.

# Una vez finalizada la tarea es necesario detener el trabajo en paralelo ya que tendremos múltiples ventanas de R realizando tareas lo cual consume una importante porción de la RAM.

# makeCluster() tambien funciona
parallel::makePSOCKcluster(8) %>% 
  # Detener el trabajo en paralelo
  parallel::stopCluster()
