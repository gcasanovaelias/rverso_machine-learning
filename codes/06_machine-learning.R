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

# Machine learning (ML) se refiere a una t?cnica basada en algoritmos de inteligencia artificial (IA) que son capaces de ser entrenados para la identificaci?n de ciertos patrones que pasar?an de ser percibidos por el investigador. La ventaja del ML es que el algoritmo es capaz de ser entrenado con un cuantioso volumen de datos para la elaboraci?n del modelo (de regresi?n o clasificaci?n). Debido a lo anterior, es dificil aplicar algoritmos de ML cuando se tienen menos de 1000 observaciones de un fen?meno (no son suficientes para entrenar el algoritmo de IA).

# En esta misma l?nea, si el volumen de datos es inmenso (+ 100000) es mejor realizar un deeplearning (caso particular de ML).

# Como se hab?a mencionado anteriormente, los algoritmos de ML son m?s adecuados cuando el enfoque del estudio es predictivo y no explicativo (en general su desempe?o es bastante malo al momento de realizar un estudio explicativo).

# Cuando trabajamos con ML debemos tener en consideraci?n la t?cnica de crossvalidation (n-repeated k-fold crossvalidation) la cual nos permite crear modelos y m?tricas de ajuste mucho m?s robustas en base al promedio de k subseteos y modelaciones repetidas n veces en base a una base de datos.

# ?Cuales son los mejores algoritmos? ?Cual emplear? Depende del volumen de datos. En general, los algoritmos de Random Forest, GBM y SVM son los que presentan mejor rendimiento en lo que ser?a el Machine Learning tradicional con un volumen de datos de alrededor de 1000 a 10000. Conforme esta cantidad va aumentando es que se debe considerar aplicar otros algoritmos relacionados con el Deep Learning tales como las redes neuronales (Neural Networks).


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

# ?Por qu? es importante poner la variable respuesta en la funci?n anterior? Cuando se crean bases de datos de entrenamiento y testeo para una variable categ?rica se desea que los distintos niveles de dicha variable tengan igual representaci?n, es decir, que se encuentren en igual cantidad o por lo menos en una misma raz?n para ambas bases de datos. La funci?n anterior de caret permite obtener este resultado de manera autom?tica.

# Esta base de datos presenta muy pocas observaciones en lo que respecta a ML. Generalmente lo que queremos realizar es que la base de datos de entrenamiento sea lo m?s grande posible al mismo tiempo de que los datos de testeo sean suficientes.

Train <- iris[Index, ]
Test <- iris[-Index, ]

# N-REPEATED K-FOLD CROSSVALIDATION

fitControl <- caret::trainControl(
  method = "repeatedcv", 
  # k-fold
  number = 10, 
  # n-repeated
  repeats = 10)

# ?Qu? algortimo ocupar?
# http://topepo.github.io/caret/train-models-by-tag.html

names(caret::getModelInfo())

# (1) RPART
# rpart es un algoritmo de ML bastante sencillo que esta basado en ?rboles de decisi?n y es empleado para realizar clasificaciones y regresiones. Es uno de los algoritmos de ML m?s antiguos y pr?cticamente se emplea unicamente de manera didactica y educativa.

Class <- caret::train(
  Species ~ .,
  data = Train,
  method = "rpart", 
  trControl = fitControl
)

# En este caso, el crossvalidation es empleado para la base de datos de entrenamiento, es decir, la subdivisi?n de k-fold repetidas n-veces ocurre dentro de este set de datos subseteados y no en el total.

# En general, en los modelos de ML no se realiza una selecci?n de variables sino que se le deja esa tarea al mismo algoritmo.

Class$resample
Class$finalModel

# EVALUACI?N BASE DE DATOS INTERNA
caret::postResample(
  pred = predict(object = Class, newdata = Train),
  obs = Train$Species
)

# Una vez que ya tenemos el modelo podemos evaluarlo con una base de datos externa, es decir, datos nuevos con los que el modelo nunca ha sido entrenado en ninguna de los modelos evaluados en el crossvalidation.

# EVALUACI?N BASE DE DATOS EXTERNA 

caret::postResample(
  pred = predict(object = Class, newdata = Test),
  obs = Test$Species
)

# En un modelo de clasificaci?n las m?tricas que permiten evaluar el desempe?o del modelo no son las mismas que para los modelos de regresi?n. En este caso, las m?tricas est?n referidas a la capacidad del modelo de clasificaci?n para determinar correctamente la clase de una observaci?n empleando como por ejemplo el Accuracy, Kappa, Sensitivity o el Specificity. Estas m?tricas son similares el R2 empleadas en los modelos de regresi?n en el sentido de que presentan valores entre 0 y 1 donde entre mayor sea esto significa que el modelo es mejor.

# Generalmente se reportan las m?tricas in (entrenamiento) y out (testeo) of dataset pero tambien es usual que se reporte esta ?ltima unicamente. "Cuando se ocupan bases de datos externas a la original as? es como se comporta el modelo".

# ?Qu? es el accuracy? Es una medida que cuantifica el % de predicciones correctas con respecto al total (Accuracy = Correctos / Total). De esta manera, un accuracy del 95% se refiere a que el 95% de las predicciones realizadas por el modelo fueron acertadas con respecto al total.

# MATRIZ DE CONFUSI?N

caret::confusionMatrix(
  data = predict(object = Class, newdata = Test),
  reference = Test$Species
)

# ?Por qu? con la base de datos de prueba? ML es aplicado principalmente cuando queremos realizar predicciones por lo que generalmente se reportar?n las estad?sticas y m?tricas obtenidas con datos externos (nuevos) a los que se emplearon para construir el modelo.

# IMPORTANCIA DE LAS VARIABLES (varImp)
# El algoritmo va a determinar cual es la variable que separa de mejor manera las clases.

Class %>% caret::varImp() %>% plot()

# COMPLEXITY PARAMETER
# El complexity parameter indica cuantas divisiones presenta el arbol de decisi?n creado por el algoritmo teniendo un efecto importante en el accuracy obtenido en el modelo. Importante: el valor en el eje x se entiende al rev?s (0: complejidad m?xima, 1: complejidad m?nima).

plot(Class)

# Los puntos empleados para construir el gr?fico corresponden a CP igual a 0, 0.44 y 0.5 (default) con sus respectivos valores de Accuracy. Si se desea modificar estos valores se puede indicar los nuevos valores de CP en un argumento de train().

rpartGrid <- expand.grid(cp = c(0.48, 0.8, 1))

Class2 <- caret::train(
  Species ~ .,
  data = Train,
  method = "rpart", 
  trControl = fitControl,
  tuneGrid = rpartGrid
)

plot(Class2)

# Cada algoritmo presenta par?metros distintos
# https://topepo.github.io/caret/available-models.html


# ML Regression -----------------------------------------------------------

# Vamos a realizar modelos de distribuci?n de especies (para el guanaco) a partir de 4 algoritmos de regresi?n ML distintos.

# Descarga de archivos a partir de github

names <- c("SA", "sp2", "sp")

urls <- {c(
  paste0("https://raw.githubusercontent.com/derek-corcoran-barrios/derek-corcoran-barrios.github.io/master/CursoMultiPres/Capitulo_6/",
         names, 
         ".rds"))}

# Loop para descarga y creaci?n de objetos (names)
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
                # Asignaci?n de valores a objetos
                .f = ~ base::assign(x = .y,
                                    value = .x,
                                    envir = .GlobalEnv))
}

base::infoRDS("SA.rds") # Lectura del archivo RDS

# REGRESI?N CON DATOS ESPACIALES

# Los modelos de distribuci?n de especies tienen por objetivo determinar la presencia potencial de una especie en particular. Para esto aplican variables explicativas del tipo ambiental (clima, topografia, vegetaci?n, etc) para entrenar un modelo que permita determinar la presencia o ausencia de dicha especie. De esta manera, la variable respuesta de esta presencia o ausencia es del tipo 1 o 0 (respectivamente) por lo que debiesemos pensar en emplear un modelo lineal generalizado (glm) con una familia de distribuci?n binomial. En este caso aplicaremos nuevamente el algoritmo rpart para determinar la distribuci?n potencial del guanaco.

# CREACI?N DE LAS BASES DE DATOS

# El n-repeated k-fold crossvalidation puede tener una subvariante denominado como "Holdout method". Previo a la implementaci?n de la t?cnica podemos subsetear la base de datos totales en lo que ser?an bases de datos interna (entrenamiento) y externa (testeo). Esta divisi?n provoca que el modelo que se somete al CV sea en base ?nicamente a la base de datos de entrenamiento dejando intacta una porci?n de los datos. Lo anterior provoca que podamos evaluar el poder predictivo del modelo final en base a una base de datos totalmente nueva (externa) que el modelo nunca ha visto. Como se ha mencionado anteriormente, el ML est? enfocado en la capacidad predictiva (y no explicativa) por lo que esta variaci?n del CV permitir?a evaluar de mejor manera esta capacidad.

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
  # Aplicado ?nicamente al set de entrenamiento (distinto de la clase anterior)
  data = Train1,
  method = "rpart",
  trControl = fitControl
)

# COMPLEXITY PARAMETER

plot(Model1)

# ?RMSE? El Root Mean Square Error, al igual que el R2, es una m?trica de ajuste del modelo (GOF) donde un menor RMSE implica un mejor ajuste de los datos predichos con respecto a los observados.

# De esta manera, el param?tro de complejidad est? indicando que el modelo con mayor complejidad (el m?s cercano a 0) va a ser el que presente un menor RMSE, es decir, el modelo m?s complejo presenta el mejor ajuste. Esto es v?lido para los an?lisis de regresi?n realizados con ML que se encuentran enfocados a la predicci?n de datos.

# EVALUACI?N CON BASE DE DATOS INTERNA
# Producto de que ahora nos encontramos en un modelo de regresi?n las m?trica de ajuste entregadas por la funci?n postResample() son RMSE, R2 y MAE.

caret::postResample(
  pred = stats::predict(
    # object es el modelo
    object = Model,
    newdata = Train1
  ),
  obs = Train1$presence
)

# EVALUACI?N CON BASE DE DATOS EXTERNA

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

# La temperatura del mes m?s fr?o es la variable m?s importante para determinar la presencia o ausencia del guanaco.

# A diferencia de los otros modelos que hemos visto en el curso hasta ahora, los algoritmos de ML no toman en cuenta la MULTICOLINEARIDAD que pueda existir en las variables explicativas al momento del entrenamiento.

# MAPAS
# El modelo obtenido puede ser aplicado a mapas ya que estos cuentan con las variables explicativas evaluadas. Para esto se emplea la funci?n predict() del paquete raster la cual presenta argumentos distintos a los que se encuentran en stats::predict().

Map <- raster::predict(
  # object hace referencia al Raster con las variables
  object = SA, 
  model = Model
  )

# Mapa resultado

plot(Map)

# OTROS ALGORITMOS ML PARA REGRESION
# gmb y rf son algoritmos de ML basados en la integraci?n (promedio) de numerosos ?rboles de decisi?n por lo que consumen m?s tiempo y memoria para el entrenamiento de los modelos. Estos dos modelos mencionados anteriormente son considerados como los "mejores". El boosting aplicado en gbm consiste en la aplicaci?n de muchos modelos ineficientes para desarrollar 1 bueno.


# Stochastic Gradient Boosting (boosted regression trees)
Model2 <- caret::train(
  presence ~ .,
  data = Train1,
  method = "gbm",
  trControl = fitControl
)

# Este algoritmo gbm no puede ser representado gr?ficamente como un ?nico arbol de decisi?n ya que de por s? est? compuesto por muchos de estos ?rboles de decisi?n.

# Influencia de las variables

summary(Model2)

# A diferencia del algoritmo de rpart, que s?lo presentaba el par?metro de complejidad (CP), gbm muestra 2 par?metros: (1) Boosting iterations ? n.trees (n? de modelos o iteraciones) y (2) Max Tree Depth ? interaction.depth (n? de divisiones).

plot(Model2)

# Del gr?fico se puede concluir que con 150 iteraciones de boosting y una profundidad m?xima de a?rbol igual a 3 se minimiza el RMSE del modelo, pero estos no son los ?nico par?metros que influyen en la modelaci?n...

# ...Expandiendo el conocimiento de los par?metros con otros GRID:
# Por default s?lo observamos 2 de los 4 par?metros que se pueden modificar en el gbm modelado (https://topepo.github.io/caret/available-models.html). Podemos explorar de mejor manera cuales son los valores de estos par?metros que maximizan o minimizan la m?trica deseada a trav?s de la funci?n expand.grid() la cual permite obtener todas las combinaciones posibles de par?metros.

gbmGrid <- base::expand.grid(
  # Porfundidad de interacci?n
  interaction.depth = c(1,5,9),
  # n? arboles = n? iteraciones (cada arbol = 1 iteracion)
  n.trees = (1:30)*50,
  # rapidez de aprendizaje del modelo
  shrinkage = c(0.1, 0.01),
  # n? m?nimo de obs por nudo
  n.minobsinnode = c(10,30)
)

# Al indicar estas nuevas combinaciones de par?metros le estamos entregando al modelo otra fuente de variaci?n en el entrenamiento de los datos. Cada una de las iteraciones realizadas en el CV debe realizarse para cada combinaci?n indicada de los parametros. Por default al realizar gbm caret provoca una variaci?n de 9 combinaci?n de par?metros para cada CV. Si aplicamos la grilla expandida anterior el factor multiplicativo es 360 y no 9.

# Random Forest (default)
Model3 <- caret::train(
  presence ~ .,
  data = Train1,
  method = "rf",
  trControl = fitControl
)

plot(Model3)

# ?Existe alguna funci?n que te permita modelar todos estos algoritmos y que entregue una selecci?n de los mejores? NO. No ser?a recomendable por lo lento que ser?a entrenar tal cantidad de algoritmos de ML (los cuales var?an mucho en su complejidad).

# M?TRICAS DE MODELOS ML
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
  
# COMPARACI?N DE ALGORITMOS ML
# M?todo de colecci?n, an?lisis y visualizaci?n de un set de datos resampleados

# A partir de los resamples se obtienen distintas muestras de cada uno de los n-repeated k-fold crossvalidations realizados (indicado en los argumentos de train)

Comp <- caret::resamples(
  x = list(
    Rpart = Model1,
    GBM = Model2,
    RF = Model3
  )
)

Comp$values

# ?Cuales son las diferencias entre estos algoritmos?

Difs <- base::diff(Comp)

Difs$difs

# Density plot

lattice::densityplot(
  x = Difs,
  metric = "Rsquared",
  auto.key = T,
  pch = "|"
)

# Este gr?fico cuantifica las diferencias (restas) que existen entre los distintos algoritmos. De esto podemos obtener las siguientes conclusiones:
#* (1) RF presenta valores de R2 ligeramente m?s altos que GBM y es por esto que hay una alta cantidad de datos que se encuentran ligeramente negativos.
#* (2) Rpart, con cualquier de los otros dos algoritmos, presenta valores mucho m?s bajos de R2 (y es por eso que las gr?ficas se encuentran m?s centradas en valores m?s negativos)

# Boxplot horizontal

lattice::bwplot(x = Difs, metric = "Rsquared")

# Mapas de los algoritmos GBM y RF
{
  # Predicci?n raster
  map(.x = list(Model2, Model3),
      .f = ~ raster::predict(object = SA,
                             model = .x)) %>%
    # Asignaci?n a symbols
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

# Podemos observar que mientras que el algoritmo de rpart entregaba como resultado 3 posibles valores de ocupancia (0.012, 0.043 y 0.9) los algoritmos de gbm y rf presentan un an?lisis de regresi?n mucho m?s detallado.


# Regresi?n GLM binomial --------------------------------------------------


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

# Selecci?n de modelos

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
  # IMPORTANTE: el tipo de predicci?n obtenida
  type = "response"
)

# El argumento type = "response" indica que la predicci?n sea con el valor de y transformado seg?n la ecuaci?n enlace (link). Esto es sumamente relevante para los glm y los nls donde la predicci?n se hace en base a una variable y transformada a alg?n comportamiento (log, exponencial, potencial, inverso, logit, etc). El argumento por default calcula las predicciones en base a la escala de los predictores lineales (y transformada).

plot(MapGLM)


# Paralelizaci?n ----------------------------------------------------------
# (secci?n de la 10ma clase del curso)

# La gran demanda de computo y tiempo de realizar modelos complejos y en grandes cantidades hacen necesario el recurrir a la paralelizaci?n. La mayor?a de los computadores que se encuentran disponibles en el mercado presentan m?s de 1 n?cleo (core). En este contexto de programaci?n esto significa que los computadores con m?ltiples cores pueden realizar los computos y tareas de manera paralela y no secuencial para despues unir los resultados obtenidos por cada uno de los procesamientos.

# ?Cuantos cores tengo en el computador?

parallel::detectCores()

# Creaci?n de una serie de copias de R corriendo en paralelo y comunicandose entre ellas

parallel::makePSOCKcluster(8) %>% 
  # Registro del trabajo en paralelo
  doParallel::registerDoParallel()
  
# De esta manera, cualquier c?digo que corramos proveniente de caret ser? paralelizado

Model2_exp <- caret::train(
  presence ~ .,
  data = Train1,
  method = "gbm",
  trControl = fitControl,
  # 360 combinaciones de los valores de par?metros en vez de 9 (default)
  tuneGrid = gbmGrid
)

plot(Model2_exp)

# Podemos observar un gr?fico mucho m?s completo acerca del comportamiento del modelo en funci?n de los distintos par?metros que componen un gbm. De esta manera, la minimizaci?n del RMSE ocurre cuando el n? de iteraciones (arboles) es igual a 1500, con una profundidad de modelaje de 9, una tasa de aprendizaje (shrinkage) de 0.01 y un n? m?nimo de observaciones por nudo igual a 10.

# Una vez finalizada la tarea es necesario detener el trabajo en paralelo ya que tendremos m?ltiples ventanas de R realizando tareas lo cual consume una importante porci?n de la RAM.

# makeCluster() tambien funciona
parallel::makePSOCKcluster(8) %>% 
  # Detener el trabajo en paralelo
  parallel::stopCluster()
