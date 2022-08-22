# Packages ----------------------------------------------------------------

library(MuMIn)
library(broom)
library(tidyverse)

# Multi Model Inference ---------------------------------------------------

# La inferencia mulimodelo es una herramienta poderosa que nos permite trabajar con multiplicidad de modelo para; (1) seleccionar el o los modelos más parsimoniosos entre varios en competencia (no el que predice más), (2) entrega una estrategia para seleccionar modelos.

# Por otro lado, este marco NO permite realizar algunas cosas tales como; (1) Arreglar un estudio mal diseñado, (2) predecir en base a variables no relacionadas, (3) darnos una receta.

# De esta manera, para realizar una inferencia multimodelo adecuada se requiere de (1) un buen diseño de muestreo/experimental, (2) generación cuidadosa de las hipótesis, y (3) una selección adecuada de variables para distinguir entre hipótesis.

# La parsimonía se refiere a la capacidad de los modelos de explicar bien la variabilidad observada sin la necesidad de ser excesivamente complejo en cuanto al número de variables. Es un balance entre sencillez y una buena explicación para un modelo.

# Bibliografía recomendada: "Model Selection and Multi-Model Inference" de Anderson & Burnham (2004)

# https://derek-corcoran-barrios.github.io/

# Modelamiento ------------------------------------------------------------

# IMPORTANTE: Esto tambien se encuentra cubierto en el script "4 Modelos" de la clase de "Análisis y Manipulación de datos".

data("mtcars")

fit1 <- lm(mpg ~ carb + cyl, data = mtcars)
fit2 <- lm(mpg ~ cyl + wt, data = mtcars)
fit3 <- lm(mpg ~ am + qsec + wt, data = mtcars)
fit4 <- lm(mpg ~ carb + cyl + wt, data = mtcars)
fit5 <- lm(mpg ~ am + carb + cyl + qsec + wt, data = mtcars)
fit6 <- lm(mpg ~ am + carb + cyl + hp + qsec, data = mtcars)

models <- list(fit1, fit2, fit3, fit4, fit5, fit6)

# Generación de una tabla de selección a partir de una función de MuMIn model.selc(). A diferencia de dredge(), model.sel() genera la misma tabla de seleccion de ciertos modelos seleccionados (especificados) mientras que dredge genera dicha tabla con una selección de modelos automatizada con todas las combinaciones de variables posibles de acuerdo a un modelo original.

Select <- MuMIn::model.sel(models)

# "weight" hace referencia a los pesos de Akaike haciendo referencia a la confianza relativa que se tiene en ese modelo con respecto al resto (al sumar todos los pesos de Akaike estos suman 1). A mayor peso mejor el modelo pero, al igual que los IC, estos pesos no tienen un valor en sí mismos si no que cobran relevancia cuando se comparan con el resto de los modelos (dependen del número de estos).

# "delta" corresponde a la diferencia entre el IC del mejor modelo con el respectivo a esa fila. El emplear un delta menor o igual a 2 para seleccionar los modelos a revisar es un consenso basado en muchas simulaciones realizadas y que se ocupa en la mayoría de la literatura (tal y como empleamos un p-value menor a 0.05 para rechazar la HO). 

Selected <- subset(x = Select, delta <= 2)

# ¿Qué modelo seleccionamos?

# ...¿seleccionar el mejor modelo?

BestModel <- MuMIn::get.models(object = Select, 
                               # Escoger 1 sólo modelo
                               subset = 1)[[1]]

broom::glance(BestModel)

# El que un parámetro no sea significativo no importa bajo la lógica de la inferencia multimodelo. Lo que sí importa es que el modelo es el mejor sustentado en términos de parsimonía en relación a los otros modelos en competencia.

# ...¿promediar modelos?

# Existe una postura que manifiesta que aquellos modelos que se encuentran dentro del rango menor o igual a 2 delta debiesen ser promediados, no únicamente escoger el primero.

# El objeto entregado por get.models() es un "model.selection", así que es mejor cambiarlo a un data frame moderno

S <- as_tibble(Selected) %>%
  select(cyl, weight)


# Existen dos métodos para promediar modelos

#... el método full: Para obtener el parámetro promediado se procede a sumar el producto entre los parámetros de los modelos seleccionados con sus respectivos weight (los cuales son una proporción que suma 1). En el caso de que se tenga un NA este se traduce a un 0.

S_full <- S
S_full$cyl <- ifelse(test = is.na(S_full$cyl),
                     yes = 0,
                     no = S_full$cyl)
S_full <- S_full %>% mutate(Theta_i = cyl * weight)

Cyl_hat <- sum(S_full$Theta_i)

# ... y el método subset: Similar al método full con la salvedad que se eliminan los modelos en los que la variable de interés no se encuentre presente. Debido a que la suma de los peso no van a dar igual a 1 es que la suma del producto entre los weight y los coeficientes va a ser dividida por la suma de los weight.

S_sub <- S %>% filter(!is.na(cyl))

S_sub <- S_sub %>% mutate(Theta_i = cyl * weight)

Cyl_hat <- sum(S_sub$Theta_i)/sum(S_sub$weight)

# Método MuMIn con model.avg(). Realizar lo que hicimos anteriormente de forma manueal pero de una manera más automatizada.

AverageModel <- model.avg(object = Select, subset = delta <=2, fit = T)

AverageModel$coefficients

# ¿Cual ocupar? Generalmente el modelo promediado full se asemeja más al mejor modelo por lo que es recomendado para hacer predicciones pero tiene la desventaja de que sus parámetros se ven mucho más alterados. De esta manera, el método subset es recomendado para interpretación, es decir, cuando se quiere estudiar la influencia de las variables en el modelo.


# Multicolinearidad -------------------------------------------------------

# Multicolinearidad hace referencia a la correlación que existe entre las variables explicativas del modelo. Al momento de modelar se debiese tener en cuenta la relación que existe entre las variables ya que, de  otro modo, el incorporar variables muy relacionadas entre sí traería errores y problemas a la regresión. De este modo, y por convención, cuando la correlación es mayor a 0.7 esto significa que dichas variables presentan una correlación considerable.

bloodpress <- read_delim("https://online.stat.psu.edu/onlinecourses/sites/stat501/files/data/bloodpress.txt", delim = "\t", escape_double = FALSE, col_types = cols(Pt = col_skip()), trim_ws = TRUE)

bloodpress %>% select(-BP) %>% cor() %>% corrplot::corrplot(diag = F)

model1 <- lm(BP ~ BSA, data = bloodpress)
model2 <- lm(BP ~ Weight, data = bloodpress)
model3 <- lm(BP ~ BSA + Weight, data = bloodpress)

# Existe una alta correlación entre las variables BSA y Weight. Al realizar regresiones lineales simples con cada una podemos observar que ambas presentan ciertos valores de parámetros (y significativos) que disminuyen drásticamente al incluir las dos variables al mismo tiempo en una regresión lineal múltiple (y que ya no presentan significancia). Esta es la principal razón por la que se quiere evitar la multicolinearidad al momento de modelar, un cambio en la estimación de los parámetros trae consecuencias al momento de realizar predicciones así como tambien en la interpretación de las relaciones causales.

# Si realizamos una selección de modelos mediante inferencia multiestadística no debiesemos tener problemas de multicolinearidad, es decir, no debiesen ser seleccionados aquellos modelos que presenten variables altamente correlacionadas. Pero, tal y como vemos abajo, esto no es así ya que los mejores modelos (menor AIC) sí presentan multicolinearidad.
