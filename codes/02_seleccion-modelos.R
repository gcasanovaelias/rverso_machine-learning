# Packages ----------------------------------------------------------------

library(MuMIn)
library(broom)
library(tidyverse)

# Multi Model Inference ---------------------------------------------------

# La inferencia mulimodelo es una herramienta poderosa que nos permite trabajar con multiplicidad de modelo para; (1) seleccionar el o los modelos m?s parsimoniosos entre varios en competencia (no el que predice m?s), (2) entrega una estrategia para seleccionar modelos.

# Por otro lado, este marco NO permite realizar algunas cosas tales como; (1) Arreglar un estudio mal dise?ado, (2) predecir en base a variables no relacionadas, (3) darnos una receta.

# De esta manera, para realizar una inferencia multimodelo adecuada se requiere de (1) un buen dise?o de muestreo/experimental, (2) generaci?n cuidadosa de las hip?tesis, y (3) una selecci?n adecuada de variables para distinguir entre hip?tesis.

# La parsimon?a se refiere a la capacidad de los modelos de explicar bien la variabilidad observada sin la necesidad de ser excesivamente complejo en cuanto al n?mero de variables. Es un balance entre sencillez y una buena explicaci?n para un modelo.

# Bibliograf?a recomendada: "Model Selection and Multi-Model Inference" de Anderson & Burnham (2004)

# https://derek-corcoran-barrios.github.io/

# Modelamiento ------------------------------------------------------------

# IMPORTANTE: Esto tambien se encuentra cubierto en el script "4 Modelos" de la clase de "An?lisis y Manipulaci?n de datos".

data("mtcars")

fit1 <- lm(mpg ~ carb + cyl, data = mtcars)
fit2 <- lm(mpg ~ cyl + wt, data = mtcars)
fit3 <- lm(mpg ~ am + qsec + wt, data = mtcars)
fit4 <- lm(mpg ~ carb + cyl + wt, data = mtcars)
fit5 <- lm(mpg ~ am + carb + cyl + qsec + wt, data = mtcars)
fit6 <- lm(mpg ~ am + carb + cyl + hp + qsec, data = mtcars)

models <- list(fit1, fit2, fit3, fit4, fit5, fit6)

# Generaci?n de una tabla de selecci?n a partir de una funci?n de MuMIn model.selc(). A diferencia de dredge(), model.sel() genera la misma tabla de seleccion de ciertos modelos seleccionados (especificados) mientras que dredge genera dicha tabla con una selecci?n de modelos automatizada con todas las combinaciones de variables posibles de acuerdo a un modelo original.

Select <- MuMIn::model.sel(models)

# "weight" hace referencia a los pesos de Akaike haciendo referencia a la confianza relativa que se tiene en ese modelo con respecto al resto (al sumar todos los pesos de Akaike estos suman 1). A mayor peso mejor el modelo pero, al igual que los IC, estos pesos no tienen un valor en s? mismos si no que cobran relevancia cuando se comparan con el resto de los modelos (dependen del n?mero de estos).

# "delta" corresponde a la diferencia entre el IC del mejor modelo con el respectivo a esa fila. El emplear un delta menor o igual a 2 para seleccionar los modelos a revisar es un consenso basado en muchas simulaciones realizadas y que se ocupa en la mayor?a de la literatura (tal y como empleamos un p-value menor a 0.05 para rechazar la HO). 

Selected <- subset(x = Select, delta <= 2)

# ?Qu? modelo seleccionamos?

# ...?seleccionar el mejor modelo?

BestModel <- MuMIn::get.models(object = Select, 
                               # Escoger 1 s?lo modelo
                               subset = 1)[[1]]

broom::glance(BestModel)

# El que un par?metro no sea significativo no importa bajo la l?gica de la inferencia multimodelo. Lo que s? importa es que el modelo es el mejor sustentado en t?rminos de parsimon?a en relaci?n a los otros modelos en competencia.

# ...?promediar modelos?

# Existe una postura que manifiesta que aquellos modelos que se encuentran dentro del rango menor o igual a 2 delta debiesen ser promediados, no ?nicamente escoger el primero.

# El objeto entregado por get.models() es un "model.selection", as? que es mejor cambiarlo a un data frame moderno

S <- as_tibble(Selected) %>%
  select(cyl, weight)


# Existen dos m?todos para promediar modelos

#... el m?todo full: Para obtener el par?metro promediado se procede a sumar el producto entre los par?metros de los modelos seleccionados con sus respectivos weight (los cuales son una proporci?n que suma 1). En el caso de que se tenga un NA este se traduce a un 0.

S_full <- S
S_full$cyl <- ifelse(test = is.na(S_full$cyl),
                     yes = 0,
                     no = S_full$cyl)
S_full <- S_full %>% mutate(Theta_i = cyl * weight)

Cyl_hat <- sum(S_full$Theta_i)

# ... y el m?todo subset: Similar al m?todo full con la salvedad que se eliminan los modelos en los que la variable de inter?s no se encuentre presente. Debido a que la suma de los peso no van a dar igual a 1 es que la suma del producto entre los weight y los coeficientes va a ser dividida por la suma de los weight.

S_sub <- S %>% filter(!is.na(cyl))

S_sub <- S_sub %>% mutate(Theta_i = cyl * weight)

Cyl_hat <- sum(S_sub$Theta_i)/sum(S_sub$weight)

# M?todo MuMIn con model.avg(). Realizar lo que hicimos anteriormente de forma manueal pero de una manera m?s automatizada.

AverageModel <- model.avg(object = Select, subset = delta <=2, fit = T)

AverageModel$coefficients

# ?Cual ocupar? Generalmente el modelo promediado full se asemeja m?s al mejor modelo por lo que es recomendado para hacer predicciones pero tiene la desventaja de que sus par?metros se ven mucho m?s alterados. De esta manera, el m?todo subset es recomendado para interpretaci?n, es decir, cuando se quiere estudiar la influencia de las variables en el modelo.


# Multicolinearidad -------------------------------------------------------

# Multicolinearidad hace referencia a la correlaci?n que existe entre las variables explicativas del modelo. Al momento de modelar se debiese tener en cuenta la relaci?n que existe entre las variables ya que, de  otro modo, el incorporar variables muy relacionadas entre s? traer?a errores y problemas a la regresi?n. De este modo, y por convenci?n, cuando la correlaci?n es mayor a 0.7 esto significa que dichas variables presentan una correlaci?n considerable.

bloodpress <- read_delim("https://online.stat.psu.edu/onlinecourses/sites/stat501/files/data/bloodpress.txt", delim = "\t", escape_double = FALSE, col_types = cols(Pt = col_skip()), trim_ws = TRUE)

bloodpress %>% select(-BP) %>% cor() %>% corrplot::corrplot(diag = F)

model1 <- lm(BP ~ BSA, data = bloodpress)
model2 <- lm(BP ~ Weight, data = bloodpress)
model3 <- lm(BP ~ BSA + Weight, data = bloodpress)

# Existe una alta correlaci?n entre las variables BSA y Weight. Al realizar regresiones lineales simples con cada una podemos observar que ambas presentan ciertos valores de par?metros (y significativos) que disminuyen dr?sticamente al incluir las dos variables al mismo tiempo en una regresi?n lineal m?ltiple (y que ya no presentan significancia). Esta es la principal raz?n por la que se quiere evitar la multicolinearidad al momento de modelar, un cambio en la estimaci?n de los par?metros trae consecuencias al momento de realizar predicciones as? como tambien en la interpretaci?n de las relaciones causales.

# Si realizamos una selecci?n de modelos mediante inferencia multiestad?stica no debiesemos tener problemas de multicolinearidad, es decir, no debiesen ser seleccionados aquellos modelos que presenten variables altamente correlacionadas. Pero, tal y como vemos abajo, esto no es as? ya que los mejores modelos (menor AIC) s? presentan multicolinearidad.
