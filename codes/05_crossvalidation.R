# Apuntes -----------------------------------------------------------------


# Existen ciertas ocasiones en las que los distintos criterios de informaci?n (AICc, AIC y BIC) no pueden ser empleados como base para la selecci?n de modelos.

# (1) No se pueden calcular. Los IC contienen el LogLikelihood en sus ecuaciones el cual no siempre est? definido para los modelos ? el programa no puede determinarlo f?cilmente.

# (2) No se cumplen los supuestos de los criterios de informaci?n.

# (3) Los distintos tipos de modelos (lm, glm, gam, rpart, etc) no pueden ser comparados bajo un enfoque de criterios de informaci?n.

# (4) Se aplican distintas transformaciones a la variable respuesta (glm en su funci?n enlace)

# (5) Cuando existen varios t?rminos polinomiales no es recomendable realizar un promedio de modelos o basarse en criterios de informaci?n para su selecci?n.

# k-fold crossvalidation
# ?Cuales son las alternativas a los m?todos basados en los criterios de informaci?n para la selecci?n de modelos? Crossvalidation. Anteriormente, las bases de datos hab?an sido divididas en una base de entrenamiento y otra de testeo con el fin de obtener los poderes explicativos y predictivos del modelo. Esto correspond?a a una simplificaci?n del proceso donde ahora, mediante el crossvalidation, corresponde realizar el subseteo anterior de manera iterativa (k) de manera que la construcci?n del modelo vaya variando en cuanto a las bases de datos empleadas para construirlo y validarlo.

# Los pasos para realizar el k-fold crossvalidation son:
#* (1) Dividir aleatoriamente la base de datos en k grupos distintos.
#* (2) Entrenar los modelos con k-1 grupos dejando un grupo como testeo. 
#* (3) Testeo con el grupo ki.
#* (4) Se calcula el promedio y desviaci?n de una medida de desempe?o del modelo como el R2, RMSE, MAE, etc.

# Que el modelo permita poder inferir comportamientos en nuevos sets de datos significa que, de cierto modo, se ha cumplido el objetivo de la selecci?n de modelos en cuanto a evitar el sobreajuste ya que el poder predictivo de los datos es suficientemente alto.

# Crossvalidation, Bootstrap y Leave one out
# Otras t?cnicas orientadas a la selecci?n de modelos (evitando un sobreajuste) similares al crossvalidation son el bootstrapping y al Leave one out. El bootstrapping es una t?cnica m?s antigua (pero que sigue funcionando muy bien) empleada principalmente para obtener desviaciones en m?todos que por s? mismos no poseen desviaci?n. 

# El m?todo Leave one out corresponde a un crossvalidation en donde el n?mero de k-folds que divide a los datos es el m?s alto en lo posible. De esta manera, cada observaci?n es un fold y se emplean k-1 folds (muestras) en el entrenamiento y el restante para testeo. Es decir, se predice una ?nica observaci?n a partir de un modelo construido con el resto de las muestras en los datos. Consecuentemente, esta t?cnica requiere mucha capacidad del ordenador y tiempo para ser implementada (si se tienen 10000 datos eso se traduce en 10000 modelos distintos).
# Esta t?cnica de leave one out es imposible de aplicar en una regresi?n (no puede haber una regresi?n de un s?lo punto). Debido a esto, si se quiere realizar una regresi?n no se debiese dejar menos de 5 puntos en la base de datos de testeo.

# k-fold repeated crossvalidation
# Tras el k-fold crossvalidation obtenemos un promedio de una m?trica pero esta puede ir variando de acuerdo a la divisi?n en los k folds, incurriendo en una especie de sesgo.

# Es una forma m?s avanzada del crossvalidation visto anteriormente que permite reducir a?n m?s el sesgo. Esta t?cnica consiste en realizar un k-fold crossvalidation (divisi?n en k grupos) repetidas n veces. De esta manera, "10-repeated 5-fold crossvalidation" significa obtener 50 valores de R2 lo cual al promediarlo da una m?trica mucho m?s robusta.


# Modelacion --------------------------------------------------------------

library(caret)
library(tidyverse)
library(MuMIn)
library(broom)
library(broom.mixed)

data("mtcars")

attach(mtcars)

fit1 <- lm(formula = mpg ~ hp)
fit2 <- lm(formula = mpg ~ hp + I(hp ^ 2))
fit3 <- lm(formula = mpg ~ hp + I(hp ^ 2) + I(hp ^ 3))
fit4 <- lm(formula = mpg ~ hp + I(hp ^ 2) + I(hp ^ 3) + I(hp ^ 4))
fit5 <- lm(formula = mpg ~ hp + I(hp ^ 2) + I(hp ^ 3) + I(hp ^ 4) + I(hp ^ 5))
fit6 <- lm(formula = mpg ~ hp + I(hp ^ 2) + I(hp ^ 3) + I(hp ^ 4) + I(hp ^ 5) + I(hp ^ 6))

models <- list(fit1, fit2, fit3, fit4, fit5, fit6)

# map(
#   .x = 1:6,
#   .f = ~lm(mpg ~ poly(hp, .x, raw = T))
# )

# Comparaci?n modelos con distintos grados polinomiales de la base de datos mtcars. En la selecci?n de estos modelos es posible que el delta no sea menor a 2 por lo que no se podr?a realizar un promedio de modelos y ser?a m?s recomendable realizar un crossvalidation.

SelectedMods <- MuMIn::model.sel(object = models)

# AKAIKE WEIGHTS

models %>% 
  purrr::map_dbl(.f = ~ MuMIn::AICc(.x)) %>% 
  # Akaike weights (Normalized model likelihoods)
  MuMIn::Weights()

# I. N-REPEATED K-FOLD CROSSVALIDATION. PARA 1 MODELO...

# El crossvalidation emplea un uso de datos de manera aleatoria

set.seed(2020)

ctrl <- caret::trainControl(
  method = "repeatedcv", 
  # k-folds
  number = 5,
  # n-repeats
  repeats = 50)

DF <- caret::train(
  mpg ~ hp, 
  # Aplicado a todo el set de datos
  data = mtcars,
  method = "lm",
  trControl = ctrl)

# M?tricas obtenidas de cada fold de cada repetici?n

DF$results

DF$control

DF$resample

# Al realizar un histograma podemos observar la frecuencia de los valores de R2 obtenidos 

DF$resample %>% pull(Rsquared) %>% hist()

DF$resample %>% 
  select(Rsquared, Resample) %>% 
  summarise_if(is.numeric, .funs = c(Mean = mean, SD = sd))

# ?Qu? ocurre con la construcci?n del modelo en el crossvalidation? Se construye un modelo basado en todos los datos presentes 

DF$finalModel %>% glance()

DF$finalModel %>% AICc()

# II. ...? PARA N MODELOS

# Creaci?n de f?rmulas

form1 <- "mpg ~ hp"
form2 <- "mpg ~ hp + I(hp ^ 2)"
form3 <- "mpg ~ hp + I(hp ^ 2) + I(hp ^ 3)"
form4 <- "mpg ~ hp + I(hp ^ 2) + I(hp ^ 3) + I(hp ^ 4)"
form5 <- "mpg ~ hp + I(hp ^ 2) + I(hp ^ 3) + I(hp ^ 4) + I(hp ^ 5)"
form6 <- "mpg ~ hp + I(hp ^ 2) + I(hp ^ 3) + I(hp ^ 4) + I(hp ^ 5) + I(hp ^ 6)"

forms <- list(form1, form2, form3, form4, form5, form6)
K = 2:7

ctrl <- caret::trainControl(method = "repeatedcv", number = 5, repeats = 50)

# Tabla con todos los modelos, cada uno de ellos construido a partir de 50 repeticiones de 5-fold crossvalidation. De esta manera, cada uno de estos modelos y valores de m?tricas han sido construidos a partir de 250 modelos distintos (repeticiones).

Tests <- forms %>%
  map(.f =  ~ train(
    as.formula(.x),
    data = mtcars,
    method = "lm",
    trControl = ctrl,
    metric = "MAE"
  )) %>%
  map(.f =  ~ as_tibble(.x$resample)) %>%
  map(.f =  ~ select(.x,
                     # Escoger R2
                     Rsquared)) %>%
  map(.f =  ~ summarise_all(.x,
                            .funs = c(Mean = mean, SD = sd),
                            na.rm = T)) %>%
  map2(.y = forms,
       .f = ~ mutate(.x,
                     # Crear una columna con las f?rmulas
                     model = .y)) %>%
  reduce(.f = bind_rows) %>%
  mutate(K = 2:7) %>%
  arrange(desc(Mean))

# Se destacan tres cosas importantes con respecto al resultado del crossvalidation: (1) siempre se escoge el primer y mejor modelo, no se realizan promedios, (2) a pesar de que el valor promediado puede variar levemente se observa que el orden obtenido en los distintos modelos es el mismo, independiente de los distintos set seeds que se realizan, y (3) que este resultado concuerda con la selecci?n de modelos m?ltiples realizada anteriormente por lo que se concluye que el crossvalidation tambien permite seleccionar modelos con la mayor parsimon?a.

# De esta manera, el crossvalidation puede ser empleado para comparar entre distintos tipos de modelos entre s? (mezcla de estos).
