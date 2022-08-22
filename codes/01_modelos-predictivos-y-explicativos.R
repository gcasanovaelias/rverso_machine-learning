# Packages ----------------------------------------------------------------

library(tidyverse)
library(MuMIn)
library(caret)
library(broom)


# Apuntes modelos ---------------------------------------------------------


# En el curso se hará distinción acerca de los modelos explicativos y predictivos. Mientras que la sección de modelos multivariados son más explicativos mientras que el machine learning se encuentra más dentro de los modelos predictivos

# Modelos estadísticos
#* Modelo no determinista (no espera realizar una predicción exacta) que utiliza una muestra de una población para intentar determinar patrones de esta
#* Simplificación numérica de la realidad con el objetivo de estudiar/descrivir o predecir un fenómeno o suceso
#* "Todos los modelos están equivocados, pero algunos de estos son útiles": Esto hace referencia a que todos los modelos están asociados a un error
#* Algunos ejemplos que vamos a observar en el curso son modelos de ANOVA, regresión lineal y regresiones no lineales

# Poder Explicativo != Poder Predictivo
#* La forma más fácil de entender el poder explicativo es a partir del R2 (% de la variación EXPLICADA por el modelo).
#* Por otro lado, para el poder predictivo tambien se puede emplear el R2 pero aplicado a una nueva base de datos (testeo)

# Ejemplo
# ¿Podemos explicar o predecur la eficiencia de combustible (mpg) a partir de los caballos de fuerza (hp) de un vehículo?

#* Para entender la diferencia entre modelos explicativos y predictivos se divide la base de datos en dos; de entrenamiento (ajustar el modelo y ver el poder explicativo) y de validación ó testeo (determinar si el modelo generado tiene un buen poder predictor)

data("mtcars")

force(mtcars)

set.seed(1)

index <- sample(
  # Vector que indica el conjunto de filas de los datos
  x = 1:nrow(mtcars), 
  # Tamaño de las muestras (mitad de los datos)
  size = round(nrow(mtcars) / 2))

# Datos de entrenamiento
train <- mtcars[index, ]

# Datos de validación
test <- mtcars[-index, ]


# Poder explicativo -------------------------------------------------------

#* Entrenamos el modelo ajustando un modelo lineal

modelo <- lm(mpg ~ hp + I(hp ^ 2), data = train)

# glance(): model statistics. Accepts a model object and returns a tibble with model summaries (goodness of fit measures)
broom::glance(modelo)

# tidy(): parameters statistics. Summarises information about components of a model (similar to summary())
broom::tidy(modelo)

# augment(): observation statistics. Accepts a dataset and adds information about each observation
broom::augment(modelo)

# Un R2 de 0.82 significa que el 82% de la variabilidad es debido al modelo (el 18% restante es del error) por lo que esto correspondería al poder explicativo

# Creamos una nueva columna con los valores predichos en la misma base de datos con la que armamos el modelo
# train$Pred <- predict(object = modelo, newdata = train) 

# Los datos que se le entregan a la función augment() pueden estar asociados a aquellos con los que se construyó el modelo (entregados por default) o un nuevo set de datos señalados con el argumento "newdata". A partir de esta modificación, la información agregada acerca de cada observación se limita a .fitted y .resid, pudiendo agregar intervalos y .se.fit si se desea.

# En este caso, como los datos con los que fue construido el modelo corresponden a train, no es necesario indicarle un newdata con los datos de entrenamiento. Esto permite que la información agregada por augment sea mayor (.hat, .sigma, .cooksd y .std.resid)

train_augment <- broom::augment(x = modelo, se_fit = T, interval = "confidence")

# Poder predictivo --------------------------------------------------------

# ¿Cómo es que el modelo se comporta en la predicción de eficiencia en una nueva base de datos?

# Hacemos una predicción del modelo en una nueva base de datos

# Creamos una nueva columna con los valores predichos
# test$Pred <- predict(object = modelo, newdata = test)

test_augment <- broom::augment(x = modelo, newdata = test, se_fit = T, interval = "prediction")

ggplot(data = test_augment, aes(x = hp, y = mpg)) +
  # Los puntos son los valores de mpg observados
  geom_point() +
  # La línea es la predicción de mpg según el modelo
  geom_line(aes(y = .fitted)) +
  theme_minimal()


# Poder predictivo vs explicativo -----------------------------------------

# caret: Paquete que permite calcular el R2 y otras medidas con los valores observados y los predichos para...

# ... el explicativo

caret::postResample(pred = train_augment$.fitted, obs = train_augment$mpg)

# ...o el poder predictivo

caret::postResample(pred = test_augment$.fitted, obs = test_augment$mpg)

# R2: Cuanto % de la variación es explicada por el modelo
# MAE: Cuanto es el error promedio del modelo

# IMPORTANTE: Usualmente el poder predictivo (R2) debiese ser menor que el poder explicativo. Para mejorar la predicción podemos incorporar más variables (y por consiguiente parámetros estimados) pero hay que tener en cuenta no sobreajustar el modelo


# Sobreajuste ------------------------------------------------------------

# Usualmente, al ir construyendo modelos, estamos tentados a ir mejorando indefinidamente el R2, lo cual puede ser bueno para los modelos explicativos pero detrimental para los predictivos. A este fenómeno se le llama complejizar o sobreparametrizar un modelo y comprende la adición de un número creciente de parámetros para cada una de las variables.

# Parámetros se refiere a las estimaciones realizadas por el modelo en cuanto al intercepto al origen (b0), pendiente del factor lineal (b1), pendiente del factor cuadrpatico (b2), etc.

# El ir agregando parámetros siempre va a mejorar el poder explicativo mientras que para el poder predictivo este puede experimentar un leve aumento al principio para luego ir disminuyendo rápidamente tras sobrepasar cierto número de parámetros. De esta manera, el sobreajuste inicia una vez que el poder predictivo empieza a disminuir.

# El sobreajuste implica que los valores obtenidos explican de manera tan fidedigna los datos con los que se construye el modelo que al ser aplicados a otro (poner a prueba su poder predictivo) este presenta errores muy grandes y evidentes.

# Una de las consecuencias de la sobreparametrización es que, a pesar de que el poder explicativo aumenta, el poder explicar el modelo y la importancia de los respectivos parámtros se complejiza afectando negativamente la discusión.

# ¿Cuando maximizar un poder u otro?
# Maximizar el poder predictivo es el objetivo de los estudios que se enfocan en saber la variabilidad de los fenómenos que sucederán en un tiempo determinado. Aquí el interés no es explicar el porqué de dicho comportamiento sino simplemente maximizar la capacidad de predicción de fenómenos.

# Por otro lado, maximizar el poder explicativo va de la mano con poder responder a las preguntas basadas en las relaciones causales de fenómenos. A diferencia de lo que ocurre en el caso anterior, el maximizar el poder explicativo va acompañado de una interpretación sólida de los resultados.

# ¿Debemos escogar explicación o predicción? Usualmente uno quiere mazimizar ambos poderes y para eso se emplean los criterios de información como AIC (Criterio de Información de Akaike), AICc o BIC (Criterio de Información Bayessiano.

# AIC = 2k - ln(L), donde k: número de parámetros y ln(L): log Likelihood. 

# El likelihood se refiere al R2, es decir, al ajuste del modelo  el cual va a ir aumentando siempre en la medida que se incorporan más parámetros al modelo. De la forma en la que está dispuesta la ecuación AIC esta tiende a premiar el ajuste pero castigar conforme el número de parámetros que se presente en el modelo. De esta manera, valores cada vez menores indican que el modelo presenta mayor parsimonía y existe una combinación más adecuada entre predicción y explicación. Por sí solos estos criterios de información no indican mucho excepto de manera comparativa.

AIC(modelo)

# AICc = AIC + (2k^2 + 2k)/(n - k -1), donde n: numero de observaciones.

# El AICc (Criterio de Información de Akaike Corregido) permite corregir el AIC obtenido anteriormente por el número de observaciones. Producto de que agrega un cuociente en donde k (n° parámetros) se encuentra al cuadrado, el AICc castiga mucho más severamente aquellos modelos que presentan un alto número de parámetros en comparación a AIC. Para hacer modelos más complejos necesito más puntos (observaciones) por lo que AICc castiga mucho menos a aquellos modelos que presenten una mayor cantidad de observaciones. Usualmente se recomienda emplear el AICc cuando el número de observaciones es bajo. De todos modos, cuando el número de observaciones es muy alto el cuociente tiende a 0 por lo que el IC estaría conformado únicamente por el AIC. De esta manera, siempre es mejor emplear AICc que AIC.

MuMIn::AICc(modelo)

# A diferencia del R2, el cual sigue aumentando al ir incorporando más parámetros, el AIC y los demás IC tienden a experimentar un mínimo muy marcado para luego ir incrementando su valor a medida que se incorporan variables. De esta manera, basar la selección de modelo según los criterios de información es mucho más robusto que hacerlo en base al R2.

# El BIC o Criterio de Información Bayessiano se emplea en experimentos de laboratorio muy controladas donde se tiene seguridad que las variables del estudio son las únicas que están afectando la variable dependiente. A diferencia del BIC, el AIC y AICc se emplean cuando las variables que se están evaluando no son las únicas existentes (se emplean más en los campos de ecología y recursos naturales.)


# Ejercicio ---------------------------------------------------------------

# Tomando la base de datos mtcars explora la relación entre AICc, R2 explicativo y R2 predictivo, para eso genera un df con las siguientes columnas


# Solución 1 --------------------------------------------------------------

set.seed(2018)

index <- sample(x = 1:nrow(mtcars),
                size = round(nrow(mtcars) / 2))

train <- mtcars[index, ]
test <- mtcars[-index, ]

# Modelo 1
{
# Se genera un tibble vacío que posteriormente se va a ir completando y llenando con los resultados obtenidos

df <- tibble(Formula = NA,
             Model = NA,
             K = NA,
             R2_Train = NA,
             R2_Test = NA, 
             AICc = NA)

# Como identificador vamos a ir agregando la ecuación misma del modelo 

df$Formula <- "mpg ~ hp + wt"

# Tibble es un formato moderno de data frame que, a diferencia de este último, te permite guardar en sus celdas un objeto de modelo

df$Model <- lm(formula = as.formula(df$Formula),
               data = train) %>% list()

# El tener acceso al modelo dentro del objeto data frame (como una lista) nos permite acceder fácilmente a las métricas de interés

df$R2_Train <- df$Model[[1]] %>% 
  glance() %>% 
  pull(r.squared)

# El número de parámetros (K) es, en este caso, igual a los degrees of freedom

df$K <- df$Model[[1]] %>% 
  glance() %>% 
  pull(df)

# AICc

df$AICc <- df$Model[[1]] %>% 
  MuMIn::AICc()

# R2 predictivo

df$R2_Test <- postResample(
  pred = predict(object = df$Model[[1]], 
                 newdata = test), 
  obs = test$mpg
  )[2]
}

# Modelo 2
{
df2 <- tibble(Formula = NA,
              Model = NA,
              K = NA,
              R2_Train = NA,
              R2_Test = NA, 
              AICc = NA)

df2$Formula <- "mpg ~ wt"

df2$Model <- lm(formula = as.formula(df2$Formula),
               data = train) %>% list()

df2$R2_Train <- df2$Model[[1]] %>% 
  glance() %>% 
  pull(r.squared)


df2$K <- df2$Model[[1]] %>% 
  glance() %>% 
  pull(df)

df2$AICc <- df2$Model[[1]] %>% 
  MuMIn::AICc()

df2$R2_Test <- postResample(
  pred = predict(object = df2$Model[[1]], 
                 newdata = test), 
  obs = test$mpg
)[2]
}

# Unión de los tibble
df_t <- df %>% bind_rows(df2)


# Solución 2 --------------------------------------------------------------

# Generamos el tibble con 4 filas

df <- tibble(Formula = rep(NA, 4),
             Model = rep(NA, 4),
             K = rep(NA, 4),
             R2_Train = rep(NA, 4),
             R2_Test = rep(NA, 4),
             AICc = rep(NA, 4))

# Guardamos el modelo como texto para usarlo despues

df$Formula <- c("mpg ~ hp + wt",
                "mpg ~ hp + disp",
                "mpg ~ wt + disp",
                "mpg ~ wt")

# Obtenemos las métricas mediante un loop

for (i in 1:nrow(df)) {
  df$Model[i] <- list(lm(as.formula(df$Formula[i]),
                         data = train))
  
  df$R2_Train[i] <- df$Model[i][[1]] %>% glance() %>% pull(r.squared)
  
  df$K[i] <- df$Model[i][[1]] %>% glance() %>% pull(df)
  
  df$AICc[i] <- df$Model[i][[1]] %>% AICc()
  
  df$R2_Test[i] <- postResample(pred = predict(df$Model[i][[1]], test),
                                obs = test$mpg)
}


# Solución 3 --------------------------------------------------------------

Data <- list()

for (i in c(1:12)) {
  Data[[i]] <-
    tibble(
      Formula = purrr::map(as.data.frame(combn(
        c(
          "cyl",
          "disp",
          "hp",
          "drat",
          "wt",
          "qsec",
          "vs",
          "am",
          "gear",
          "carb",
          "I(wt^2)",
          "I(hp^2)"
        ),
        i
      )), ~ paste(.x, collapse = "+")) %>%
        reduce(c),
      Model = purrr::map(as.data.frame(combn(
        c(
          "cyl",
          "disp",
          "hp",
          "drat",
          "wt",
          "qsec",
          "vs",
          "am",
          "gear",
          "carb",
          "I(wt^2)",
          "I(hp^2)"
        ),
        i
      )), ~ paste(.x, collapse = "+")) %>%
        purrr::map( ~ paste("mpg ~", .x)) %>%
        purrr::map(as.formula) %>%
        purrr::map( ~ lm(.x, data = train)),
      K = i + 1
    )
}

Data <- bind_rows(Data)

Data$R_2_Train <- NA

Data$R_2_Test <- NA

Data$AICc <- NA

for (i in 1:nrow(Data)) {
  Data$R_2_Train[i] <- R2(pred = predict(Data$Model[i][[1]], 
                                         train), obs = train$mpg)
  Data$R_2_Test[i] <- R2(pred = predict(Data$Model[i][[1]], 
                                        test), obs = test$mpg)
  Data$AICc[i] <- AICc(Data$Model[i][[1]])
}

Data <- Data %>% dplyr::select(-Model) %>% arrange(AICc)

# Multicolinearidad. Hace referencia a las correlaciones que existen entre las variables
