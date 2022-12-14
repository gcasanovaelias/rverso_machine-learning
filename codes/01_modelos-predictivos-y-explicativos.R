# Packages ----------------------------------------------------------------

library(tidyverse)
library(MuMIn)
library(caret)
library(broom)


# Apuntes modelos ---------------------------------------------------------


# En el curso se har? distinci?n acerca de los modelos explicativos y predictivos. Mientras que la secci?n de modelos multivariados son m?s explicativos mientras que el machine learning se encuentra m?s dentro de los modelos predictivos

# Modelos estad?sticos
#* Modelo no determinista (no espera realizar una predicci?n exacta) que utiliza una muestra de una poblaci?n para intentar determinar patrones de esta
#* Simplificaci?n num?rica de la realidad con el objetivo de estudiar/descrivir o predecir un fen?meno o suceso
#* "Todos los modelos est?n equivocados, pero algunos de estos son ?tiles": Esto hace referencia a que todos los modelos est?n asociados a un error
#* Algunos ejemplos que vamos a observar en el curso son modelos de ANOVA, regresi?n lineal y regresiones no lineales

# Poder Explicativo != Poder Predictivo
#* La forma m?s f?cil de entender el poder explicativo es a partir del R2 (% de la variaci?n EXPLICADA por el modelo).
#* Por otro lado, para el poder predictivo tambien se puede emplear el R2 pero aplicado a una nueva base de datos (testeo)

# Ejemplo
# ?Podemos explicar o predecur la eficiencia de combustible (mpg) a partir de los caballos de fuerza (hp) de un veh?culo?

#* Para entender la diferencia entre modelos explicativos y predictivos se divide la base de datos en dos; de entrenamiento (ajustar el modelo y ver el poder explicativo) y de validaci?n ? testeo (determinar si el modelo generado tiene un buen poder predictor)

data("mtcars")

force(mtcars)

set.seed(1)

index <- sample(
  # Vector que indica el conjunto de filas de los datos
  x = 1:nrow(mtcars), 
  # Tama?o de las muestras (mitad de los datos)
  size = round(nrow(mtcars) / 2))

# Datos de entrenamiento
train <- mtcars[index, ]

# Datos de validaci?n
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

# Un R2 de 0.82 significa que el 82% de la variabilidad es debido al modelo (el 18% restante es del error) por lo que esto corresponder?a al poder explicativo

# Creamos una nueva columna con los valores predichos en la misma base de datos con la que armamos el modelo
# train$Pred <- predict(object = modelo, newdata = train) 

# Los datos que se le entregan a la funci?n augment() pueden estar asociados a aquellos con los que se construy? el modelo (entregados por default) o un nuevo set de datos se?alados con el argumento "newdata". A partir de esta modificaci?n, la informaci?n agregada acerca de cada observaci?n se limita a .fitted y .resid, pudiendo agregar intervalos y .se.fit si se desea.

# En este caso, como los datos con los que fue construido el modelo corresponden a train, no es necesario indicarle un newdata con los datos de entrenamiento. Esto permite que la informaci?n agregada por augment sea mayor (.hat, .sigma, .cooksd y .std.resid)

train_augment <- broom::augment(x = modelo, se_fit = T, interval = "confidence")

# Poder predictivo --------------------------------------------------------

# ?C?mo es que el modelo se comporta en la predicci?n de eficiencia en una nueva base de datos?

# Hacemos una predicci?n del modelo en una nueva base de datos

# Creamos una nueva columna con los valores predichos
# test$Pred <- predict(object = modelo, newdata = test)

test_augment <- broom::augment(x = modelo, newdata = test, se_fit = T, interval = "prediction")

ggplot(data = test_augment, aes(x = hp, y = mpg)) +
  # Los puntos son los valores de mpg observados
  geom_point() +
  # La l?nea es la predicci?n de mpg seg?n el modelo
  geom_line(aes(y = .fitted)) +
  theme_minimal()


# Poder predictivo vs explicativo -----------------------------------------

# caret: Paquete que permite calcular el R2 y otras medidas con los valores observados y los predichos para...

# ... el explicativo

caret::postResample(pred = train_augment$.fitted, obs = train_augment$mpg)

# ...o el poder predictivo

caret::postResample(pred = test_augment$.fitted, obs = test_augment$mpg)

# R2: Cuanto % de la variaci?n es explicada por el modelo
# MAE: Cuanto es el error promedio del modelo

# IMPORTANTE: Usualmente el poder predictivo (R2) debiese ser menor que el poder explicativo. Para mejorar la predicci?n podemos incorporar m?s variables (y por consiguiente par?metros estimados) pero hay que tener en cuenta no sobreajustar el modelo


# Sobreajuste ------------------------------------------------------------

# Usualmente, al ir construyendo modelos, estamos tentados a ir mejorando indefinidamente el R2, lo cual puede ser bueno para los modelos explicativos pero detrimental para los predictivos. A este fen?meno se le llama complejizar o sobreparametrizar un modelo y comprende la adici?n de un n?mero creciente de par?metros para cada una de las variables.

# Par?metros se refiere a las estimaciones realizadas por el modelo en cuanto al intercepto al origen (b0), pendiente del factor lineal (b1), pendiente del factor cuadrpatico (b2), etc.

# El ir agregando par?metros siempre va a mejorar el poder explicativo mientras que para el poder predictivo este puede experimentar un leve aumento al principio para luego ir disminuyendo r?pidamente tras sobrepasar cierto n?mero de par?metros. De esta manera, el sobreajuste inicia una vez que el poder predictivo empieza a disminuir.

# El sobreajuste implica que los valores obtenidos explican de manera tan fidedigna los datos con los que se construye el modelo que al ser aplicados a otro (poner a prueba su poder predictivo) este presenta errores muy grandes y evidentes.

# Una de las consecuencias de la sobreparametrizaci?n es que, a pesar de que el poder explicativo aumenta, el poder explicar el modelo y la importancia de los respectivos par?mtros se complejiza afectando negativamente la discusi?n.

# ?Cuando maximizar un poder u otro?
# Maximizar el poder predictivo es el objetivo de los estudios que se enfocan en saber la variabilidad de los fen?menos que suceder?n en un tiempo determinado. Aqu? el inter?s no es explicar el porqu? de dicho comportamiento sino simplemente maximizar la capacidad de predicci?n de fen?menos.

# Por otro lado, maximizar el poder explicativo va de la mano con poder responder a las preguntas basadas en las relaciones causales de fen?menos. A diferencia de lo que ocurre en el caso anterior, el maximizar el poder explicativo va acompa?ado de una interpretaci?n s?lida de los resultados.

# ?Debemos escogar explicaci?n o predicci?n? Usualmente uno quiere mazimizar ambos poderes y para eso se emplean los criterios de informaci?n como AIC (Criterio de Informaci?n de Akaike), AICc o BIC (Criterio de Informaci?n Bayessiano.

# AIC = 2k - ln(L), donde k: n?mero de par?metros y ln(L): log Likelihood. 

# El likelihood se refiere al R2, es decir, al ajuste del modelo  el cual va a ir aumentando siempre en la medida que se incorporan m?s par?metros al modelo. De la forma en la que est? dispuesta la ecuaci?n AIC esta tiende a premiar el ajuste pero castigar conforme el n?mero de par?metros que se presente en el modelo. De esta manera, valores cada vez menores indican que el modelo presenta mayor parsimon?a y existe una combinaci?n m?s adecuada entre predicci?n y explicaci?n. Por s? solos estos criterios de informaci?n no indican mucho excepto de manera comparativa.

AIC(modelo)

# AICc = AIC + (2k^2 + 2k)/(n - k -1), donde n: numero de observaciones.

# El AICc (Criterio de Informaci?n de Akaike Corregido) permite corregir el AIC obtenido anteriormente por el n?mero de observaciones. Producto de que agrega un cuociente en donde k (n? par?metros) se encuentra al cuadrado, el AICc castiga mucho m?s severamente aquellos modelos que presentan un alto n?mero de par?metros en comparaci?n a AIC. Para hacer modelos m?s complejos necesito m?s puntos (observaciones) por lo que AICc castiga mucho menos a aquellos modelos que presenten una mayor cantidad de observaciones. Usualmente se recomienda emplear el AICc cuando el n?mero de observaciones es bajo. De todos modos, cuando el n?mero de observaciones es muy alto el cuociente tiende a 0 por lo que el IC estar?a conformado ?nicamente por el AIC. De esta manera, siempre es mejor emplear AICc que AIC.

MuMIn::AICc(modelo)

# A diferencia del R2, el cual sigue aumentando al ir incorporando m?s par?metros, el AIC y los dem?s IC tienden a experimentar un m?nimo muy marcado para luego ir incrementando su valor a medida que se incorporan variables. De esta manera, basar la selecci?n de modelo seg?n los criterios de informaci?n es mucho m?s robusto que hacerlo en base al R2.

# El BIC o Criterio de Informaci?n Bayessiano se emplea en experimentos de laboratorio muy controladas donde se tiene seguridad que las variables del estudio son las ?nicas que est?n afectando la variable dependiente. A diferencia del BIC, el AIC y AICc se emplean cuando las variables que se est?n evaluando no son las ?nicas existentes (se emplean m?s en los campos de ecolog?a y recursos naturales.)


# Ejercicio ---------------------------------------------------------------

# Tomando la base de datos mtcars explora la relaci?n entre AICc, R2 explicativo y R2 predictivo, para eso genera un df con las siguientes columnas


# Soluci?n 1 --------------------------------------------------------------

set.seed(2018)

index <- sample(x = 1:nrow(mtcars),
                size = round(nrow(mtcars) / 2))

train <- mtcars[index, ]
test <- mtcars[-index, ]

# Modelo 1
{
# Se genera un tibble vac?o que posteriormente se va a ir completando y llenando con los resultados obtenidos

df <- tibble(Formula = NA,
             Model = NA,
             K = NA,
             R2_Train = NA,
             R2_Test = NA, 
             AICc = NA)

# Como identificador vamos a ir agregando la ecuaci?n misma del modelo 

df$Formula <- "mpg ~ hp + wt"

# Tibble es un formato moderno de data frame que, a diferencia de este ?ltimo, te permite guardar en sus celdas un objeto de modelo

df$Model <- lm(formula = as.formula(df$Formula),
               data = train) %>% list()

# El tener acceso al modelo dentro del objeto data frame (como una lista) nos permite acceder f?cilmente a las m?tricas de inter?s

df$R2_Train <- df$Model[[1]] %>% 
  glance() %>% 
  pull(r.squared)

# El n?mero de par?metros (K) es, en este caso, igual a los degrees of freedom

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

# Uni?n de los tibble
df_t <- df %>% bind_rows(df2)


# Soluci?n 2 --------------------------------------------------------------

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

# Obtenemos las m?tricas mediante un loop

for (i in 1:nrow(df)) {
  df$Model[i] <- list(lm(as.formula(df$Formula[i]),
                         data = train))
  
  df$R2_Train[i] <- df$Model[i][[1]] %>% glance() %>% pull(r.squared)
  
  df$K[i] <- df$Model[i][[1]] %>% glance() %>% pull(df)
  
  df$AICc[i] <- df$Model[i][[1]] %>% AICc()
  
  df$R2_Test[i] <- postResample(pred = predict(df$Model[i][[1]], test),
                                obs = test$mpg)
}


# Soluci?n 3 --------------------------------------------------------------

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
