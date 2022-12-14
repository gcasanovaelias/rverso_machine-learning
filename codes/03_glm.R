# Packages ----------------------------------------------------------------

library(MuMIn)
library(broom)
library(tidyverse)

library(renv)


# Modelos lineales con interacciones y modificadores ----------------------------

# Trabajaremos con variables predictoras del tipo cuantitativo y cualitativo donde se tendr? en consideraci?n la interacci?n que pueda existir entre estas para explicar la variabilidad de una variable de respuesta cuantitativa

data("ChickWeight")

# ?Es la dieta (variable cualitativa) importante para determinar el peso final del pollo?

# Ajuste del primer modelo (similar a un modelo de clasificaci?n). Si construimos la formula de la manera m?s simple posible podemos observar que el planteamiento del modelo es err?neo. Hay diferencias entre las dietas pero observando la gr?fica pareciera que cada una de las dietas plantea una pendiente paralela (igual incremento) pero para distintos valores iniciales (lo que no es as? ya que todos los pollos nacen relativamente con el mismo peso)

# Primera aproximaci?n

attach(ChickWeight)

theme_set(theme_bw())

p <- ggplot(data = ChickWeight, aes(x = Time, y = weight))

p + geom_point(aes(color = Diet)) +
  geom_line(aes(color = Diet,
                # Agrupar las l?neas por la columna Chick (pollo individual)
                group = Chick))

# Modelamiento

fit1 <- lm(formula = weight ~ Time + Diet)

fit1_augment <- augment(fit1)

# Grafico lm 1 (versi?n original con geom_line)

p + geom_line(data = fit1_augment, aes(y = .fitted, color = Diet))

# Grafico lm 1 (versi?n nueva con stat smooth)
{
  ggplot(data = fit1_augment, aes(x = Time, y = weight)) +
    geom_point(aes(color = Diet)) +
    stat_smooth(
      method = "lm",
      formula = y ~ x,
      se = F,
      mapping = aes(y = .fitted, color = Diet),
      method.args = list()
    )
}

# En un modelo de clasificaci?n estamos estimando las medias para cada tratamiento lo que, en este caso, se traduce a estimar el peso de los pollos seg?n cada una de las dietas (an?logo a realizar un ANOVA). Para esto se emplea el valor del intercepto general b0 y de los modificadores de cada una de las dietas. Para la dieta 1, la cual es el nivel de la variable que ha sido designada como base, el estimado del modificador es igual a 0 mientras que el resto de los modificadores para el resto de las dietas corresponden a una comparaci?n del peso promedio estimado para cada una de estas. Cada uno de estos estimadores se suma a la media general estimada (intercepto) por lo que las medias para cada una de las dietas ser?a; 10.9 + 0 = 10.9 para la dieta 1, 10.9 + 16.2 = 28.1 para la dieta 2, 10.9 + 36.5 = 47.4 para la dieta 3, y 10.9 + 30.2 = 41.1 para la dieta 4. 

# De igual modo, este modelo tambien presenta una estimaci?n del par?metro de pendiente para la variable tiempo por lo que en este modelamiento vamos a poder ingresar el tipo de dieta y el tiempo que ha trasncurrido para estimar el peso de los pollos. El valor de 8.75 se traduce a que por cada d?a que pase los pollos aumentar?n de peso en 8.75 gramos en promedio lo cual es aplicable por igual a TODAS LAS DIETAS. De este modo, de la manera en la que est? planteado la regresi?n, las dietas presentan diferencias en las medias estimadas (valores iniciales en el tiempo 0) pero tienen la misma tasa de incremento con respecto al tiempo. Lo que nosotros queremos (para la pregunta de investigaci?n planteada) es un mismo intercepto pero distintas pendientes seg?n la dieta.

# Al incluirse como variable, la dieta provoca que el peso promedio inicial sea diferente para las dietas, lo cual es err?neo.

# ?C?mo se soluciona esto? Mediante interacciones. Se plantea que las variables explicativas del modelo van a ser el tiempo y la interacci?n entre el tiempo y la dieta. De esta manera, el modelo plantea que el tiempo va a tener un efecto en el peso pero que tambien el efecto que este tenga va a variar para cada dieta. De esta manera, la dieta no se incluye como una variable por s? misma en el modelo sino que est? supeditada al tiempo como interacci?n (provoca un efecto distinto en una variable explicativa) por lo que no habr? una distinci?n en el peso inicial promedio para cada dieta (intercepto no cambia porque no se incluyen estos modificadores).

fit2 <- lm(formula = weight ~ Time + Time:Diet)

fit2_augment <- augment(fit2)

# Gr?fico lm 2 (versi?n original)

p + geom_line(data = fit2_augment, aes(y = .fitted, color = Diet))

# Gr?fico lm 2 (versi?n nueva con stat smooth)
{
  ggplot(data = fit2_augment, aes(x = Time, y = weight)) +
    geom_point(aes(color = Diet)) +
    stat_smooth(
      method = "lm",
      formula = y ~ x,
      se = F,
      mapping = aes(y = .fitted, color = Diet),
      method.args = list()
    )
}   
# Observando los par?metros, el modelo presenta un ?nico intercepto para todas las dietas, es decir, el valor estimado del peso para el tiempo 0 es el mismo para todas las dietas. Aqu? la pendiente entre el weight y Time es la que var?a y esto es proudcto de Time:Diet. Para la dieta 1 el incremento es igual a la pendiente de Time (7.05 + 0 = 7.05), para la dieta 2 (7.05 + 1.61 = 8.66), dieta 3 (7.05 + 3.74 = 10.79) y dieta 4 (7.05 + 2.86 = 9.91).

# Podemos observar que el criterio de informaci?n de Akaike indica que este ?ltimo modelo es m?s parsimonioso que el anterior. Independiente de esto, podemos mejorar a?n m?s el modelo indicandole cambios en la pendiente as? como tambien en el intercepto.

# fit3 <- lm(formula = weight ~ Time * Diet)
fit3 <- lm(formula = weight ~ Time + Diet + Time:Diet)

fit3_augment <- augment(fit3)

# Gr?fico lm 3 (versi?n original con geom_line)

p + geom_line(data = fit3_augment, aes(y = .fitted, color = Diet))

# Grafico lm3 (versi?n nueva con stat_smooth)
{
  ggplot(data = fit3_augment, aes(x = Time, y = weight)) +
    geom_point(aes(color = Diet)) +
    stat_smooth(
      method = "lm",
      formula = y ~ x,
      se = F,
      mapping = aes(y = .fitted, color = Diet),
      method.args = list()
    )
}

# Ahora cada una de las dietas se considera como una variable por separado, es decir, que el valor promedio inicial de cada una ser? incorporado al modelo. En el casd anterior la dieta solamente era incluida para diferenciar el efecto del tiempo, al no incluirse como variable por s? misma el valor inicial de peso era el mismo para todos los pollos, independiente de la dieta.

# De esta manera, cada dieta va a tener un intercepto y una pendiente distinta. Pero, al observar las estimaciones de los par?metros nos damos cuenta de que los modificadores del intercepto para cada dieta no presentan significancia lo que estar?a apuntando a la idea de que diferenciar interceptos por dieta no estar?a aportando al modelo. Del mismo modo, al comparar el AIC entre estos dos modelos nos damos cuenta de que el modelo 3 (distintos interceptos y pendientes) presenta un AIC mayor que el modelo 2 (distintas pendientes pero igual interceptos) por lo que este ?ltimo es el modelo m?s passimonioso. Esto significa que el agregar estos estimados de interceptos distintos no aporta un mejor ajuste m?s en lo que aporta en complejidad.

# En cuanto a los par?metros. El intercepto para la dieta 1 es (30.93 + 0 = 30.93), dieta 2 (30.93 - 2.30 = 28.63), dieta 3 (30.93 - 12.68 = 18.25) y dieta 4 (30.93 - 0.14 = 30.79). Para la pendiente, la dieta 1 (6.84 + 0 = 6.84), para la dieta 2 (6.84 + 1.76 = 8.60), dieta 3 (6.84 + 4.58 = 11.42) y dieta 4 (6.84 + 2.87 = 9.71).



# Seleccion de modelos ----------------------------------------------------


Select <- MuMIn::model.sel(object = list(fit1, fit2, fit3)) %>% as_tibble()

# El segundo modelo pareciera ser el m?s parsimonioso. Lo que s?, al observar el gr?fico de aproximaci?n es que podemos notar una gran dispersi?n de datos en los ?ltimos d?as del estudio y el modelo debiese incorporar esta variabilidad. Para esto analizaremos los residuos del modelo.

# Una etapa importante de la evaluaci?n de los modelos es la comprobaci?n de los supuestos para lo cual los residuos de los valores predichos y observados son fundamentales. (1) Que los residuos presenten una distribuci?n normal y (2) homogeneidad de la varianza (residuos vs predichos)

# Normalidad de los residuos

Test <- augment(fit2)
hist(Test$.resid)

qplot(sample = weight, data = Test) + stat_qq_line()

qqnorm(Test$.resid)

# Homogeneidad de la varianza (varianza constante)

ggplot(data = Test, aes(x = .fitted, y = .resid)) +
  geom_point() +
  # Adherir una l?nea horizontal de referencia
  geom_hline(yintercept = 0,
             # l?nea punteada
             lty = 2, 
             color = "red")

# Cuando no se cumple alguno de estos supuestos de modelos lineales es que podemos emplear los llamados modelos lineales generalizados "glm" los cuales nos permiten incorporar otras estructuras de errores en el modelo.


# Modelos Lineales Generalizados ------------------------------------------

# Una vez que el modelo lineal no ha desempe?ado correctamente o no cumple los supuestos es que el siguiente paso es intentar construir un modelo lineal generalizado.

# Para entender los glm hay que entender distribuciones ya que en estos modelos la variable respuesta puede presentar distintas estructuras (distribuciones gaussianas, binomiales, poisson, gamma, etc)

# Al estudiar los datos salta a la vista de que la variable weight s?lo presenta valores positivos por lo que su distribuci?n m?s apropiada podr?a ser un gamma (cuantitativo continua solo positiva) y no gaussiana (cuantitativo continua). De la misma manera, aunque el peso es cuantitativo continuo en los datos de weight la variable s?lo aparece con valores cuantitativos discretos por lo que tambien podr?amos probar con la distribuci?n de poisson (cuantitativos discretos)

hist(weight)

# Cuando nosotros escogemos una familia (distribuci?n) la variable respuesta Y es transformada mediante una funci?n enlace (link). 

ggplot(data = ChickWeight, aes(x = weight, y = 1/weight)) + 
  geom_point(color = "red")

# En el caso de la familia gamma la funci?n enlace (link) es el inverso de manera que ya no estaremos prediciendo el valor de Y sino el de 1/Y. Esta transformaci?n genera modificaciones a los datos, al aplicar el inverso aquellos pesos altos se transformar?n a bajos y viceversa. Junto con esto, se puede apreciar de que se generan 2 as?ntotas, cada una en un eke cartesiano.

# GLM CON FAMILIA GAMMA

fit2g <- glm(formula = weight ~ Time + Time:Diet, family = Gamma)

#Como se puede apreciar, el modelo ahora incorpora una curvatura en vez de ser una l?nea recta lo que es producto de cambiar la distribuci?n de la variable respuesta. De esta manera, al aplicar la familia Gamma se obtiene una respuesta similar a una exponencial. Esta respueta tiene sentido desde el punto de vista biologico ya que los infantes no presentan un crecimiento rectilineal sino que van duplicando su tasa de crecimiento conforme avanza el tiempo dentro de un periodo.

# df$Pred2g <- 1/predict(object = fit2g, newdata = df)
# df$Pred2g <- predict(object = fit2g, newdata = df, type = "response")

fit2g_augment <- augment(x = fit2g, type.predict = "response")

# Los valores predichos del modelo son el inverso del weight.

# Gr?fico 1 glm (Gamma) (versi?n original c/ geom_line)

p + geom_line(data = fit2g_augment, aes(y = .fitted, color = Diet))

# Gr?fico 1 glm (Gamma) (nueva versi?n c/ stat_smooth)
{
  ggplot(data = fit2g_augment, aes(x = Time, y = weight)) +
    geom_point(aes(color = Diet)) +
    stat_smooth(
      method = "glm",
      formula = y ~ x,
      se = F,
      mapping = aes(y = .fitted, color = Diet),
      method.args = list(family = Gamma)
    )
}  
# GLM CON FAMILIA POISSON

# En la distribuci?n de poisson el link que conecta la relaci?n lineal y la variable respuesta es un logaritmo por lo que los valores predichos del modelo corresponden a log(Y). Producto de este log es que poisson s?lo debe ser empleado cuando la variable Y es cuentitativa discreta y no contempla el 0. 

ggplot(data = ChickWeight, aes(x = weight, y = log(weight))) + 
  geom_point(color = "red")

# De esta manera, cuando aplicamos una familia poisson para explicar la variable respuesta es que debemos aplicar una exponencial para despejar Y.

fit2p <- glm(formula = weight ~ Time + Time:Diet, family = poisson)

# Podemos observar que al aplicar la familia poisson los datos presentan un comportamiento exponencial (se aplica esta realci?n para transformar el logaritmo que se encontraba como link). En comparaci?n al glm de gamma este modelo presenta un crecimiento menos acelerado (en general gamma presenta una tasa de crecimiento mucho m?s acelarada que poisson).

# Al momento de interpretar los resultados debemos hacerlo teniendo en cuenta el coportamiento de la variable dependiente original (weight) y no la transformada en un principio (log(weight)). Para eso podemos basarnos en un gr?fico y basar el an?lisis en el comportamiento exponencial que se puede apreciar.

# df$Pred2p <- predict(object = fit2p, newdata = df) %>% exp()
# df$Pred2p <- predict(object = fit2p, newdata = df, type = "response")

fit2p_augment <- augment(fit2p, type.predict = "response")

# Gr?fico 2 glm (Poisson) (versi?n original con geom_line)

p + geom_line(data = fit2p_augment, aes(y = .fitted, color = Diet))

# Gr?fico 2 glm (Poisson) (nueva versi?n c/ stat_smooth)

{
  ggplot(data = fit2p_augment, aes(x = Time, y = weight)) +
    geom_point(aes(color = Diet)) +
    stat_smooth(
      method = "glm",
      formula = y ~ x,
      se = F,
      mapping = aes(y = .fitted, color = Diet),
      method.args = list(family = poisson)
    )
  
}

# Comparaci?n de modelos lm y glm. Cuando se realiza una transformaci?n de la variable Y ya no podemos realizar una comparaci?n de criterios de informaci?n como AICc o BIC.

Select <- Select %>% dplyr::full_join(MuMIn::model.sel(list(fit2g, fit2p)))


# GLM CON FAMILIA BINOMIAL

train <- read_csv("https://raw.githubusercontent.com/derek-corcoran-barrios/derek-corcoran-barrios.github.io/master/CursoMultiPres/Capitulo_3/train.csv") %>% 
  filter(Embarked == "S")

dplyr::glimpse(train)

# Modelaremos la sobrevivencia de los pasajeros por lo que la variable respuesta presentar? una distribuci?n binomial. Esta distribuci?n tiene un link igual a logit = log((p)/(1-p)), el cual genera una l?nea en forma de S. Dada esta transformaci?n, logit s?lo puede ser empleado a valores que se encuentren entre 0 y 1, sin contarlos.

detach(ChickWeight)

attach(train)

# Si construimos el modelo como un lm habitual (familia gaussiana) se observa que algunos valores pueden superar el 100% de probabilidades de sobrevivir, lo cual es err?neo.

fitBin2 <- glm(formula = Survived ~ Fare * Sex, family = binomial)

train_augment <- augment(fitBin2, type.predict = "response")

# El modelo binomial produce que en los valores extremos de Fare se generen convergencias hacia los valores de y = 0 e y = 1.

{
  ggplot(data = train_augment, aes(x = Fare, y = Survived)) +
    geom_point(aes(color = Sex)) +
    stat_smooth(
      method = "glm",
      formula = y ~ x,
      se = F,
      mapping = aes(y = .fitted, color = Sex),
      method.args = list(family = binomial)
    )
  
}

