# Packages ----------------------------------------------------------------

library(tidyverse)
library(MuMIn)
library(broom)
library(broom.mixed)
library(lme4) # Modelos Lineales Mixtos


# Modelos Lineales Mixtos: Variables fijas y aleatorias -------------------

# ?Qu? son las variables fijas y aleatorias? Son tipos de variables que se diferencian en cuanto al grado de conocimiento de su influencia sobre la variable de inter?s as? como por la cantidad de niveles que se integran al modelo.

# Las variables fijas (continuas o categoricas) presentan una influencia predecible y sistem?tica sobre la variable respuesta y dentro del modelo se integran la totalidad de niveles existentes del factor (variable) en cuesti?n. De esta manera, un modelo de efecto fijo est? compuesto por niveles de un factor en particular escogidos por el investigador.

# Por otro lado, las variables aleatorias presentan una influencia impredecible e ideosincr?tica adem?s de que no se incluye la totalidad de niveles que componen al factor dentro del modelo. Por lo mismo, en un modelo de efecto aleatorio existen niveles de los factores aleatorios que son incluidos al modelo de manera inconsciente por el investigador (todo lo que haya sido seleccionado al azar en una poblaci?n y que sea de inter?s para ser incluido en el modelo puede ser considerado una variable aleatoria). Entonces, una variable aleatoria es aquella que puede generar cierto grado de variabilidad pero que no se espera que tenga una gran poder explicativo.

# De esta manera, aquellos modelos que incluyan variables (factores) fijas y a la vez aleatorias son denominados modelos mixtos

# Tambien se puede extender a glm (Modelos Lineales Generalizados)

data("CO2")

attach(CO2)

theme_set(theme_light())

p <- ggplot(data = CO2, aes(x = conc, y = uptake, group = Plant, color = Type)) + 
  geom_line() +
  geom_point(aes(shape = Treatment))

# Observando el gr?fico pareciera que los puntos tienen un comportamiento de un m?ximo asint?tico pero para trabajaremos con un modelo logaritmico que es m?s sencillo. Vamos a generar un modelo lineal y un modelo lineal mixto para evaluar el efecto de este ?ltimo en predecir la variable resultante.

# Generaremos modelos que consideren la interacci?n entre las variables Treatment (nonchilled y chilled) con Type (Quebec y Mississippi). Esto es producto de que en el gr?fico podemos observar que el efecto de enfriado o no enfriado sobre la captura de carbono pareciera ser muy distinto de acuerdo a si se realiza en las plantas de Quebec o Mississippi (mientras que en el primero se observar una gran distinci?n en el segundo el efecto se ve m?s difuso).

# ?C?mo saber cuando hay una interacci?n? Hay un efecto diferente de una variable (Treatment) de acuerdo a otra (Type) en la modelaci?n de una variable respuesta. La interacci?n del tipo netamente Var1:Var2 significa que s?lo las pendientes ser?n distintas mientras que Var1 * Var2 (= Var 2 + Var1:Var2) significa que habr? una variaci?n tanto de la pendiente como de los interceptos.

# Al poner log(conc) + conc en el modelo estoy tomando en cuenta el comportamiento logaritmico de la variable as? como su comportamiento lineal.

# Modelo lineal

# mod1 <- lm(formula = uptake ~ Type * Treatment + I(log(conc)) + conc)
mod1 <- lm(formula = uptake ~ Type + Treatment + Type:Treatment + I(log(conc)) + conc)

# Modelo lineal mixto

# Tal como vidmos en el gr?fico la identidad de las plantas pareciera (se desconoce) ser un factor de variaci?n en el modelo (las l?neas no son iguales). Por esto, la variable Plant la podemos incorporar al modelo como una variable aleatoria ya que de por s? se espera que no explique mucho (comportamiento levemente diferente).

# En un modelo lineal mixto las variables fijas (las que se espera que tengan una incidencia predecible en la variable respuesta) se denotan de la misma manera que en los glm y lm.

# Las variables aleatorias se denotan de manera distinta de la forma (1 | Var.alea), lo cual significa que cada una de los niveles que componen el factor aleatorio presentan un intercepto relativamente distinto. En este caso, en la variable Plant se observa que existen observaciones repetidas de para las plantas, lo que significa que estas medidas no son independientes (aumentamos el n artificialmente).

mod2 <- lme4::lmer(formula = uptake ~ Type * Treatment + I(log(conc)) + conc + (1 | Plant))

# Los modelos lineales mixtos lmer, a diferencia de los lm y los glm, no presentan un R2. De todas manera, cuando los modelos son completamente distintos (uno un lm y el otro un glm o un lmer) estos no son comparables mediante las m?tricas de manera normal.

# Si observamos los par?metros estimados de ambos modelos nos damos cuenta de que estos son pr?cticamente iguales, simplemente hay unos nuevos par?metros en el lmer que hace referencia a una leve variaci?n en las predicciones de uptake de acuerdo a la Plant y por los errores, respectivamente.


# Selecci?n automatizada de modelos ---------------------------------------

# A partir del lmer confeccionado anteriormente (mod2) se realiza una selecci?n automatizada de modelos mediante el paquete MuMIn con la funci?n dredge(). Esta no es aplicable a algortimos de machine learning o super vector machine.

base::options(na.action = "na.fail")

# Normalmente se debe explicitar el m?ximo n?mero de variables explicativas para un modelo. Esta cantidad generalmente est? dada por la raz?n 10:1 entre el n?mero de observaciones (filas) y la cantidad de variables (columnas a aplicar en el modelo), respectivamente.

# floor() y round() son an?logos al redondeo (round()) hacia abajo y arriba, respectivamente.

Seleccion <- MuMIn::dredge(global.model = mod2,
                           # 0 a 8 variables en los modelos
                           m.lim = c(0, floor(nrow(CO2) / 10)))

BestModel <- MuMIn::get.models(object = Seleccion, subset = 1)[[1]]

BestModel %>% broom.mixed::tidy(conf.int = T, conf.level = 0.95)
BestModel %>% broom.mixed::glance()

BestModel.augment <- BestModel %>% broom.mixed::augment()

# La tabla augment de los modelos mixtos presentan valores predichos (.fitted) as? como tambien valores predichos sin efecto aleatorio (.fixed). A la hora de graficar se emplean los .fixed debido a que los .fitted presentan un comportamiento err?tico (efecto aleatorio)

ggplot(data = BestModel.augment, aes(x = conc, y = uptake, color = Type)) +
  geom_point(aes(shape = Treatment)) +
  geom_line(aes(y = .fixed, group = Treatment:Type))


# + control de dredge ----------------------------------------------------

Cement <- data("Cement")

Cement2<- Cement %>%
  as_tibble() %>% 
  rename(Calorias = y,
         CaAl = X1,
         SiCa3 = X2,
         Ca2AlFe = X3,
         Ca2Si = X4)

attach(Cement2)

# Generamos el modelo global

GlobalMod <- lm(Calorias ~., data = Cement2)

# La multicolinearidad se analiza a partir de las variables explicativas del modelo por lo que al calcular las correlaciones se?alamos que no queremos incluir en el an?lisis a la variable respuesta.

cor(Cement2[, -1], method = "pearson")

# Observamos que existen dos pares de variables que presentan un alto grado de correlaci?n. Para solucionar esto se le se?ala a dredge() que evite problemas de multicolinearidad al realizar la selecci?n de modelos.

# Para esto, se construye una matriz que indique aquellas variables que pueden o no estar incluidas al mismo tiempo en un modelo.

colnames(Cement2[,-1])

smat <- Cement2[, -1] %>% cor() %>% abs() <= 0.7

smat[!lower.tri(smat)] <- NA

# De esta manera, se se?ala que las variables TRUE s? pueden estar juntas mientras que las indicadas con FALSE no.

# Ahora realizamos la selecci?n de modelos considerando la multicolinearidad

options(na.action = "na.fail")

Selected <-
  dredge(global.model = GlobalMod,
         # Considerando la multicolinearidad
         subset = smat,
         # Considerando el m?ximo numero de variables por obs
         m.lim = c(0, floor(nrow(Cement2) / 10)))

# Con los resultados de la selecci?n la mejor opci?n es aceptar el segundo mejor modelo, no el primero ni realizar un promedio de ambos. El segundo modelo plantea que para aumentar el secado del cemento debemos incrementar la concentraci?n de la variable mientras que el primero plantea que para obtener el mismo resultado debemos disminuir la concentraci?n de la variable explicativa (es m?s simple realizar lo primero). Algunos par?metros que nos permiten realizar dicha elecci?n es el hecho de que los weight y delta son insignificantes entre estos dos modelos, es decir, las diferencias apreciables son m?nimas.

# Entonces, ?cuales son las etapas de la elaboraci?n de modelos? (1) Pensar y plantear el modelo de manera correcta (determinar si existen interacciones entre las variables explicativas). (2) Determinar el n?mero m?ximo de variables a incluir en el modelo de acuerdo a la cantidad de observaciones en los datos. (3) Estudiar y corregir la multicolinearidad presente en los modelos. (4) Determinar si es apropiado incluir una variable del tipo aleatoria para disminuir la variabilidad (trabajar con modelo mixto). (5) Seleccionar el modelo de acuerdo al inter?s del investigador.


# Modelo Lineal Mixto Generalizado ----------------------------------------

# Los modelos mixtos tambien pueden ser aplicados a modelos lineales generalizados (glme). En la base de datos ChickWeight se puede plantear que la variable Chick (pollo individual) es una variable aleatoria ya que se desconoce el efecto que pueda tener en la predicci?n que las mediciones se realicen en pollos particulares (se estima insignificante) adem?s de que el realizar mediciones a un mismo individuo (tal como est? designado) no cumple con el supuesto de independencia.

data("ChickWeight")

attach(ChickWeight)

ChickPoissMM1 <- lme4::glmer(formula = weight ~ Time + Diet + (1 | Chick), family = poisson)

ChickPoissMM2 <- lme4::glmer(formula = weight ~ Time + Time:Diet + (1 | Chick), family = poisson)

ChickPoissMM3 <- lme4::glmer(formula = weight ~ Time * Diet + (1 | Chick), family = poisson)





