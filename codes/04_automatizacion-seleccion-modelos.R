# Packages ----------------------------------------------------------------

library(tidyverse)
library(MuMIn)
library(broom)
library(broom.mixed)
library(lme4) # Modelos Lineales Mixtos


# Modelos Lineales Mixtos: Variables fijas y aleatorias -------------------

# ¿Qué son las variables fijas y aleatorias? Son tipos de variables que se diferencian en cuanto al grado de conocimiento de su influencia sobre la variable de interés así como por la cantidad de niveles que se integran al modelo.

# Las variables fijas (continuas o categoricas) presentan una influencia predecible y sistemática sobre la variable respuesta y dentro del modelo se integran la totalidad de niveles existentes del factor (variable) en cuestión. De esta manera, un modelo de efecto fijo está compuesto por niveles de un factor en particular escogidos por el investigador.

# Por otro lado, las variables aleatorias presentan una influencia impredecible e ideosincrática además de que no se incluye la totalidad de niveles que componen al factor dentro del modelo. Por lo mismo, en un modelo de efecto aleatorio existen niveles de los factores aleatorios que son incluidos al modelo de manera inconsciente por el investigador (todo lo que haya sido seleccionado al azar en una población y que sea de interés para ser incluido en el modelo puede ser considerado una variable aleatoria). Entonces, una variable aleatoria es aquella que puede generar cierto grado de variabilidad pero que no se espera que tenga una gran poder explicativo.

# De esta manera, aquellos modelos que incluyan variables (factores) fijas y a la vez aleatorias son denominados modelos mixtos

# Tambien se puede extender a glm (Modelos Lineales Generalizados)

data("CO2")

attach(CO2)

theme_set(theme_light())

p <- ggplot(data = CO2, aes(x = conc, y = uptake, group = Plant, color = Type)) + 
  geom_line() +
  geom_point(aes(shape = Treatment))

# Observando el gráfico pareciera que los puntos tienen un comportamiento de un máximo asintótico pero para trabajaremos con un modelo logaritmico que es más sencillo. Vamos a generar un modelo lineal y un modelo lineal mixto para evaluar el efecto de este último en predecir la variable resultante.

# Generaremos modelos que consideren la interacción entre las variables Treatment (nonchilled y chilled) con Type (Quebec y Mississippi). Esto es producto de que en el gráfico podemos observar que el efecto de enfriado o no enfriado sobre la captura de carbono pareciera ser muy distinto de acuerdo a si se realiza en las plantas de Quebec o Mississippi (mientras que en el primero se observar una gran distinción en el segundo el efecto se ve más difuso).

# ¿Cómo saber cuando hay una interacción? Hay un efecto diferente de una variable (Treatment) de acuerdo a otra (Type) en la modelación de una variable respuesta. La interacción del tipo netamente Var1:Var2 significa que sólo las pendientes serán distintas mientras que Var1 * Var2 (= Var 2 + Var1:Var2) significa que habrá una variación tanto de la pendiente como de los interceptos.

# Al poner log(conc) + conc en el modelo estoy tomando en cuenta el comportamiento logaritmico de la variable así como su comportamiento lineal.

# Modelo lineal

# mod1 <- lm(formula = uptake ~ Type * Treatment + I(log(conc)) + conc)
mod1 <- lm(formula = uptake ~ Type + Treatment + Type:Treatment + I(log(conc)) + conc)

# Modelo lineal mixto

# Tal como vidmos en el gráfico la identidad de las plantas pareciera (se desconoce) ser un factor de variación en el modelo (las líneas no son iguales). Por esto, la variable Plant la podemos incorporar al modelo como una variable aleatoria ya que de por sí se espera que no explique mucho (comportamiento levemente diferente).

# En un modelo lineal mixto las variables fijas (las que se espera que tengan una incidencia predecible en la variable respuesta) se denotan de la misma manera que en los glm y lm.

# Las variables aleatorias se denotan de manera distinta de la forma (1 | Var.alea), lo cual significa que cada una de los niveles que componen el factor aleatorio presentan un intercepto relativamente distinto. En este caso, en la variable Plant se observa que existen observaciones repetidas de para las plantas, lo que significa que estas medidas no son independientes (aumentamos el n artificialmente).

mod2 <- lme4::lmer(formula = uptake ~ Type * Treatment + I(log(conc)) + conc + (1 | Plant))

# Los modelos lineales mixtos lmer, a diferencia de los lm y los glm, no presentan un R2. De todas manera, cuando los modelos son completamente distintos (uno un lm y el otro un glm o un lmer) estos no son comparables mediante las métricas de manera normal.

# Si observamos los parámetros estimados de ambos modelos nos damos cuenta de que estos son prácticamente iguales, simplemente hay unos nuevos parámetros en el lmer que hace referencia a una leve variación en las predicciones de uptake de acuerdo a la Plant y por los errores, respectivamente.


# Selección automatizada de modelos ---------------------------------------

# A partir del lmer confeccionado anteriormente (mod2) se realiza una selección automatizada de modelos mediante el paquete MuMIn con la función dredge(). Esta no es aplicable a algortimos de machine learning o super vector machine.

base::options(na.action = "na.fail")

# Normalmente se debe explicitar el máximo número de variables explicativas para un modelo. Esta cantidad generalmente está dada por la razón 10:1 entre el número de observaciones (filas) y la cantidad de variables (columnas a aplicar en el modelo), respectivamente.

# floor() y round() son análogos al redondeo (round()) hacia abajo y arriba, respectivamente.

Seleccion <- MuMIn::dredge(global.model = mod2,
                           # 0 a 8 variables en los modelos
                           m.lim = c(0, floor(nrow(CO2) / 10)))

BestModel <- MuMIn::get.models(object = Seleccion, subset = 1)[[1]]

BestModel %>% broom.mixed::tidy(conf.int = T, conf.level = 0.95)
BestModel %>% broom.mixed::glance()

BestModel.augment <- BestModel %>% broom.mixed::augment()

# La tabla augment de los modelos mixtos presentan valores predichos (.fitted) así como tambien valores predichos sin efecto aleatorio (.fixed). A la hora de graficar se emplean los .fixed debido a que los .fitted presentan un comportamiento errático (efecto aleatorio)

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

# La multicolinearidad se analiza a partir de las variables explicativas del modelo por lo que al calcular las correlaciones señalamos que no queremos incluir en el análisis a la variable respuesta.

cor(Cement2[, -1], method = "pearson")

# Observamos que existen dos pares de variables que presentan un alto grado de correlación. Para solucionar esto se le señala a dredge() que evite problemas de multicolinearidad al realizar la selección de modelos.

# Para esto, se construye una matriz que indique aquellas variables que pueden o no estar incluidas al mismo tiempo en un modelo.

colnames(Cement2[,-1])

smat <- Cement2[, -1] %>% cor() %>% abs() <= 0.7

smat[!lower.tri(smat)] <- NA

# De esta manera, se señala que las variables TRUE sí pueden estar juntas mientras que las indicadas con FALSE no.

# Ahora realizamos la selección de modelos considerando la multicolinearidad

options(na.action = "na.fail")

Selected <-
  dredge(global.model = GlobalMod,
         # Considerando la multicolinearidad
         subset = smat,
         # Considerando el máximo numero de variables por obs
         m.lim = c(0, floor(nrow(Cement2) / 10)))

# Con los resultados de la selección la mejor opción es aceptar el segundo mejor modelo, no el primero ni realizar un promedio de ambos. El segundo modelo plantea que para aumentar el secado del cemento debemos incrementar la concentración de la variable mientras que el primero plantea que para obtener el mismo resultado debemos disminuir la concentración de la variable explicativa (es más simple realizar lo primero). Algunos parámetros que nos permiten realizar dicha elección es el hecho de que los weight y delta son insignificantes entre estos dos modelos, es decir, las diferencias apreciables son mínimas.

# Entonces, ¿cuales son las etapas de la elaboración de modelos? (1) Pensar y plantear el modelo de manera correcta (determinar si existen interacciones entre las variables explicativas). (2) Determinar el número máximo de variables a incluir en el modelo de acuerdo a la cantidad de observaciones en los datos. (3) Estudiar y corregir la multicolinearidad presente en los modelos. (4) Determinar si es apropiado incluir una variable del tipo aleatoria para disminuir la variabilidad (trabajar con modelo mixto). (5) Seleccionar el modelo de acuerdo al interés del investigador.


# Modelo Lineal Mixto Generalizado ----------------------------------------

# Los modelos mixtos tambien pueden ser aplicados a modelos lineales generalizados (glme). En la base de datos ChickWeight se puede plantear que la variable Chick (pollo individual) es una variable aleatoria ya que se desconoce el efecto que pueda tener en la predicción que las mediciones se realicen en pollos particulares (se estima insignificante) además de que el realizar mediciones a un mismo individuo (tal como está designado) no cumple con el supuesto de independencia.

data("ChickWeight")

attach(ChickWeight)

ChickPoissMM1 <- lme4::glmer(formula = weight ~ Time + Diet + (1 | Chick), family = poisson)

ChickPoissMM2 <- lme4::glmer(formula = weight ~ Time + Time:Diet + (1 | Chick), family = poisson)

ChickPoissMM3 <- lme4::glmer(formula = weight ~ Time * Diet + (1 | Chick), family = poisson)





