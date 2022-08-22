# Packages ----------------------------------------------------------------

library(tidyverse)
library(caret)
library(lubridate)
library(broom)
library(broom.mixed)
library(furrr)
library(doParallel)
library(tictoc)


# Data --------------------------------------------------------------------

# Import

unzip("covid19chile.zip")
{
  map(.x = list("Test.csv", "Train.csv"),
      .f = ~ read_csv(.x)) %>% map2(
        .y = list("Test", "Train"),
        .f = ~ assign(x = .y,
                      value = .x,
                      envir = .GlobalEnv)
      )
}

# Add variables and sort by Date

{
  map(
    .x = list(Train, Test),
    .f = ~ mutate(
      .data = .x,
      Dia = day(Fecha),
      Semana = week(Fecha),
      Mes = month(Fecha),
      delta_pob = Poblacion_region - Poblacion
    )
  ) %>% map(.f = ~ arrange(.data = .x,
                           Fecha)) %>% walk2(
                             .y = list("Train_plus", "Test_plus"),
                             .f = ~ assign(x = .y,
                                           value = .x,
                                           envir = .GlobalEnv)
                           )
}

attach(Train_plus)

# Graphic

theme_set(theme_bw())

ggplot(data = Train_plus, aes(x = Fecha, y = Activos_5_por_100.000)) + 
  geom_path(aes(color = Region))

# Time cycle? 346 observations

Train %>% count(Fecha)

# Temporal Model

options(na.action = "na.fail")

set.seed(1)

Slices <- createTimeSlices(
  y = Train$Activos_5_por_100.000,
  horizon = 346,
  initialWindow = 346 * 3,
  fixedWindow = T,
  skip = 346
)

fitControl <- trainControl(
  method = "timeslice",
  horizon = 346,
  initialWindow = 346 * 3,
  fixedWindow = T,
  skip = 346
)

fitTime <- train(
  form = Activos_5_por_100.000 ~ .,
  data = Train,
  method = "gbm",
  trControl = fitControl,
  metric = "RMSE",
  verbose = T
)

# Predictive power

pred <- predict(object = fitTime,
               newdata = Test_plus)

# Export

Pred <- tibble(ID = seq_along(pred),
               Activos_5_por_100.000 = pred)

write_csv(x = Pred, file = "Prediccion.csv")


# Functional programming --------------------------------------------------

# Nesting models

{
  Train_nest <- Train_plus %>%
    nest(data = everything()) %>%
    # duplicate rows by weight
    uncount(weights = 2) %>%
    mutate(fixedWindow = c(T, F)) %>%
    uncount(weights = 4, .id = "ID") %>%
    group_by(fixedWindow) %>%
    mutate(
      initialWindow = map2(
        .x = c(2, 3, 4, 5),
        .y = rep(x = 346, times = 4),
        .f = ~ .x * .y
      ) %>% reduce(c),
      horizon = map2(
        .x = c(1, 1, 2, 5),
        .y = rep(x = 346, times = 4),
        .f = .Primitive("*")
      ) %>% reduce(c)
    ) %>%
    group_by(fixedWindow, ID) %>%
    relocate(data, .after = horizon)
}

# Seting up parallel processing

future::plan(strategy = "multisession", workers = 4)

parallel::makePSOCKcluster(4) %>% doParallel::registerDoParallel()

# Nesting modelling

options(na.action = "na.fail")

set.seed(1)

tic()
{
  Train_model <- Train_nest %>%
    mutate(
      timeSlices = future_pmap(
        .l = list(fixedWindow, initialWindow, horizon),
        .f = ~ createTimeSlices(
          y = data[[1]]$Activos_5_por_100.000,
          horizon = horizon,
          initialWindow = initialWindow,
          fixedWindow = fixedWindow,
          skip = 346
        ),
        .options = furrr_options(seed = list(1:8))
      ),
      fitControl = future_pmap(
        .l = list(fixedWindow, initialWindow, horizon),
        .f = ~ trainControl(
          method = "timeslice",
          horizon = horizon,
          initialWindow = initialWindow,
          fixedWindow = fixedWindow,
          skip = 346
        ),
        .options = furrr_options(seed = list(1:8))
      ),
      fitTime = future_map(
        .x = fitControl,
        .f = ~ train(
          form = Activos_5_por_100.000 ~ .,
          data = Train_plus,
          method = "gbm",
          trControl = .x,
          metric = "RMSE",
          verbose = T
        ),
        .options = furrr_options(seed = list(1:8))
      )
    )
}
toc()

future::plan(strategy = sequential)

parallel::makePSOCKcluster(4) %>% parallel::stopCluster()

# Predictions

Test_pred <- Train_model %>% 
  mutate(Pred = map(
    .x = fitTime,
    .f = ~ predict(
      object = .x,
      newdata = Test_plus
    )
  )) %>% 
  select(-c(data, timeSlices, fitControl,fitTime)) %>% 
  unnest(cols = Pred)
