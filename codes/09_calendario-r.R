# Packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)


# Calendario --------------------------------------------------------------

info <- tibble(Fecha = c("20/05/02", "20/05/05", "20/05/07", "20/05/12", "20/05/14", "20/05/19", "20/05/21", "20/05/26"),
               Horario = c("18:00", rep("20:30", 7))) %>% 
  mutate(Fecha = lubridate::ymd(Fecha),
         Horario = lubridate::hm(Horario),
         Dia = lubridate::wday(Fecha, 
                               # Para que el dia de semana empiece el lunes (1), y no el domingo (7)
                               week_start = getOption("lubridate.week.start", 1),
                               label = T, 
                               abbr = T),
         Date = day(Fecha));info

info2 <- tibble(Fecha = seq.Date(from = dmy("1/5/2020"), to = dmy("30/5/2020"), by = 1)) %>% 
  mutate(Dia = lubridate::wday(Fecha, 
                               week_start = getOption("lubridate.week.start", 1),
                               label = T),
         Date = lubridate::day(Fecha),
         Semana = lubridate::isoweek(Fecha));info2

df <- info %>% 
  full_join(info2) %>% 
  mutate(Horario = as.character(Horario),
         Mes = "Mayo")

theme_set(new = theme_bw())

ggplot(data = df, aes(x = Dia, y = Semana)) +
  geom_tile(aes(fill = Horario)) + 
  geom_text(aes(label = Date)) +
  # Invertir la escala del eje y
  scale_y_reverse() +
  facet_grid(~Mes) +
  labs(x = "",
       y = "")
