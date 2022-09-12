rm( list=ls() )  #remove all objects
gc()             #garbage collection

pacman::p_load(tidyverse, magrittr, tibble, ggplot2, ggfortify, forecast, data.table, dplyr, dtplyr) # data wrangling packages
pacman::p_load(lubridate,xgboost, timetk, modeltime, fpp3, tsibble, tidymodels, modeltime.gluonts, modeltime.ensemble, modeltime.resample) # time series model packages
pacman::p_load(foreach, future) # parallel functions
pacman::p_load(viridis, plotly) # visualizations packages


# remotes::install_github("AlbertoAlmuinha/neuralprophet")

df = fread("data/movimientos.txt" )


df = df %>% 
  mutate(organo = str_c(circunscripcion, "-", organismo)) %>% 
  filter(!str_detect(circunscripcion, "Entre")) %>% 
  select(-organismo)

# si se eligiera hacer predicción de las presentaciones por tipo de proceso se vería
# que los distintos tipos tienen frecuencia variable en materia de transacciones
# Muchos procesos tienen presentaciones esporàdicas o otros
# tiene presentaciones frecuentes. Los que tienen patrones esporádicos (salvo casos de regularidad)
# sin dificil de predecir y por tanto es dificil bajar el error en estas predicciones.
# Su predictibilidad es baja. Cómo pueden tratarse. Se pueden computar su frecuencia es períodos largos
# y luego distribuir en el período bajo estudio. (una forma de tratamietno rápida, ver). De todas
# formas estos procesos esporàdicos, dado que no implican gran volumen de transacciones, no generan
# grandes variaciones en el servicio.
# Cuando se trabaje proyeccion por tipo de proceso ver:
# https://towardsdatascience.com/multiple-time-series-forecast-demand-pattern-classification-using-r-part-1-31601158d33b
# Demand Categorization — SBC Method.





