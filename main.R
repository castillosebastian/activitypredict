setwd("~/R/activitypredict")


pacman::p_load(tidyverse, magrittr) # data wrangling packages
pacman::p_load(lubridate, tsintermittent, fpp3, modeltime, timetk, modeltime.gluonts, tidymodels, modeltime.ensemble, modeltime.resample) # time series model packages
pacman::p_load(foreach, future) # parallel functions
pacman::p_load(viridis, plotly) # visualizations packages
theme_set(hrbrthemes::theme_ipsum()) # set default themes
pacman::p_load(data.table)

df = fread("movimientosxdptoyloc.txt" )

df <- df %>% 
  group_by(codigo_organismo, tipo_movimiento, fecha) %>% 
  summarise(cantidad = n()) %>% 
  ungroup() %>% 
  mutate(codigo_organismo = factor(codigo_organismo),
         tipo_movimiento = factor(tipo_movimiento)) %>% 
  filter(tipo_movimiento == "procesal_presentacion")
  
skimr::skim(df)


#The Time Series mov x org
muestra = df %>% distinct(codigo_organismo) %>% sample_n(10)
df %>%
  filter(codigo_organismo %in% muestra$codigo_organismo) %>% 
  as_tsibble(key = codigo_organismo, index = fecha) %>%
  #fill_gaps(cantidad = 0, .full = end()) %>%
  autoplot(cantidad) +
  scale_color_viridis(discrete = T)

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




