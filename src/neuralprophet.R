# prediccion de la demanda judicial con neuraprophet

# Instalando paquetes necesarios
#remotes::install_github("AlbertoAlmuinha/neuralprophet")
#install_nprophet()
# Activando librerias
pacman::p_load(neuralprophet, tidymodels, tidyverse, timetk, tibble)

# library example: working OK
# m <- m4_monthly %>% filter(id == "M750")
# splits <- initial_time_split(m)
# model_fit_nprophet <- neural_prophet(
#   freq            = "M",
#   growth          = "linear",
#   trend_reg       = 3,
#   learn_rate      = 0.1,
#   changepoint_range = 0.8,
#   seasonality_mode = "additive"
# ) %>%
#   set_engine("prophet") %>%
#   fit(value ~ ., training(splits))
# 
# # Forecast with 95% Confidence Interval
# modeltime_table(
#   model_fit_nprophet
# ) %>%
#   modeltime_calibrate(new_data = testing(splits)) %>%
#   modeltime_forecast(
#     new_data      = testing(splits),
#     actual_data   = m,
#     conf_interval = 0.95
#   ) %>%
#   plot_modeltime_forecast(.interactive = FALSE)

df = fread("data/movimientos.txt" )

df_presentanciones_xmes = df %>% 
  mutate(organo = str_c(circunscripcion, "-", organismo)) %>% 
  filter(!str_detect(circunscripcion, "Entre")) %>% 
  select(-organismo) %>% as_tibble() %>%
  mutate(fecha = lubridate::floor_date(fecha, unit = "month")) %>%
  group_by(fecha) %>% summarise(presentaciones_abogados = sum(presentaciones_abogados, na.rm = T)) %>% 
  na.omit() %>% as_tibble()

splits <- initial_time_split(df_presentanciones_xmes)

model_fit_nprophet <- neural_prophet(
  freq            = "M",
  growth          = "linear",
  trend_reg       = 3,
  learn_rate      = 0.1,
  changepoint_range = 0.8,
  seasonality_mode = "additive") %>%
  set_engine("prophet") %>%
  fit(presentaciones_abogados ~ ., training(splits))

# Forecast with 95% Confidence Interval
modeltime_table(
  model_fit_nprophet) %>%
  modeltime_calibrate(new_data = testing(splits)) %>%
  modeltime_forecast(
    new_data      = testing(splits),
    actual_data   = df_presentanciones_xmes,
    conf_interval = 0.95) %>%
  plot_modeltime_forecast(.interactive = FALSE)

