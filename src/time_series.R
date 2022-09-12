source("~/apgyeinformes/R/all_results.R")

start_date = "2018-02-01"
end_date = "2022-08-01"



#https://blog.bguarisma.com/time-series-forecasting-lab-part-4-hyperparameter-tuning


pacman::p_load(tidyverse, lubridate, tsibble, tidymodels, 
               modeltime.ensemble, modeltime.resample, timetk, 
               fastDummies) # time series model packages

all_results <- tbl(DB_PROD(), "all_results") %>% 
  filter(tproducto %in% c("autos", "sentencias")) %>% 
  collect() %>% 
  filter(!is.na(año)) %>% 
  mutate(date = ymd(paste0(año,"-",mes,"-1"))) %>% 
  filter(date < ymd(end_date))

# all_results_penal_2018 <- all_results %>% 
#   filter(materia == "penal" & año == 2018) %>% 
#   group_by(date, tproducto) %>% 
#   summarise(value = sum(cantidad, na.rm = T)) %>% 
#   pivot_wider(names_from = tproducto, values_from = value)
# 
# año_2017 = fread("../Modelos_AdHoc/data/matriz_2017.csv") %>% 
#   mutate(date = ymd(paste0("2017","-",mes,"-1"))) %>% 
#   select(date, cra, crs) %>% as_tibble()
# 
# sentencias_2017 = año_2017 %>% 
#   group_by(date) %>% 
#   summarise(value = sum(crs, na.rm = T))


all_results_wide <- all_results %>%
  group_by(date, instancia, tproducto) %>%
  summarise(value = sum(cantidad, na.rm = T)) %>%
  pivot_wider(names_from = tproducto, values_from = value) %>% 
  filter(month(date) != 1) %>% 
  rowwise() %>% 
  mutate(value = sum(autos, sentencias))

# sentencias = all_results %>% 
#   filter(tproducto == "sentencias") %>% 
#   group_by(date) %>% 
#   summarise(value = sum(cantidad, na.rm = T))
#   
# autos = all_results %>% 
#   filter(tproducto == "autos") %>% 
#   group_by(date) %>% 
#   summarise(value = sum(cantidad, na.rm = T))

# interactive = F
# 
# 
# produccion_feria <- tbl(DB_PROD(), "movimientos") %>% 
#   filter(fecha_hora >= start_date, fecha_hora <= end_date) %>%  
#   mutate(mes = month(fecha_hora)) %>% 
#   filter(mes == 1) %>% 
#   filter( tipo_movimiento == "procesal" & 
#             stringr::str_detect(descripcion, "^SENTENCI|^AUTOS|^RESOL")) %>% 
#   # No se filtra por poblacion pues en se crean LEx-de feria ad-hoc que no corresponden a ningún órgano
#   # left_join(tbl(DB_PROD(), "lookupentidades") %>% rename(iep = organismo)) %>% collect()
#   # filter(iep %in% !!pob$organismo) %>% 
#   mutate(fecha = as.Date(fecha_hora)) %>% collect() %>% 
#   distinct() %>% 
#   mutate(date = lubridate::floor_date(fecha, "month")) %>% 
#   group_by(date) %>% 
#   summarise(value = n()) %>% 
#   collect() 


# all_results_wide <- all_results_wide %>% 
#   bind_rows(produccion_feria)


feria <- data_frame(
  holiday  = 'feria',
  ds = as.Date(c('2018-01-01', '2018-07-01',
                 '2019-01-01', '2019-07-01',
                 '2020-01-01', '2020-07-01',
                 '2021-01-01', '2021-07-01',
                 '2022-01-01', '2022-07-01', 
                 '2023-01-01', '2023-07-01', 
                 '2024-01-01', '2024-07-01', 
                 '2025-01-01', '2025-07-01', 
                 '2026-01-01', '2026-07-01', 
                 '2027-01-01', '2027-07-01')),
  lower_window = 0,
  upper_window = 30
)

# parte 1 introducción----
# https://blog.bguarisma.com/time-series-forecasting-lab-part-1-introduction-to-feature-engineering


produccion <- all_results_wide %>%
  tk_tbl() %>%
  mutate(Mes = date) %>%
  mutate(Instancia = as_factor(instancia)) %>%
  select(Mes, Instancia, Produccion = value)


# Summary Diagnostics
# Let us check the regularity of all time series with timetk::tk_summary_diagnostics()

produccion %>%
  group_by(Instancia) %>%
  tk_summary_diagnostics(.date_var = Mes)

# Other Diagnostics
produccion %>% 
  filter(Instancia == Instancia[1]) %>% 
  plot_seasonal_diagnostics(.date_var = Mes,
                            .value = Produccion,
                            .title = Instancia[1])

# End-to-end FE process with recipe
#https://blog.bguarisma.com/time-series-forecasting-lab-part-2-feature-engineering-with-recipes


groups <- lapply(X = 1:length(Industries), FUN = function(x){
  
  monthly_retail_tbl %>%
    filter(Industry == Industries[x]) %>%
    arrange(Month) %>%
    mutate(Turnover =  log1p(x = Turnover)) %>%
    mutate(Turnover =  standardize_vec(Turnover)) %>%
    future_frame(Month, .length_out = "12 months", .bind_data = TRUE) %>%
    mutate(Industry = Industries[x]) %>%
    tk_augment_fourier(.date_var = Month, .periods = 12, .K = 1) %>%
    tk_augment_lags(.value = Turnover, .lags = 12) %>%
    tk_augment_slidify(.value   = Turnover_lag12,
                       .f       = ~ mean(.x, na.rm = TRUE), 
                       .period  = c(3, 6, 9, 12),
                       .partial = TRUE,
                       .align   = "center")
})

groups_fe_tbl <- bind_rows(groups) %>%
  rowid_to_column(var = "rowid")
