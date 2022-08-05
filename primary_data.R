# primary
source("~/apgyeinformes/R/informe.R")
source("~/apgyeinformes/R/habiles.R")
source("~/apgyeinformes/R/all_results.R")
source("~/apgyeinformes/R/poblacion.R")

start_date = "2021-01-01"
end_date = "2022-07-30"

movimientos_db <- tbl(DB_PROD(), "movimientos") %>% 
  filter(fecha_hora >= start_date, fecha_hora <= end_date) %>% 
  filter(!(dia_s %in% c("sábado", "domingo"))) %>% 
  left_join(tbl(DB_PROD(), "lookupentidades") %>% rename(iep = organismo)) %>% 
  filter(iep %in% !!poblacion_er$organismo) %>% 
  mutate(fecha = as.Date(fecha_hora)) %>% 
  distinct() %>% 
  mutate(tipo_movimiento = case_when(
    tipo_movimiento == "procesal" | is.na(tipo_movimiento) ~ "actos_procesales",
    tipo_movimiento == "procesal_presentacion" ~"presentaciones_abogados")) %>% 
  group_by(iep, fecha, tipo_movimiento) %>% 
  summarise(cantidad = n()) %>% 
  collect() %>% 
  left_join(poblacion_er %>% select(organismo, organismo_descripcion, circunscripcion, tipo, materia), by = c("iep" = "organismo")) %>% 
  rename(organismo = organismo_descripcion) 

setwd("~/activitypredict/data")
write.table(movimientos_db, "movimientos.txt", col.names = T, row.names = F, sep = ",")

