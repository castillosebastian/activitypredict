# primary
source("~/apgyeinformes/R/informe.R")
source("~/apgyeinformes/R/habiles.R")
source("~/apgyeinformes/R/all_results.R")
source("~/apgyeinformes/R/poblacion.R")
library(easystats)


start_date = "2020-07-01"
end_date = "2022-07-01"


# Actos procesales y presentaciones
movimientos_db <- tbl(DB_PROD(), "movimientos") %>% 
  filter(fecha_hora >= start_date, fecha_hora <= end_date) %>% 
  #filter(!(dia_s %in% c("sábado", "domingo"))) %>% 
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
  rename(organismo = organismo_descripcion) %>% 
  pivot_wider(id_cols = c("circunscripcion", "organismo", "fecha"), names_from = "tipo_movimiento", values_from = "cantidad")

# los movimientos en el caso del fuero penal están subestimados siguiendo los registros de Lex-doctor

# Iniciadas
inic_nop <- function(start_date, end_date) {
  
  inicxmes <- function(df, materia) {
    
    df %>% 
      mutate(materia = materia, 
             superfuero = 'Materias no penales')
  }
  #cco1--------
  resultado <- iniciados_cco(db_con = DB_PROD(), poblacion = jdos_cco, start_date = start_date,end_date = end_date, estadistico = "conteo")
  iniciados <- resultado$inic_cco_pc %>% inicxmes(materia = "Civil-Comercial")
  #fam---------
  resultado <- iniciados_fam(db_con = DB_PROD(), poblacion = jdos_fam,start_date = start_date, end_date = end_date, estadistico = "conteo") 
  inic_multif <- iniciados_multifuero_labfam(db_con = DB_PROD(), poblacion = jdos_cco, start_date = start_date, end_date = end_date, estadistico = "conteo") 
  pazfam <- iniciados_paz_fam(db_con = DB_PROD(), poblacion = jdos_paz, start_date = start_date, end_date = end_date, estadistico = "conteo") %>% .$inic_pazfam_pc
  if(!is.null(inic_multif$inic_multi_famlab_pc)){multifam <- inic_multif$inic_multi_famlab_pc %>% filter(fuero == "familia") %>% select(-fuero)} 
  if(exists("multifam")){resultado$inic_fam_pc <- resultado$inic_fam_pc %>% bind_rows(multifam)}
  if(exists("pazfam")){resultado$inic_fam_pc <- resultado$inic_fam_pc %>% bind_rows(pazfam)}
  iniciados <- iniciados %>% bind_rows(resultado$inic_fam_pc %>% inicxmes(materia = "Familia"))
  #ecq---------
  resultado <- iniciados_ecq(db_con = DB_PROD(), poblacion = jdos_ecq, start_date = start_date, end_date = end_date, estadistico = "conteo")
  if(!is.null(resultado$inic_ecq_pc)){iniciados <- iniciados %>% bind_rows( resultado$inic_ecq_pc %>% inicxmes(materia = "Quiebra_Ejecuciones"))}
  #lab-----------
  resultado <- iniciados_lab(db_con = DB_PROD(), poblacion = jdos_lab, start_date = start_date, end_date = end_date, estadistico = "conteo")
  res_multif <- iniciados_multifuero_labfam(db_con = DB_PROD(), poblacion = jdos_cco, start_date = start_date, end_date = end_date, estadistico = "conteo") 
  if(!is.null(res_multif$inic_ccolab_pc)){
    ccolabpc <- res_multif$inic_ccolab_pc %>% select(-fuero)
    multilab <- res_multif$inic_multi_famlab_pc %>% filter(fuero == "laboral") %>% select(-fuero)
  }
  if(exists("ccolabpc")){resultado$inic_lab_pc <- resultado$inic_lab_pc %>% bind_rows(ccolabpc)}
  if(exists("multilab")){resultado$inic_lab_pc <- resultado$inic_lab_pc %>% bind_rows(multilab)} 
  iniciados <- iniciados %>% bind_rows( resultado$inic_lab_pc %>% inicxmes(materia = "Laboral"))
  #paz1----------
  resultado_proc <- apgyeProcesamiento::iniciados_paz_procxgpo(db_con = DB_PROD(), poblacion = jdos_paz, start_date = start_date, end_date = end_date, estadistico = "conteo")
  iniciados <- iniciados %>% bind_rows( resultado_proc$inic_paz_pc %>% inicxmes(materia = "Paz 1°"))
  #paz23---------
  resultado <- iniciados_paz_23c(poblacion = jdos_paz_23,start_date = start_date,end_date = end_date, estadistico = "conteo") 
  if(!is.null(resultado$inic_paz23_pc)){iniciados <- iniciados %>% bind_rows( resultado$inic_paz23_pc %>% rename(finicio = fecha) %>% inicxmes(materia = "Paz 2°-3°"))}
  #resultados-------
  iniciados
} 
inic_nop_resultado <- inic_nop(start_date, end_date) %>% 
  group_by(circunscripcion, organismo, finicio) %>% 
  summarise(causas_iniciadas = n()) %>% 
  rename(fecha  = finicio)

# 
inic_p_resultado <- iniciadas_pen(db_con = DB_PROD(), poblacion = oga,
                                  start_date = start_date, end_date = end_date) %>% .$inic_gtia_pc %>% 
  mutate(organismo = "OGA") %>% 
  group_by(circunscripcion, organismo, finicio) %>% 
  summarise(causas_iniciadas = n()) %>% 
  rename(fecha  = finicio)
  
inic = inic_nop_resultado %>% bind_rows(inic_p_resultado) %>% ungroup()

movimientos <- movimientos_db %>% ungroup() %>% 
  left_join(inic, by= c("circunscripcion", "organismo", "fecha"))

describe_distribution(movimientos)

setwd("~/activitypredict/data")
write.table(movimientos, "movimientos.txt", col.names = T, row.names = F, sep = ",")

