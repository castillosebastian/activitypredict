<img src="data/presentacionesdigitales.png" width="100%" style="display: block; margin: auto;" />

# Introducción

La actividad de un órgano judicial está vinculada al volumen de causas o
procesos radicados en él. Este volumen de causas o procesos implica, en
la práctica, un volumen aún mayor de transacciones asociadas a las
peticiones que realizan las partes y las respuestas generadas por el
órgano en el marco de cada proceso. De esta manera el servicio que
presta cada órgano judicial a un ciudadano se traduce -generalmente- en
una interacción entre peticiones y respuestas, desde el inicio de un
proceso con la presentación de una demanda hasta la conclusión del mismo
con el dictado de una sentencia.

Desde esa perspectiva, un buen servicio de justicia podría verse como
aquél que ante cada petición de un ciudadano es capaz de responder
rápidamente y cumpliendo con los criterios de justicia que deben
aplicarse al caso según la ley. Incluso podría decirse que la primera
parte de esa definición es redundante pues la rapidez en la respuesta, o
mejor dicho, ‘el cumplimiento de plazos’ al ser un requisito legal es
también uno de los ‘criterios de justicia’ que debe satisfacer todo
órgano judicial. Esta idea se ha plasmado dentro el mundo judicial en
frases como ‘la justicia lenta no es justicia’ (Dra.Hilda Kogan,vocal de
la Suprema Corte de la Provincia de Buenos Aires, accesible
[aquí](https://magistradoslp.org.ar/project/dra-hilda-kogan-la-justicia-lenta-no-es-justicia/).

En esa línea, puede verse con claridad que administrar eficientemente
las peticiones que recibe un órgano judicial es una parte vital del
servicio de justicia. Precisamente en este punto aparece la necesidad de
contar no solo con estrategias de control de las peticiones ingresadas
sino también de previsión de las peticiones futuras. Surge así el
problema de elaborar ‘pronósticos sobre la demanda’ que no sería otra
cosa que elaborar una idea aproximada de las peticiones que ingresarán a
un órgano judicial en un período determinado. Dicho pronóstico no solo
permitiría preveer los recursos necesarios para afrontar la carga de
actividad que tendrá un ógano, sino también -y acaso más importante aún-
poder evaluar escenarios generales y detectar cambios estructurales en
la demanda que requieren otro tipo de abordajes (e.g. reformas
legislativas, creación de nuevos órganos, entre mucho otros).

En el caso particular del Poder Judicial de Entre Ríos, en su área de
Planificación y Estadística, interesa abordar el problema de manera
progresiva, teniendo en mente la búsqueda de modelos que nos permitan
generar proyecciones confiables (con bajo margen de error) e
interpretables por nuestros usuarios. La interpretabilidad es una parte
importante de la estadística pública judicial pues trata particularmente
del enfoque de los productos hacia un destinatario no técnico.

Como punto de partida de esa búsqueda (una suerte de *primeros pasos*)
este documento presenta una introducción a dicho proyecto analítico.
Para eso, vamos a crear un dataset de trabajo que nos permita analizar y
proyectar las presentaciones que puede tener el Poder Judicial a nivel
global. Vamos a seleccionar y comparar modelos predictivos probados en
el dominio, para luego iniciar formalmente la etapa de experimentación
con modelos más complejos.

# Trabajos vinculados (antecedentes)

Elaborar pronósticos o proyecciones sobre variables importantes de un
negocio es algo común en el mundo privado, aunque no abundan los casos
documentados en el ambito público judicial. La abundante producción que
existe en materia de planificación (véase la proliferación de órganos
especializados en la materia en los distintos Poderes Judiciales de
nuestro país) y gestión judicial[1] contrasta con la relativamente
escasa producción relativa a proyección y sus metodologías apliacadas al
servicio de justicia.

Sin perjuicio de ello, contamos con abundante material que aborda el
tema ‘proyecciones’ (*forecast*) en distintos dominios a partir de
modelos estadísticos y *machine learning* [Alsharef, A., Aggarwal, K.,
Sonia et
al.](https://link.springer.com/content/pdf/10.1007/s11831-022-09765-0.pdf).
Además de lo anterior, una basta cantidad de recursos se encuentra
disponible en formato abierto y de libre acceso en github[2].

# Explorando el problema y los datos

El dataset inicial contiene información organizada por organismo y
circunscripción judicial de Entre Ríos (unidad territorial que coincide
casi siempre con los departamentos de la provincia, a excepción de
Federación que en la órbita judicial tiene una subdivisión en dos
circunscripciones: Federación, por un lado y la ciudad de Chajarí, por
el otro) entre 2020-07-01 y 2022-06-30.

Dicha información describe la cantidad de presentaciones digitales
efectuadas por Abogados a través del sistema informático de gestión
judicial que tiene el Superior Tribunal de Justicia. Presentaciones que
se realizan en las causas de distintos fueros, a saber: Civil y
Comercial (que incluye Familia y Paz), Laboral y Penal.

Para los análisis iniciales de presentaciones vamos a trabajar con datos
globales de toda la provincia en intervalos mensuales.

## Presentaciones digitales mensuales de Abogados

``` r
df_presentanciones = df %>% 
  group_by(fecha) %>% 
  summarise(presentaciones_abogados = sum(presentaciones_abogados, na.rm = T)) %>% as_tibble()

df_presentanciones_xmes = df_presentanciones %>% 
  mutate(fecha = lubridate::floor_date(fecha, unit = "month")) %>%
  group_by(fecha) %>% summarise(presentaciones_abogados = sum(presentaciones_abogados, na.rm = T)) %>% 
  na.omit() %>% as_tibble()
  
df_presentanciones_xmes %>% 
  ggplot(aes(x=fecha, y=presentaciones_abogados)) +
  geom_line( color="steelblue") + 
  geom_point() +
  xlab("") +
  theme(axis.text.x=element_text(angle=60, hjust=1))
```

<img src="prediccion_de_actividad_files/figure-markdown_github/unnamed-chunk-23-1.png" style="display: block; margin: auto;" />

En el gráfico se ve cieta tendencia incipiete y la estacionalidad
asocidada a los periodos de actividad y recesos judiciales (que se
extienden durante todo el mes de enero y mitad de julio, donde las
presentaciones -sin llegar a 0- disminuyen notablemente). Estamos viendo
24 meses dado que no se disponen de datos previos, lo cual es un límite
importante a considerar en el análisis de la serie temporal.

## Presentaciones diarias por Circunscripcion

En el siguiente gráfico de densidad vemos las presentaciones agrupadas
por circunscripción judicial. Vemos que salvo La Paz e Islas, nos
encontramos con distribuciones bimodales similares en todos los grupos.
Ello podría sugerir que estamos ante datos generados por distintas
poblaciones. Cabe notar que las presentaciones que estamos analizando
son generadas en distintos fueros que -sin dudas- es un factor
importante de discriminación que emplearemo más adelante.

``` r
df %>% 
  group_by(circunscripcion, fecha) %>% 
  summarise(presentaciones_abogados = sum(presentaciones_abogados, na.rm = T)) %>% 
  as_tibble() %>% 
  ggplot()+
  geom_density(aes(x = presentaciones_abogados, fill = as_factor(circunscripcion)), alpha = 0.5)+
  scale_x_log10()+
  theme(legend.position = "note") +
  labs(fill = "Circunscripcion", x = "Presentaciones Diarias") + 
  facet_wrap( ~ circunscripcion, scales = "free") +
  theme(strip.text.x = element_text(size = 20))
```

<img src="prediccion_de_actividad_files/figure-markdown_github/unnamed-chunk-24-1.png" style="display: block; margin: auto;" />

## Indicadores por Mes

``` r
options(scipen = 999)

df %>% 
  mutate(mes = lubridate::month(fecha, label = T)) %>% ungroup() %>% 
  group_by(mes) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% ungroup() %>% 
  as_tibble() %>% 
  pivot_longer(!mes, names_to = "evento", values_to = "cantidad") %>% 
  mutate(evento = factor(evento, levels = c("causas_iniciadas", 
                                            "presentaciones_abogados",
                                            "actos_procesales"), ordered = T)) %>% 
  ggplot()+
    geom_col(aes(x = mes, y = cantidad, fill=mes))+
    scale_fill_viridis_d()+
    theme(axis.text.x=element_text(angle=90,hjust=1)) +
    labs(x = 'mes',
         y = 'cantidad',
         fill = 'mes') +
     facet_wrap( ~ evento)
```

<img src="prediccion_de_actividad_files/figure-markdown_github/unnamed-chunk-25-1.png" style="display: block; margin: auto;" />

## Indicadores por día

``` r
options(scipen = 999)

df %>% 
  mutate(diaS = lubridate::wday(fecha, label = T)) %>% ungroup() %>% 
  group_by(diaS) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% ungroup() %>% 
  pivot_longer(!diaS, names_to = "evento", values_to = "cantidad") %>% 
  mutate(evento = factor(evento, levels = c("causas_iniciadas", 
                                            "presentaciones_abogados",
                                            "actos_procesales"), ordered = T)) %>% 
  as_tibble() %>% 
  ggplot()+
    geom_col(aes(x = diaS, y = cantidad, fill=diaS))+
    scale_fill_viridis_d()+
    labs(x = 'día se semana',
         y = 'cantidad',
         fill = 'día de la semana') +
     facet_wrap( ~ evento)
```

<img src="prediccion_de_actividad_files/figure-markdown_github/unnamed-chunk-26-1.png" style="display: block; margin: auto;" />

## Correlación

``` r
GGally::ggpairs(df %>% select_if(is.numeric) %>% as_tibble())
```

<img src="prediccion_de_actividad_files/figure-markdown_github/unnamed-chunk-27-1.png" style="display: block; margin: auto;" />

# Modelos predictivos univariantes

A continuación analizaremo la serie siguiendo distintos modelos.
Emplearemos en primer lugar modelos univariantes que nos darán una idea
de la *predictibilidad* de la serie[3], para luego avanzar con modelos
multivariantes.

## Dataset

Transformamos nuestro dataset para ajustarlo al análisis creando un
campo fecha y un campo valor para la cantidad de presentaciones
digitales generadas por mes en todo el Poder Judicial.

``` r
df_presentanciones_xmes = df_presentanciones_xmes %>% rename(date = fecha, value = presentaciones_abogados) 
# DT::datatable(df_presentanciones_xmes)
summary(df_presentanciones_xmes)
```

    ##       date                value      
    ##  Min.   :2020-07-01   Min.   : 4578  
    ##  1st Qu.:2020-12-24   1st Qu.:60168  
    ##  Median :2021-06-16   Median :69381  
    ##  Mean   :2021-06-16   Mean   :60948  
    ##  3rd Qu.:2021-12-08   3rd Qu.:74474  
    ##  Max.   :2022-06-01   Max.   :87630

## Dividimos el data set para entrenamiento de modelos

``` r
splits <- initial_time_split(df_presentanciones_xmes, prop = 0.8)
```

## Creando modelos: ARIMA, Exponential Smoothing, Linear Regression y MARS

A continuación vamos crear 4 modelos de los datos de presentaciones
(modelos univariantes) que nos servirán de ‘linea de base’ para armar
los modelos más complejos, que incluirán variables predictoras (modelos
multivariantes).[4]

Partiremos entonces de modelos extendidos y ampliamente aplicados en la
literatura vinculada a proyección en series temporales: ARIMA, Suavizado
Exponencial, Regresión Lineal y MARS. El primero es un modelo
autorregresivo integrado de media móvil (*AutoRegresive Integrated
Moving Average*) que permite obtener una descripción de un valor como
una función lineal de datos anteriores y errores aleatorios, permitiendo
resaltar los componentes cíclicos o estacionales de una serie. Para este
modelo se sugieren no menos de 50 datos, el doble de lo que nosotros
tenemos a disposición por lo que es esperable una baja performance del
modelo. El Suavizado Exponencial calcula promedios ponderados de las
observaciones asignando mayor peso cuanto más reciente es la
observación.[5]. Utilizaremos, finalmente, una regresión lineal
empleando la tendencia y estacionalidad de la serie, y una versión más
sofisticada de regresion empleando el algoritmo MARS (*Multivariate
Adaptive Regression Spline*).

### 1 Auto ARIMA

``` r
# stepwise=FALSE and approximation=FALSE
model_fit_arima_no_boost <- arima_reg() %>%
    set_engine(engine = "auto_arima", stepwise=FALSE,approximation=FALSE) %>%
    fit(value ~ date, data = training(splits))
```

### 2: Boosted Auto ARIMA

``` r
model_fit_arima_boosted <- arima_boost(min_n = 2, learn_rate = 0.015) %>%
    set_engine(engine = "auto_arima_xgboost") %>%
    fit(value ~ date + as.numeric(date) + factor(month(date), ordered = F),
        data = training(splits))
```

### 3 Exponential Smoothing

``` r
model_fit_ets <- exp_smoothing() %>%
    set_engine(engine = "ets") %>%
    fit(value ~ date, data = training(splits))
```

### 4 Linear Regression

``` r
model_fit_lm <- linear_reg() %>%
    set_engine("lm") %>%
    fit(value ~ as.numeric(date) + factor(month(date), ordered = FALSE),
        data = training(splits))
```

### 5 MARS

``` r
model_spec_mars <- mars(mode = "regression") %>%
    set_engine("earth", endspan = 15) 

recipe_spec <- recipe(value ~ date, data = training(splits)) %>%
    step_date(date, features = "month", ordinal = FALSE) %>%
    step_mutate(date_num = as.numeric(date)) %>%
    step_normalize(date_num) %>%
    step_rm(date)
  
wflw_fit_mars <- workflow() %>%
    add_recipe(recipe_spec) %>%
    add_model(model_spec_mars) %>%
    fit(training(splits))
```

## Entrenando los modelos creados

Se entrenan los modelos con la configuración de parámetros asignada.

``` r
models_tbl <- modeltime_table(
    model_fit_arima_no_boost,
    model_fit_arima_boosted,
    model_fit_ets,
    model_fit_lm,
    wflw_fit_mars
)
```

## Calibrando los modelos

Los calibración permite evaluar las predicciones y calcular residuos de
los pronósticos efectuados por cada modelo. Los datos para la
calibración surgen de la división del dataset en datos de entrenamiento
y testeo (**initial_time_split**).

``` r
calibration_tbl <- models_tbl %>%
    modeltime_calibrate(new_data = testing(splits))
```

## Evaluando predicciones en datos de Testing y comprobando la precisión de los modelos

``` r
calibration_tbl %>%
    modeltime_forecast(
        new_data    = testing(splits),
        actual_data = df_presentanciones_xmes
    ) %>%
    plot_modeltime_forecast(
      .legend_max_width = 25, # For mobile screens
      .interactive      = interactive
    )
```

<img src="prediccion_de_actividad_files/figure-markdown_github/unnamed-chunk-37-1.png" style="display: block; margin: auto;" />

Vemos que en general (salvo el 5°) los modelos tienen un ajuste
*aceptable* a la serie temporal de presentaciones. Pero se advierte que
todos reproducen la caida de valores de julio 2021, cosa que no sucedió
en 2022, en este punto están sobreajustado las predicciones a los datos.

``` r
calibration_tbl %>%
  modeltime_accuracy() %>% 
  gridExtra::grid.table()
```

<img src="prediccion_de_actividad_files/figure-markdown_github/unnamed-chunk-38-1.png" style="display: block; margin: auto;" />

Los resultados de las predicciones muestran que el Modelo 4, basado en
una Regresión Lineal, presenta los mejores resultados considerando los
errores de los modelos (residuos). Considerando el error porcentual
absoluto medio (MAPE) el modelo tiene una alta calificación
(Lewis:1982), con un error promedio del **10.5%**. Este modelo plantea
una regresión de las presentaciones a partir de la tendencia
(*as.numeric(date)*) y la estacionalidad (*month(date)*).

## Reentrenamiento de los modelos y proyección de un año

``` r
refit_tbl <- calibration_tbl %>%
    modeltime_refit(data = df_presentanciones_xmes)

refit_tbl %>%
    modeltime_forecast(h = "1 years", actual_data = df_presentanciones_xmes) %>%
    plot_modeltime_forecast(
      .legend_max_width = 25, # For mobile screens
      .interactive      = interactive)
```

<img src="prediccion_de_actividad_files/figure-markdown_github/unnamed-chunk-39-1.png" style="display: block; margin: auto;" />

# Consideraciones Finales

En esta nota introductoria vimo modelos simples univariantes para la
predicción de presentaciones digitales en el Poder Judicial de Entre
Rios, y encontramos resultados que parecen prometedores. Nuestros
próximos pasos serán explorar modelos más complejos multivariantes,
ampliando el dataset y períodos etudiados. En particular vamos a
experimentar de manera sistemática dos algoritmos importantes y
largamente documentados en la bibliografía especializada: NeuralProphet
y LightGBM.

<!-- # NeuralProphet y LightGBM -->
<!-- ```{r} -->
<!-- # https://rpubs.com/tdneumann/351073 R-BO prophet -->
<!-- #https://github.com/AlbertoAlmuinha/neuralprophet  -->
<!-- #https://github.com/JedStephens/prophethyperbayes optimizacion -->
<!-- # https://rpubs.com/tdneumann/351073 -->
<!-- #[new_stoping_criterion](https://www.amazon.science/publications/automatic-termination-for-hyperparameter-optimization) -->
<!-- ``` -->

[1] <https://www.cij.gov.ar/gestion-judicial.html>

[2] <https://github.com/search?q=forecast>

[3] <https://otexts.com/fpp2/what-can-be-forecast.html>

[4] para el armado de estos modelos seguimos la guía que puede
consultarse aquí
<https://cran.r-project.org/web/packages/modeltime/vignettes/getting-started-with-modeltime.html>

[5] <https://otexts.com/fpp2/expsmooth.html>
