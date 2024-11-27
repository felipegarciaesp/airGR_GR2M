rm(list=ls())
graphics.off()

# =====================================================================
# Modelo airGR para Cuenca Cauquenes / Felipe Garcia
# =====================================================================

# Instalacion de paquetes:
#install.packages("airGR")
#install.packages("zoo")
#install.packages("openxlsx")
#install.packages("hydroGOF")
#install.packages("hydroTSM")
#install.packages("lubridate")

# Carga de paquetes:
library(zoo)
library(airGR)
library (openxlsx)
library(hydroGOF)
library(hydroTSM)
library(readr)
library(dplyr)
library(tibble)
library(lubridate)

# Paquete tibble es para ocupar column_to_rownames, que nos permite dejar una 
# columna como indice del dataframe y eliminarlo como columna del dataframe.

# Seteamos el directorio de trabajo donde trabajaremos
setwd('C:/Users/Usuario/Codigos_R/tarea_etp_mensual')

# Se identifica nombre de la carpeta a utilizar:
carpeta <- "camels_cl_7336001"
ruta_archivos <- paste0(getwd(),"/",carpeta)

# Se identifican nombres de archivos:
pp_mon <- "precip_cr2met_mon.csv"
tmax_mon <- "tmax_cr2met_mon.csv"
tmin_mon <- "tmin_cr2met_mon.csv"
q_mon <- "q_mm_mon.csv"

# Se identifican rutas de estos archivos:
ruta_pp_mon <- paste0(ruta_archivos,"/",pp_mon)
ruta_tmax_mon <- paste0(ruta_archivos,"/",tmax_mon)
ruta_tmin_mon <- paste0(ruta_archivos,"/",tmin_mon)
ruta_q_mon <- paste0(ruta_archivos,"/",q_mon)

# Se cargan los archivos requeridos (pp, tmax, tmin y q):
PPload <- read_csv(ruta_pp_mon)
tmax_load <- read_csv(ruta_tmax_mon)
tmin_load <- read_csv(ruta_tmin_mon)
Qload <- read_csv(ruta_q_mon)

# Asignamos la latitud de la cuenca a la variable lat_point:
lat_point <- -36.02 #Correspondiente a la salida de la cuenca Cauquenes.

# Dejamos los dataframe solo con fecha y la data de interes:
# Ojo que esto lo podria dejar como una funcion (u OOP):
PPload <- PPload %>% select('date','7336001') %>% rename(P = '7336001')
tmax_load <- tmax_load %>% select('date','7336001') %>% rename(tmax = '7336001')
tmin_load <- tmin_load %>% select('date','7336001') %>% rename(tmin = '7336001')
Qload <- Qload %>% select('date','7336001') %>% rename(Q = '7336001')

# Para la cuenca solo nos interesa la temperatura promedio, por lo que la
# calculamos y la almacenamos en un nuevo dataframe:
# 1ero, combinamos los df por la columna 'date':
temp_combined <- left_join(tmax_load, tmin_load, by='date')

# 2do, calculamos la temperatura promedio usando mutate y rowMeans:
temp_avg <- temp_combined %>% 
  mutate(T = rowMeans(select(., 'tmax', 'tmin'), na.rm = TRUE)) %>% 
  select(date, T)

# Ahora calcularemos la ET con la ecuación de Oudin:
# 1ero, creamos un dataframe vacío para ir almacenando los datos:
n <- nrow(PPload) #Determinamos la cantidad de filas (meses) que tiene PPload.
ET_mon <- data.frame(matrix(ncol = 2, nrow = n)) #dataframe de 2 columna y n filas.
colnames(ET_mon) <- c('date', 'ET')

# 2do, rellenamos la primera columna con la fecha:
ET_mon[,1] <- temp_avg$date

# 3ero, agregamos una nueva columna en el dataframe con los días julianos.
# Esto es para poder ocupar la formula de Oudin.
temp_avg$julian_date <- yday(temp_avg$date)

# 4to, calculamos la ET con la ecuación de Oudin:
ET_mon[,2] <- 30 * PE_Oudin(JD = temp_avg$julian_date, Temp = temp_avg$T,
                            Lat = lat_point, LatUnit = "deg")

# Se guarda resultado de la ETP de Oudin en la carpeta de trabajo:
write.csv(ET_mon, 'ETobs.csv')

# Creamos un dataframe que almacena toda la info obs de la cuenca:
BasinObs <- PPload %>%
  left_join(Qload, by = 'date') %>%
  left_join(ET_mon, by = 'date') %>%
  left_join(select(temp_avg, date, T), by = 'date')

# Cambiamos el formato de fecha a POSIXct:
BasinObs$date <- as.POSIXct(BasinObs$date)

# De esta forma tenemos el dataframe BasinObs que contiene:
# date: fechas en formato POSIXct.
# P: precipitacion en mm/mes.
# Q: escorrentia en mm/mes.
# ET: evapotranspiracion en mm/mes.
# T: temperatura promedio en °C.

# Para implementar airGR, no podemos tener valores NA en la precipitación o ET.
# Por esto, filtraremos del dataframe las filas NA.
BasinObs <- BasinObs %>%
  filter(!is.na(P) & !is.na(ET))

# A continuación, se leerán las variables necesarias para calibrar y validar el
# modelo GR2M de airGR para estimar caudales.
# Para esto se han cargado las librerías zoo, airGR, openxlsx, hydroGOF.

# Algunos comandos de interés en R:
# Con class podemos verificar la clase de la columna "date".
# class(PPload$date)

# Con str podemos obtener una estructura detallada del dataframe:
# str(PPload)

# En este caso, se va a calibrar el modelo con los datos del periodo 1989-2020.
# Se va a querer que en la calibracion esten contenidos los datos de los ultimos
# años, ya que son los mas secos. Luego se va a verificar con los datos de los
# primeros 10 años.

ini_cal <- '01/1989'
fin_cal <- '04/2020'

# Determinamos la cantidad de meses a calibrar. Esto va a estar relacionado
# con la cantidad de meses que tenemos datos observados de Q (recordar que esta 
# es la variable a calibrar).
meses_cal <- 376 #ESTO LO TENEMOS QUE CAMBIAR!

# Vamos a inicializar algunos dataframe vacios para guardar los parametros del
# modelo, los Nash-Sutcliffe Efficiency y los Q simulados.
# 1. El modelo tiene 2 parametros, los guardaremos en el df 'parametros'.
parametros <- data.frame(matrix(ncol = 1, nrow = 2))
# 2. Solo hay un valor de Nash:
Ef_NS <- data.frame(matrix(ncol = 1, nrow = 1))
# 3. Creamos un df con 'meses_cal' con el numero de columnas
Q_sim <- data.frame(matrix(ncol = 1, nrow = meses_cal))

# El modelo airGR requiere de los siguientes pasos:
# 1. Crear las entradas al modelo en la variable InputsModel_Cuenca mediante la
# función "CreateInputsModel".
InputsModel_Cuenca <- CreateInputsModel(FUN_MOD = RunModel_GR2M, 
                                        DatesR = BasinObs$date,
                                        Precip = BasinObs$P,
                                        PotEvap = BasinObs$ET)

# 2. Mediante la funcion "Seq" establecemos las fechas de calibracion y 
# validacion del modelo. Recordar que la funcion which identifica las posiciones
# (indices) de los elementos.
Run_Calib_Period <- seq(which(format(BasinObs$date, format = "%m/%Y")==ini_cal),
                        which(format(BasinObs$date, format = "%m/%Y")==fin_cal))

# 3. Mediante la función "CreateRunOptions" establecemos las opciones para correr 
# el modelo
RunCalib_Options_Cuenca <- CreateRunOptions(FUN_MOD = RunModel_GR2M,
                                            InputsModel = InputsModel_Cuenca, IndPeriod_Run = Run_Calib_Period)

# 4. Mediante la funcion "CreateInputsCrit" establecemos las opciones y funciones
# de calibracion del modelo.
InputsCrit_Cuenca <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE,
                                      InputsModel = InputsModel_Cuenca,
                                      RunOptions = RunCalib_Options_Cuenca,
                                      Obs = BasinObs$Q[Run_Calib_Period])

# 5. Mediante la función "CreateCalibOptions" establecemos las funciones de 
# calibración del modelo
CalibOptions_Cuenca <- CreateCalibOptions(FUN_MOD = RunModel_GR2M, FUN_CALIB =
                                            Calibration_Michel)

# 6. Mediante la funcion "Calibration" calibramos el modelo
Calib_Cuenca <- Calibration(InputsModel = InputsModel_Cuenca, RunOptions = RunCalib_Options_Cuenca,
                            InputsCrit = InputsCrit_Cuenca, CalibOptions = CalibOptions_Cuenca,
                            FUN_MOD = RunModel_GR2M, FUN_CRIT = ErrorCrit_NSE, FUN_CALIB =
                              Calibration_Michel)

# Una vez que tenemos el modelo calibrado, guardamos los parámetros en una 
# variable auxiliar, los que usamos para validar el modelo
Parametros_Cuenca <- Calib_Cuenca$ParamFinalR
parametros[,1] <- Parametros_Cuenca

# Correremos para todo el periodo de calibracion, para obtener los Q simulados.
# Para ello usamos la función "RunModel"
Qsim_Calib_Cuenca <- RunModel(InputsModel = InputsModel_Cuenca, RunOptions =
                                RunCalib_Options_Cuenca,
                              Param = Parametros_Cuenca, FUN = RunModel_GR2M)

# Colectamos los datos simulados para todo el periodo de calibracion
Q_sim[,1] <- Qsim_Calib_Cuenca$Qsim

# Establecemos el cálculo de coeficientes de evaluación
InputsCrit_NSE<- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, 
                                  InputsModel = InputsModel_Cuenca,
                                  RunOptions = RunCalib_Options_Cuenca, 
                                  Obs = BasinObs$Q[Run_Calib_Period])

# Colectamos los coeficientes de evaluacion y los guardamos en "OutputsCrit_NSE"
OutputsCrit_NSE <- ErrorCrit_NSE(InputsCrit = InputsCrit_NSE, 
                                 OutputsModel = Qsim_Calib_Cuenca)

Ef_NS <- OutputsCrit_NSE$CritValue

# Podemos usar la función ggof para obtener una evaluación grafica de la 
# simulación
ggof(sim=Qsim_Calib_Cuenca$Qsim, obs=InputsCrit_NSE$Obs, ftype="ma", FUN=mean)

ggof(sim=Qsim_Calib_Cuenca$Qsim, obs=InputsCrit_NSE$Obs, dates = ,ftype="ma", FUN=mean)