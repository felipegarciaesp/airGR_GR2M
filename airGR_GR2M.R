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
PPload <- PPload %>% select('date','7336001') %>% rename('Pp (mm)' = '7336001')
tmax_load <- tmax_load %>% select('date','7336001') %>% rename('tmax (deg)' = '7336001')
tmin_load <- tmin_load %>% select('date','7336001') %>% rename('tmin (deg)' = '7336001')
Qload <- Qload %>% select('date','7336001') %>% rename('Q (mm)' = '7336001')

# Calculamos la temperatura promedio y lo almacenamos en un nuevo dataframe:
# 1ero, combinamos los df por la columna 'date':
temp_combined <- left_join(tmax_load, tmin_load, by='date')

# 2do, calculamos la temperatura promedio usando mutate y rowMeans:
temp_avg <- temp_combined %>% 
  mutate(temperature_avg = rowMeans(select(., 'tmax (deg)', 'tmin (deg)'), na.rm = TRUE)) %>% 
  select(date, temperature_avg)

# 3ero, agregamos una nueva columna en el dataframe con los días julianos.
# Esto es para poder ocupar la formula de Oudin.
temp_avg$julian_date <- yday(temp_avg$date)

# Calcularemos la ET con la ecuación de Oudin:
# 1ero, creamos un dataframe vacío para ir almacenando los datos:
n <- nrow(PPload) #Determinamos la cantidad de filas (meses) que tiene PPload.
ET_mon <- data.frame(matrix(ncol = 2, nrow = n)) #dataframe de 2 columna y n filas.
colnames(ET_mon) <- c('date', 'ET (mm)')

# 2do, rellenamos la primera columna con la fecha:
ET_mon[,1] <- temp_avg$date

# 3ero, calculamos la ET con la ecuación de Oudin:
ET_mon[,2] <- 30 * PE_Oudin(JD = temp_avg$julian_date, Temp = temp_avg$temperature_avg,
                            Lat = lat_point, LatUnit = "deg")

# Se guarda resultado de la ETP de Oudin en la carpeta de trabajo:
write.csv(ET_mon, 'ETobs.csv')

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

ini_cal <- as.Date("1989-01-01")
fin_cal <- as.Date("2020-04-01")

# Definido el periodo de calibracion, filtramos los dataframe de Pp y ET para
# estas fechas.

PPload_calib <- PPload[PPload$date >= ini_cal & PPload$date <= fin_cal, ]
ET_calib <- ET_mon[ET_mon$date >= ini_cal & ET_mon$date <= fin_cal, ]

# Debemos convertir las fechas a un formato POSIXct, requerido para correr el
# modelo GR2M:

PPload_calib$date <- as.POSIXct(PPload_calib$date)

# NOTA: la conversion a POSIXct prefiero hacerla en esta etapa y no antes. Esto
# debido a que al hacer el filtrado con las fechas en formato POSIXct no me va
# a tomar la primera fecha. Por eso prefiero filtrar y despues transformar el
# formato de las fechas. Ten en consideración esto para futuros codigos.





# El modelo airGR requiere de los siguientes pasos:
# 1. Crear las entradas al modelo en la variable InputsModel_Cuenca mediante la
# función "CreateInputsModel".
InputsModel_Cuenca <- CreateInputsModel(FUN_MOD = RunModel_GR2M, 
                                        DatesR = PPload_calib$date,
                                        Precip = PPload_calib$`Pp (mm)`,
                                        PotEvap = ET_calib$`ET (mm)`)
