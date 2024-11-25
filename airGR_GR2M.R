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

# Se identifican rutas de estos archivos:
ruta_pp_mon <- paste0(ruta_archivos,"/",pp_mon)
ruta_tmax_mon <- paste0(ruta_archivos,"/",tmax_mon)
ruta_tmin_mon <- paste0(ruta_archivos,"/",tmin_mon)

# Se cargan los archivos requeridos (pp, tmax y tmin):
PPload <- read_csv(ruta_pp_mon)
tmax_load <- read_csv(ruta_tmax_mon)
tmin_load <- read_csv(ruta_tmin_mon)