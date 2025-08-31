rm(list=ls())#para limpiar el entorno

# RENTA FIJA 
Sys.setlocale("LC_ALL", "Spanish")

# Paqueter?as ----
list.of.packages <- c(
  "dplyr", "writexl", 'readxl', 'readr', 'openxlsx', 'stringr', 'tidyr', 'lubridate', 
  'purrr', 'bizdays', 'Rblpapi', 'shiny', 'shinyalert', 'shinythemes', 'dipsaus', 'here'
)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages)

# Funciones ----
source('Z:/II Responsabilidades/26 BMK Global/Inversiones/Proyecto RF/Cupones y Vencimientos/Código/Funciones Cupones y Vencimientos.R')
source('Z:/II Responsabilidades/26 BMK Global/Inversiones/Proyecto RF/Valuaciones y Resultados/Código/Renta Fija.R')
source('Z:/II Responsabilidades/26 BMK Global/Inversiones/Proyecto RF/IC CTD Futuros/Código/Funciones IC CTD.R')
source('Z:/II Responsabilidades/26 BMK Global/Inversiones/Proyecto RF/CPI y FRN/Código/Tasas FRN y CPI.R')
source('Z:/II Responsabilidades/26 BMK Global/Inversiones/Proyecto RF/Validaciones/Validaciones.R')

# Conexión a bbg ----
library(Rblpapi)
blpConnect()

# Fechas ----
hol <- read_excel('Z:/II Responsabilidades/50 Liquidez/Feriados.xlsx', sheet =  1)
date <- Sys.Date()
date_ayer <- dates(hol, date,1,"resta",'MX')

# Proceso ----
source('Z:/II Responsabilidades/26 BMK Global/Inversiones/Proyecto RF/Valuaciones y Resultados/Código/Shiny.R')

shinyApp(ui_rf, server_rf)
