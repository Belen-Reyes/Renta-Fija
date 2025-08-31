# RENTA FIJA 
Sys.setlocale("LC_ALL", "Spanish")
source('Z:/II Responsabilidades/26 BMK Global/Inversiones/Proyecto RF/Valuaciones y Resultados/Código/Funciones Renta Fija.R')
source('Z:/II Responsabilidades/26 BMK Global/Inversiones/Proyecto RF/Cupones y Vencimientos/Código/Funciones Cupones y Vencimientos.R')

# Revisamos conexión a bbg ----
library(Rblpapi)
blpConnect()

# Fechas ----
hol <- read_excel('Z:/II Responsabilidades/50 Liquidez/Feriados.xlsx', sheet =  1)
date <- Sys.Date()
date_ayer <- dates(hol, date,1,"resta",'MX')

genera_cup_ven(hol, date)


