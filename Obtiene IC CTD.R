# Cheapest to deliver futuros 
Sys.setlocale("LC_ALL", "Spanish")
source('Z:/II Responsabilidades/26 BMK Global/Inversiones/Proyecto RF/Valuaciones y Resultados/Código/Funciones Renta Fija.R')
source('Z:/II Responsabilidades/26 BMK Global/Inversiones/Proyecto RF/IC CTD Futuros/Código/Funciones IC CTD.R')

# Revisamos conexión a bbg ----
library(Rblpapi)
blpConnect()

# Fechas ----
hol <- read_excel('Z:/II Responsabilidades/50 Liquidez/Feriados.xlsx', sheet =  1)
date <- Sys.Date()
date_ayer <- dates(hol, date,1,"resta",'MX')

genera_ic_ctd(hol, date)

