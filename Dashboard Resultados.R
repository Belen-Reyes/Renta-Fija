# Dashboard resultados RF
Sys.setlocale("LC_ALL", "Spanish")

# Paqueterías ----
list.of.packages <- c(
  "shinydashboard", "ggplot2", 'formattable', 'scales'
)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages)

# Funciones ----
source('Z:/II Responsabilidades/26 BMK Global/Inversiones/Proyecto RF/Valuaciones y Resultados/Código/Renta Fija.R')
source('Z:/II Responsabilidades/26 BMK Global/Inversiones/Proyecto RF/Valuaciones y Resultados/Código/Funciones dashboard.R')

# Fechas ----
hol <- read_excel('Z:/II Responsabilidades/50 Liquidez/Feriados.xlsx', sheet =  1)
date <- Sys.Date()
date_ayer <- dates(hol, date,1,"resta",'MX')

# Proceso ---- 
source('Z:/II Responsabilidades/26 BMK Global/Inversiones/Proyecto RF/Valuaciones y Resultados/Código/Shiny dashboard.R')

shinyApp(ui, server)

