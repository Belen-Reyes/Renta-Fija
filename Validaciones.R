# Verifica IC y cupones y vencimientos 
# RENTA FIJA 
Sys.setlocale("LC_ALL", "Spanish")
source('Z:/II Responsabilidades/26 BMK Global/Inversiones/Proyecto RF/Valuaciones y Resultados/Código/Funciones Renta Fija.R')
source('Z:/II Responsabilidades/26 BMK Global/Inversiones/Proyecto RF/Validaciones/Funciones Validaciones.R')

# # Fechas ----
# hol <- read_excel('Z:/II Responsabilidades/50 Liquidez/Feriados.xlsx', sheet =  1)
# date <- Sys.Date()
# date_ayer <- dates(hol, date,1,"resta",'MX')

# ----------------------- VALIDA CUPONES Y VENCIMIENTOS ------------------------ 
valida_cup_ven <- function(hol, date_ayer){
  date_anteayer <- dates(hol, date_ayer,1,"resta",'MX')
  
  rutas <- list(
    j = 'J:/outdir/reports/',
    px = 'Z:/II Responsabilidades/2 Inversiones/datos/precios/',
    global = 'Z:/II Responsabilidades/26 BMK Global/',
    futuros = 'J:/sysdir/import/precios/px_findur/',
    proyecto = 'Z:/II Responsabilidades/26 BMK Global/Inversiones/Proyecto RF/'
  )
  
  # Nombres archivos ----
  names <- list(
    ic = list(hoy = name_ic(rutas, date_ayer),
              ayer = name_ic(rutas, date_anteayer)),
    
    datos_inv = name_datos_inv(rutas, date_ayer),
    
    cup_ven = name_cup_ven(rutas, date_ayer)
  )
  
  # Leemos archivos ----
  insumos <- list(
    ic = list(hoy = lee_ic(names$ic$hoy),
              ayer = lee_ic(names$ic$ayer)),
    
    datos_inv = lee_datos_inv(names),
    
    cup_ven = list(goi = lee_cup_ven(names$cup_ven, 'GOI'),
                   bmk = lee_cup_ven(names$cup_ven, 'BMK'))
  )
  
  # Parte 1: Verificar el archivo de cupones y vencimientos
  verifica_cup_ven(insumos$datos_inv$cupones,
                   insumos$datos_inv$vencimientos,
                   insumos$cup_ven$goi)
}

# --------------------------------- VALIDA IC ---------------------------------- 
valida_ic <- function(hol, date_ayer){
  date_anteayer <- dates(hol, date_ayer,1,"resta",'MX')
  
  rutas <- list(
    j = 'J:/outdir/reports/',
    px = 'Z:/II Responsabilidades/2 Inversiones/datos/precios/',
    global = 'Z:/II Responsabilidades/26 BMK Global/',
    futuros = 'J:/sysdir/import/precios/px_findur/',
    proyecto = 'Z:/II Responsabilidades/26 BMK Global/Inversiones/Proyecto RF/'
  )
  
  # Nombres archivos ----
  # Nombres archivos ----
  names <- list(
    ic = list(hoy = name_ic(rutas, date_ayer),
              ayer = name_ic(rutas, date_anteayer)),
    
    ic_copia = name_ic_copia(rutas, date_ayer),
    
    datos_inv = name_datos_inv(rutas, date_ayer),
    
    cup_ven = name_cup_ven(rutas, date_ayer)
  )
  
  # Leemos archivos ----
  insumos <- list(
    ic = list(hoy = lee_ic(names$ic$hoy),
              ayer = lee_ic(names$ic$ayer)),
    
    datos_inv = lee_datos_inv(names),
    
    cup_ven = list(goi = lee_cup_ven(names$cup_ven, 'GOI'),
                   bmk = lee_cup_ven(names$cup_ven, 'BMK'))
  )
  
  # Parte 2: Verificar el cambio en la IC 
  verifica_ic(insumos$ic$hoy,
              insumos$ic$ayer,
              insumos$datos_inv$comprasventas,
              insumos$cup_ven,
              date_ayer)
  
  file.copy(names$ic$hoy, 
            names$ic_copia, 
            overwrite = TRUE)
}


# ----------------------------- VALIDA PX IC_CTD ------------------------------- 
valida_px_ic_ctd <- function(hol, date_ayer){
  date_anteayer <- dates(hol, date_ayer,1,"resta",'MX')
  
  rutas <- list(
    j = 'J:/outdir/reports/',
    px = 'Z:/II Responsabilidades/2 Inversiones/datos/precios/',
    global = 'Z:/II Responsabilidades/26 BMK Global/',
    futuros = 'J:/sysdir/import/precios/px_findur/',
    proyecto = 'Z:/II Responsabilidades/26 BMK Global/Inversiones/Proyecto RF/'
  )
  
  # Nombres archivos ----
  names <- list(
    ic = list(hoy = name_ic(rutas, date_ayer),
              ayer = name_ic(rutas, date_anteayer)),
    
    datos_inv = name_datos_inv(rutas, date_ayer),
    
    cup_ven = name_cup_ven(rutas, date_ayer),
    
    ctd = name_ic_ctd(rutas, date_ayer),
    
    ctd_copia = name_ic_ctd_copia(rutas, date_ayer),
    
    px = name_px(rutas, date_ayer)
  )
  
  # Leemos archivos ----
  insumos <- list(
    ic = list(hoy = lee_ic(names$ic$hoy),
              ayer = lee_ic(names$ic$ayer)),
    
    datos_inv = lee_datos_inv(names),
    
    cup_ven = list(goi = lee_cup_ven(names$cup_ven, 'GOI'),
                   bmk = lee_cup_ven(names$cup_ven, 'BMK')),
    
    ic_fut_ctd = lee_ic_ctd(names$ctd),
    
    px = lee_px(names$px)
  )
  
  verifica_ic_ctd(insumos$ic_fut_ctd$ISIN,
                  insumos$px$ISIN)
  
  file.copy(names$ctd, 
            names$ctd_copia, 
            overwrite = TRUE)
  
}
