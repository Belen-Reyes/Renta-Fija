# Tasas FRN y CPI
Sys.setlocale("LC_ALL", "Spanish")
source('Z:/II Responsabilidades/26 BMK Global/Inversiones/Proyecto RF/CPI y FRN/Código/Funciones Tasas FRN y CPI.R')
source('Z:/II Responsabilidades/26 BMK Global/Inversiones/Proyecto RF/Valuaciones y Resultados/Código/Funciones Renta Fija.R')

# # Revisamos conexión a bbg ----
# library(Rblpapi)
# blpConnect()
# 
# 
# # Fechas ----
# hol <- read_excel('Z:/II Responsabilidades/50 Liquidez/Feriados.xlsx', sheet =  1)
# date <- Sys.Date()
# date_ayer <- dates(hol, date,1,"resta",'MX')

# ----------------------------- PRIMERA PARTE: FRN ----------------------------- 
genera_frn <- function(hol, date_ayer){
  options(digits = 16)
  date_frn <- dates(hol, date_ayer,1,"suma",'US')
  # Rutas ----
  rutas <- list(j = 'J:/outdir/reports/',
                proyecto = 'Z:/II Responsabilidades/26 BMK Global/Inversiones/Proyecto RF/', 
                global = 'Z:/II Responsabilidades/26 BMK Global/',
                px = 'Z:/II Responsabilidades/2 Inversiones/datos/precios/')
  
  # Nombres archivos ----
  names <- list(ic = name_ic(rutas, date_ayer),
                px = name_px(rutas, date_ayer),
                datos = name_datos(rutas),
                cpi = name_cpi(rutas),
                frn = name_frn(rutas),
                frn_index = 'USBMMY3M Index',
                bmk = name_bmk(rutas, date_ayer)
                )
  
  # Leemos archivos ----
  insumos <- list(ic = lee_ic(names$ic),
                  px = lee_px(names$px),
                  bmk = lee_bmk(names$bmk),
                  catalogo = read_excel(names$datos, sheet = 'Catalogo'))
  
  tasas_frn <- list(index_wb = loadWorkbook(names$frn),
                    index = read_excel(names$frn, sheet = names$frn_index),
                    spread = read_excel(names$frn, sheet = 'Spreads FRN'), 
                    reaperturas = read_excel(names$frn, sheet = 'Reaperturas'))
  
  cpi <- list(cpi_wb = loadWorkbook(names$cpi),
              cpi = read_excel(names$cpi, sheet = 'CPI'),
              paises = read_excel(names$cpi, sheet = 'Paises'))
  
  # Leemos IC y tomamos los FRN 

  frn_ic <- insumos$ic %>%
  mutate(Divisa = substr(ISIN, 1, 2),
         Inst = word(Instrumento)) %>%
  left_join(insumos$px %>% 
              select(ISIN,`ULTIMO CUPON`), 
            by = 'ISIN') %>%
  left_join(insumos$catalogo,
            by = c('Inst', 'Divisa')) %>%
  filter(`Tipo de instrumento` == 'FRNs' & `ULTIMO CUPON` <= date_frn)
  
  frn_bmk <- insumos$bmk %>%
    mutate(Divisa = substr(ISIN, 1, 2),
           Inst = word(Instrumento)) %>%
    left_join(insumos$px %>% 
                select(ISIN,`ULTIMO CUPON`), 
              by = 'ISIN') %>%
    left_join(insumos$catalogo,
              by = c('Inst', 'Divisa')) %>%
    filter(`Tipo de instrumento` == 'FRNs' & `ULTIMO CUPON` <= date_frn & !(ISIN %in% frn_ic$ISIN))

  frn <- bind_rows(frn_ic,
                   frn_bmk)
  
  # Reapertura ----
  # Verificamos que exista la información de reaperturas de todos los FRN de la cartera
  verifica_reapertura(frn,tasas_frn, names$frn)

  # Lockouts ----
  withProgress(
    message = 'Obtenemos lockouts...',
    detail = 'Esto puede tomar unos segundos, por favor espera.',
    value = 0, {
      
      # Obtenemos Lockouts de los FRN de la cartera y los imprimimos en ruta 
      list_frn <- split(frn %>%
                          left_join(tasas_frn$reaperturas %>% 
                                      mutate_at(vars(-ISIN), as.Date), by = 'ISIN'), 
                        f = frn$ISIN)
      
      incProgress(1/3)
      Sys.sleep(0.25)
      
      lockouts_frn <- lapply(list_frn,obtiene_lockout)
      
      incProgress(2/3)
      Sys.sleep(0.25)
      
      lockouts_frn_dates <- imprime_lockouts(lockouts_frn, 
                                             tasas_frn$index_wb,
                                             names$frn,
                                             frn$ISIN)
      incProgress(1)
      Sys.sleep(0.25)
    }
  )
  
  
  # Índice y spreads de Tasas FRN -------
  withProgress(
    message = 'Actualizando indice y spread...',
    detail = 'Esto puede tomar unos segundos, por favor espera.',
    value = 0, {
      
      # Actualizamos
      frn_index <- actualiza_frn_index(tasas_frn, names)
      incProgress(1/2)
      Sys.sleep(0.25)
      
      frn_spread <- actualiza_frn_spread(frn, tasas_frn, names)
      incProgress(2/2)
      Sys.sleep(0.25)
    }
  )
  
  # Historico de tasas FRN ------------
  withProgress(
    message = 'Obteniendo historico de tasas FRN...',
    detail = 'Esto puede tomar unos segundos, por favor espera.',
    value = 0, {
      
      # Actualizamos
      tasas_frn_hist <- obtiene_tasas_frn(frn, 
                                          frn_index, 
                                          frn_spread, 
                                          date_frn,
                                          lockouts_frn_dates)
      incProgress(1/2)
      Sys.sleep(0.25)
      
      imprime_tasas_frn(tasas_frn_hist,
                        tasas_frn$index_wb,
                        names$frn)
      incProgress(2/2)
      Sys.sleep(0.25)
    }
  )
  
}

# ----------------------------- SEGUNDA PARTE: CPI -----------------------------
genera_cpi <- function(hol, date_ayer){
  options(digits = 16)
  date_frn <- dates(hol, date_ayer,2,"suma",'US')
  # Rutas ----
  rutas <- list(j = 'J:/outdir/reports/',
                proyecto = 'Z:/II Responsabilidades/26 BMK Global/Inversiones/Proyecto RF/', 
                px = 'Z:/II Responsabilidades/2 Inversiones/datos/precios/')
  
  # Nombres archivos ----
  names <- list(ic = name_ic(rutas, date_ayer),
                px = name_px(rutas, date_ayer),
                datos = name_datos(rutas),
                cpi = name_cpi(rutas),
                frn = name_frn(rutas),
                frn_index = 'USBMMY3M Index')
  
  # Leemos archivos ----
  insumos <- list(ic = lee_ic(names$ic),
                  px = lee_px(names$px),
                  catalogo = read_excel(names$datos, sheet = 'Catalogo'))
  
  tasas_frn <- list(index_wb = loadWorkbook(names$frn),
                    index = read_excel(names$frn, sheet = names$frn_index),
                    spread = read_excel(names$frn, sheet = 'Spreads FRN'), 
                    reaperturas = read_excel(names$frn, sheet = 'Reaperturas'))
  
  cpi <- list(cpi_wb = loadWorkbook(names$cpi),
              cpi = read_excel(names$cpi, sheet = 'CPI'),
              paises = read_excel(names$cpi, sheet = 'Paises'))
  
  cpi <<- cpi
  # Actualiza CPI ------------
  withProgress(
    message = 'Actualizando CPI...',
    detail = 'Esto puede tomar unos segundos, por favor espera.',
    value = 0, {
      
      # Actualizamos el CPI de los paises en los que se tienen TIPS
      actualiza_cpi(cpi, names$cpi)
      incProgress(2/2)
      Sys.sleep(0.25)
    }
  )
  
  
}
