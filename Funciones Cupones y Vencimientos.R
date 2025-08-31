# Funciones para obtener los cupones y vencimientos del día 
library(dplyr)
library(writexl)
library(readxl)
library(readr)
library(openxlsx)
library(stringr)
library(tidyr)
library(lubridate)
library(purrr)

# Imprimimos información
options(openxlsx.dateFormat = "dd/mm/yyyy")
options(openxlsx.datetimeFormat = "dd/mm/yyyy")
options(openxlsx.headerStyle = createStyle(textDecoration = "Bold"))

# --------------------------------- junta_px  ---------------------------------
# Función para unir la PX de t y t-1 con el fin de tomar la fecha de next cash flow más pequena 
junta_px <- function(px_t, px_t1){
  px <- px_t %>%
    select(ISIN,`Next Cash Flow Dt`) %>%
    dplyr::rename(`Next Cash Flow Dt t` = `Next Cash Flow Dt`) %>%
    full_join(px_t1 %>%
                select(ISIN,`Next Cash Flow Dt`) %>%
                dplyr::rename(`Next Cash Flow Dt t1` = `Next Cash Flow Dt`)) %>%
    mutate(`Next Cash Flow Dt` = case_when(is.na(`Next Cash Flow Dt t1`) & is.na(`Next Cash Flow Dt t`) ~ NA,
                                           is.na(`Next Cash Flow Dt t1`) ~ `Next Cash Flow Dt t`,
                                           !is.na(`Next Cash Flow Dt t1`) ~ `Next Cash Flow Dt t1`)) %>%
    
    select(ISIN,`Next Cash Flow Dt`)
  return(px)
}

# -------------------------------- completa_ic --------------------------------
# Función para llenar la ic, clasifica instrumentos, agrega fecha del siguiente cashflow,
# Convención del instrumento y calendario por convención. Y quita futuros
completa_ic <- function(insumos,datos,px){
  ic_cup_ven <- insumos$ic %>%
    mutate(Portafolio = 'GOI') %>%
    bind_rows(insumos$bmk %>%
                mutate(Portafolio = 'BMK')) %>%
    mutate(Divisa = substr(ISIN, 1, 2),
           Inst = word(Instrumento)) %>%
    left_join(px %>% 
                select(ISIN,`Next Cash Flow Dt`), 
              by = 'ISIN') %>%
    left_join(datos$catalogo,
              by = c('Inst', 'Divisa')) %>%
    left_join(datos$convenciones,
              by = 'Tipo de instrumento') %>%
    left_join(datos$divisas,
              by = 'Divisa') %>%
    filter(`Inst` != 'FUT' &
           !(Instrumento %in% c('JPY', 'USD', 'XAU')) &
             Vencimiento > date)
  
  return(ic_cup_ven)
}

# ----------------------------- obtiene_fecha_cupon ----------------------------
# Obtiene la fecha en que se va a reflejar el pago de cupón o vencimiento
# TOmando en cuenta el calendario del instrumento y su convención 
obtiene_fecha_cupon <- function(ic_cup_ven){
  cup_ven_dates <- ic_cup_ven %>%
    mutate(
      `Fecha Cup Aux` = case_when(
      `Tipo de instrumento` %in% c('TnotesNZD','TnotesAUD') ~ `Next Cash Flow Dt` - Ex_div_days,
      `Tipo de instrumento` == 'TnotesGBP' ~ dates_v2(hol,
                                                      `Next Cash Flow Dt`,
                                                      `Ex_div_days`,
                                                      "resta",
                                                      Calendario),
      TRUE ~ `Next Cash Flow Dt`),
      `Fecha Cupón` = dates_v2(hol,
                               `Fecha Cup Aux`,
                               `Convencion`,
                               "resta",
                               Calendario)) %>%
    mutate(`Fecha Cupón` = dates(hol,
                                 `Fecha Cupón`,
                                 1,
                                 "suma",
                                 'MX'),
           `Fecha Cupón` = dates(hol,
                                 `Fecha Cupón`,
                                 1,
                                 "resta",
                                 'MX'))
  
  return(cup_ven_dates)
  
}

# -------------------------------- obtiene_flujos ------------------------------
# Obtiene los flujos que se van a recibir en el día especificado 
obtiene_flujos <- function(cup_ven_dates){
  cup_ven_hoy <- cup_ven_dates %>%
    filter(`Fecha Cupón` == date)
  
  if(nrow(cup_ven_hoy)==0){
    
    cup_ven <- tibble(
      ISIN = NA,
      Instrumento = NA,
      Portafolio = NA,
      Principal = NA,
      Cupon = NA,
      Frecuencia = NA,
      `Tipo de instrumento` = NA,
      `Ex_div_days` = NA,
      `Settle Date` = NA,
      `Fecha Cupón` = NA,
      `Vencimiento BBG` = NA,
      `Cupón BBG` = NA
    ) 
    
  }else{
    # Obtenemos el face amount y el cashflow de BBG
    cup_ven_bbg <- cup_ven_hoy %>%
      mutate(
        `Face Amount` = bdp(
          securities = paste(ISIN, 'GOVT', sep = " "),
          fields = 'BQ_FACE_AMT'
        )[[1]],
        'Cupon BBG' = NA,
        'Vencimiento BBG' = NA
      )
    
    for (isin in cup_ven_bbg$ISIN) {
      fila <- which(cup_ven_bbg$ISIN == isin)
      
      isin_bbg <- paste(isin, 'GOVT', sep = " ")
      
      fecha <- cup_ven_bbg[fila, 'Next Cash Flow Dt'] %>%
        pull('Next Cash Flow Dt')
      fecha_rqst <- gsub('-', '', date)
      
      cupon <- bds(isin_bbg, 
                   "DES_CASH_FLOW", 
                   overrides = c("SETTLE_DT" = fecha_rqst)) %>%
        filter(`Payment Date` == fecha) %>%
        select(`Coupon Amount`) %>%
        pull(`Coupon Amount`)
      
      vencimiento <- bds(isin_bbg, 
                         "DES_CASH_FLOW", 
                         overrides = c("SETTLE_DT"= fecha_rqst)) %>%
        filter(`Payment Date` == fecha) %>%
        select(`Principal Amount`) %>%
        pull(`Principal Amount`)
      
      cup_ven_bbg[fila, 'Cupon BBG'] <- ifelse(length(cupon) == 0,
                                               0,
                                               cupon)
      
      cup_ven_bbg[fila, 'Vencimiento BBG'] <-ifelse(length(vencimiento) == 0,
                                                    0,
                                                    vencimiento)
    } 
    
    # Hacemos el cashflow proporcional al nocional que se tiene en cada instrumento 
    cup_ven <- cup_ven_bbg %>%
      mutate(`Cupón BBG` = (`Cupon BBG` / `Face Amount`) * Principal / 1000) %>%
      mutate(`Vencimiento BBG` = (`Vencimiento BBG` / `Face Amount`) * Principal / 1000)
      
    cup_ven <- cup_ven %>%
      select(
        ISIN,
        Instrumento,
        Portafolio,
        Principal,
        Cupon,
        Frecuencia,
        `Tipo de instrumento`,
        `Ex_div_days`,
        `Next Cash Flow Dt`,
        `Fecha Cupón`,
        `Vencimiento BBG`,
        `Cupón BBG`
      ) %>%
      filter(`Vencimiento BBG` != 0 | `Cupón BBG` != 0) %>%
      dplyr::rename(`Settle Date` = `Next Cash Flow Dt`)
    
  }
  return(cup_ven)
}

# -------------------------------- revisa_insumos ------------------------------
# Función para revisar si los archivos necesarios existen
revisa_insumos <- function(file_paths) {
  all(file.exists(file_paths))
}

# -------------------------- Juntamos BMK GOI y BMK CP6 ------------------------
lee_bmk_bmkcp6 <- function(name_bmk, name_bmk_cp6){
  bmk <- lee_bmk(name_bmk)
  
  if(file.exists(name_bmk_cp6)){
    bmk_cp6 <- lee_bmk(name_bmk_cp6)
    bmk <- bind_rows(bmk, bmk_cp6)
  }
  
  return(bmk)
}


# -------------------------------- genera_cup_ven ------------------------------
genera_cup_ven <- function(hol, date){
  date <<- date 
  date_ayer <- dates(hol, date,1,"resta",'MX')
  
  # Rutas ----
  rutas <- list(
    j = 'J:/outdir/reports/',
    px = 'Z:/II Responsabilidades/2 Inversiones/datos/precios/',
    global = 'Z:/II Responsabilidades/26 BMK Global/',
    futuros = 'J:/sysdir/import/precios/px_findur/',
    proyecto = 'Z:/II Responsabilidades/26 BMK Global/Inversiones/Proyecto RF/'
  )
  
  # Nombres archivos ----
  names <- list(
    ic = name_ic(rutas, date_ayer),
    
    cup_ven = name_cup_ven(rutas, date),
    
    datos = name_datos(rutas),
    
    bmk = list(
      hoy = name_bmk(rutas, date_ayer)
    ),
    bmk_cp6 = list(
      hoy = name_bmk_cp6(rutas, date_ayer)
    ),
    px = list(
      hoy = name_px(rutas, date),
      ayer = name_px(rutas, date_ayer)
    )
  )
  
  # Leemos archivos ----
  insumos <- list(ic = lee_ic(names$ic),
                  bmk = lee_bmk_bmkcp6(names$bmk$hoy,
                                       names$bmk_cp6$hoy),
                  px = list(
                    hoy = lee_px(names$px$hoy),
                    ayer = lee_px(names$px$ayer)
                  ))
  
  datos <- list(catalogo = read_excel(names$datos, sheet = 'Catalogo'),
                divisas = read_excel(names$datos, sheet = 'Divisas'),
                convenciones = read_excel(names$datos, sheet = 'Convenciones'))
  
  # Obtenemos cupones y vencimientos ----
  # Juntamos px de t y t-1
  px <- junta_px(insumos$px$hoy, insumos$px$ayer)
  
  # Clasificamos instrumentos y quitamos futuros 
  ic_cup_ven <- completa_ic(insumos, datos, px)
  
  # Calculamos la fecha en que vamos a recibir el flujo de cada cupón 
  cup_ven_dates <- obtiene_fecha_cupon(ic_cup_ven)
  
  # Separamos por GOI y BMK
  cup_ven_pt <- split(cup_ven_dates, f = cup_ven_dates$Portafolio)
  cup_ven <- lapply(cup_ven_pt, obtiene_flujos)
  
  # Imprimimos ----
  write_xlsx(cup_ven, names$cup_ven)
}
