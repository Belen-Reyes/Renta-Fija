# PAQUETERIAS ------------------------------------------------------------------
library(dplyr)
library(writexl)
library(readxl)
library(readr)
library(openxlsx)
library(stringr)
library(tidyr)

# Imprimimos información
options(openxlsx.dateFormat = "dd/mm/yyyy")
options(openxlsx.datetimeFormat = "dd/mm/yyyy")
options(openxlsx.headerStyle = createStyle(textDecoration = "Bold"))

# FUNCIONES FECHAS -------------------------------------------------------------

# ----------------------------------- dates -----------------------------------
# Regresa las fechas a t-n o t+n del día­a especificado
dates <- function(holidays, date, n, operador, pais) {
  
  holidays_1 <- na.omit(holidays[[pais]])
  
  calendar <- bizdays::create.calendar('calendar',
                                       holidays = holidays_1,
                                       weekdays = c("saturday", "sunday"),
                                       start.date = '2000-01-01',
                                       end.date = '2090-12-31')
  if (operador == "suma") {
    date <- bizdays::offset(date,+n, cal = calendar)
  } else{
    date <- bizdays::offset(date,-n, cal = calendar)
  }
  return(date)
}

# -------------------------------- dates_v2 --------------------------------
# Es una función para aplicar la función dates a un vector de paises y de n's 
dates_v2 <- function(holidays, date, n, operador, paises){
  holidays_1 <- lapply(holidays, na.omit)
  
  calendars <- lapply(holidays_1, function(h) {
    bizdays::create.calendar('calendar',
                             holidays = h,
                             weekdays = c("saturday", "sunday"),
                             end.date = '2090-12-31')
  })
  
  if(length(date) == 1){
    for(i in 1:length(paises)){
      
      if (operador == "suma") {
        date_1 <- bizdays::offset(date,+n[i], cal = calendars[[paises[i]]])
      } else{
        date_1 <- bizdays::offset(date,-n[i], cal = calendars[[paises[i]]])
      }
      
      if(i == 1){
        dates <- date_1
      }else{
        dates <- c(dates,date_1)
      }
    }
  }else{
    for(i in 1:length(paises)){
      if (operador == "suma") {
        date_1 <- bizdays::offset(date[i],+n[i], cal = calendars[[paises[i]]])
      } else{
        date_1 <- bizdays::offset(date[i],-n[i], cal = calendars[[paises[i]]])
      }
      
      if(i == 1){
        dates <- date_1
      }else{
        dates <- c(dates,date_1)
      }
    }
  }
  
  return(dates)
}

# FUNCIONES NOMBRES ------------------------------------------------------------

# -------------------------------- name_ic ------------------------------------
name_ic <- function(rutas, fecha){
  Sys.setlocale("LC_TIME", "English")
  ruta <- paste0(rutas$j, 
                 format(fecha, "%y%b%d"), 
                 '/',
                 'IC', 
                 format(fecha, "%Y%m%d"),
                 '.csv')
  return(ruta)
}

# ------------------------------ name_datos_inv ----------------------------------
name_datos_inv <- function(rutas, fecha){
  Sys.setlocale("LC_TIME", "English")
  ruta <- paste0(rutas$j, 
                 format(fecha, "%y%b%d"), 
                 '/',
                 'Datos_Inv_', 
                 format(fecha, "%Y%m%d"),
                 '_NEW.xls')
  return(ruta)
}

# ------------------------------ name_px ----------------------------------
name_px <- function(rutas, fecha){
  ruta <- paste0(rutas$px, 
                 'PX',
                 format(fecha, "%y%m%d"),
                 '.csv')
  return(ruta)
}

# -------------------------------- name_bmk ------------------------------------
name_bmk <- function(rutas, fecha){
  Sys.setlocale("LC_TIME", "English")
  ruta <- paste0(rutas$global, 
                 format(fecha, "%Y"), 
                 '/',
                 'BMK', 
                 '/',
                 format(fecha, "%Y%m"),
                 '/',
                 'BMK',
                 format(fecha, "%y%m"),
                 '.csv')
  return(ruta)
}

# -------------------------------- name_bmk_cp6 ------------------------------------
name_bmk_cp6 <- function(rutas, fecha){
  Sys.setlocale("LC_TIME", "English")
  ruta <- paste0(rutas$global, 
                 format(fecha, "%Y"), 
                 '/',
                 'BMK', 
                 '/',
                 format(fecha, "%Y%m"),
                 '/',
                 'BMKCP6',
                 format(fecha, "%y%m"),
                 '.csv')
  return(ruta)
}

# ---------------------------- name_findur_imp ---------------------------------
name_findur_imp <- function(rutas, fecha){
  Sys.setlocale("LC_TIME", "English")
  ruta <- paste0(rutas$futuros,
                 'findur', 
                 format(fecha, "%Y%m%d"),
                 '.imp')
  return(ruta)
}

# ------------------------------- name_ic_ctd ----------------------------------
name_ic_ctd <- function(rutas, fecha){
  Sys.setlocale("LC_TIME", "English")
  ruta <- paste0(rutas$proyecto,
                 'IC CTD Futuros/',
                 'IC_CTD', 
                 format(fecha, "%Y%m%d"),
                 '.xlsx')
  return(ruta)
}

# -------------------------------- name_bv ------------------------------------
name_bv <- function(rutas, fecha){
  Sys.setlocale("LC_TIME", "English")
  ruta <- paste0(rutas$j, 
                 format(fecha, "%y%b%d"), 
                 '/',
                 'BondValuation_NewMethod.INV BENCHMARK.csv')
  return(ruta)
}

# ------------------------------ name_val_ic -----------------------------------
name_val_ic <- function(rutas, fecha){
  ruta <- paste0(rutas$global,
                 'Global/GOI/Valua/',
                 'Val_IC',
                 format(fecha, "%Y%m%d"), 
                 '.csv')
  return(ruta)
}

# ------------------------------ name_val_bmk -----------------------------------
name_val_bmk <- function(rutas, fecha){
  ruta <- paste0(rutas$global,
                 'Global/BMK/Valua/',
                 'Val_BMK',
                 format(fecha, "%Y%m%d"), 
                 '.csv')
  
  return(ruta)
}

# ------------------------------ name_val_bmk_r -----------------------------------
name_val_bmk_r <- function(rutas, fecha){
  ruta <- paste0(rutas$global,
                 'Global/BMK/Valua/',
                 'Val_BMK',
                 format(fecha, "%Y%m%d"), 
                 '_Rebalanceo',
                 '.csv')
  
  return(ruta)
}

# ------------------------------ name_val_bmk_cp6 -----------------------------------
name_val_bmk_cp6 <- function(rutas, fecha){
  ruta <- paste0(rutas$global,
                 'Global/BMK/Valua/',
                 'Val_BMKCP6',
                 format(fecha, "%Y%m%d"), 
                 '.csv')
  
  return(ruta)
}

# ------------------------------ name_val_bmk_r_cp6 -----------------------------------
name_val_bmk_r_cp6 <- function(rutas, fecha){
  ruta <- paste0(rutas$global,
                 'Global/BMK/Valua/',
                 'Val_BMKCP6',
                 format(fecha, "%Y%m%d"), 
                 '_Rebalanceo',
                 '.csv')
  return(ruta)
}

# ------------------------------ name_res_ic -----------------------------------
name_res_ic <- function(rutas, fecha){
  ruta <- paste0(rutas$global,
                 'Global/GOI/Valua/',
                 'Resultados_',
                 format(fecha, "%Y%m%d"), 
                 '.csv')
  
  
  return(ruta)
}

# ------------------------------ name_res_bmk -----------------------------------
name_res_bmk <- function(rutas, fecha){
  ruta <- paste0(rutas$global,
                 'Global/BMK/Valua/',
                 'ResultadosBMK_',
                 format(fecha, "%Y%m%d"), 
                 '.csv')
  
  return(ruta)
}

# ------------------------------ name_res_bmk_cp6 -----------------------------------
name_res_bmk_cp6 <- function(rutas, fecha){
  ruta <- paste0(rutas$global,
                 'Global/BMK/Valua/',
                 'ResultadosBMKCP6_',
                 format(fecha, "%Y%m%d"), 
                 '.csv')
  return(ruta)
}

# -------------------------------- name_frn -------------------------------------
name_frn <- function(rutas){
  ruta <- paste0(rutas$proyecto,
                 'CPI y FRN/',
                 'Tasas FRN',
                 '.xlsx')
  return(ruta)
}

# -------------------------------- name_cpi -------------------------------------
name_cpi <- function(rutas){
  ruta <- paste0(rutas$proyecto,
                 'CPI y FRN/',
                 'CPI',
                 '.xlsx')
  return(ruta)
}

# -------------------------------- name_datos -------------------------------------
name_datos <- function(rutas){
  ruta <- paste0(rutas$proyecto,
                 'Datos',
                 '.xlsx')
  return(ruta)
}

# ------------------------------ name_hist_tc -----------------------------------
name_hist_tc <- function(rutas){
  ruta <- paste0(rutas$global,
                 'Cambios/archivos_rpt_tc/',
                 'historico_tc',
                 '.csv')
  return(ruta)
}

# -------------------------- names_plantilla_val --------------------------------
name_plantilla_val <- function(rutas){
  ruta <- paste0(rutas$proyecto,
                 'Valuaciones y Resultados/',
                 'Plantilla nombres',
                 '.xlsx')
  return(ruta)
}

# -------------------------- names_compara --------------------------------
name_compara <- function(rutas, fecha){
  ruta <- paste0(rutas$proyecto,
                 'Valuaciones y Resultados/Compara/',
                 'Compara ',
                 fecha,
                 '.xlsx')
  return(ruta)
}

# -------------------------- names_compara_p --------------------------------
name_compara_p <- function(rutas){
  ruta <- paste0(rutas$proyecto,
                 'Valuaciones y Resultados/Compara/',
                 'Plantilla Compara',
                 '.xlsx')
  return(ruta)
}

# ------------------------ names_plantilla_ic_ctd ------------------------------
names_plantilla_ic_ctd <- function(rutas){
  ruta <- paste0(rutas$proyecto,
                 'IC CTD Futuros/',
                 'Plantilla',
                 '.xlsx')
  return(ruta)
}

# -------------------------- name_cup_ven --------------------------------
name_cup_ven <- function(rutas, fecha){
  ruta <- paste0(rutas$proyecto,
                 'Cupones y Vencimientos/',
                 'Cupones y Vencimientos ',
                 fecha, 
                 '.xlsx')
  return(ruta)
}

# -------------------------- name_ic_copia --------------------------------
name_ic_copia <- function(rutas, fecha){
  ruta <- paste0(gsub('precios/','',rutas$px),
                 'IC', 
                 format(fecha, "%Y%m%d"),
                 '.csv')
  return(ruta)
}

# -------------------------- name_ic_ctd_copia --------------------------------
name_ic_ctd_copia <- function(rutas, fecha){
  ruta <- paste0(gsub('precios/','',rutas$px),
                 'IC_CTD', 
                 format(fecha, "%Y%m%d"),
                 '.xlsx')
  return(ruta)
}


# FUNCIONES LECTURA ARCHIVOS ---------------------------------------------------

# -------------------------------- lee_datos_inv -----------------------------------
lee_datos_inv <- function(names){
  datos <- loadWorkbook(names$datos_inv)
  datos <- list(
    comprasventas = read.xlsx(datos, sheet =  "ComprasVentas"),
    vencimientos = read.xlsx(datos, sheet =  "Vencimientos"),
    cupones = read.xlsx(datos, sheet =  "Cupones")
  )
  
  auxfun2 <- function(df) {
    df[is.na(df)] <- "" 
    # Agregar un sufijo único a los nombres de columna duplicados
    colnames(df) <- make.unique(colnames(df), sep = "_")
    return(df)
  }
  
  datos <- lapply(datos, auxfun2)
  datos <- lapply(datos, function(x) tibble(x))
  
  return(datos)
}

# -------------------------------- lee_px -----------------------------------
lee_px <- function(name_px){
  px <- read_csv(name_px, skip = 3) %>%
    mutate(
      `ULTIMO CUPON` = case_when(
        is.numeric(`ULTIMO CUPON`) ~ as.Date(`ULTIMO CUPON`, origin = "1899-12-30"),
        TRUE ~ as.Date(`ULTIMO CUPON`, format = '%d/%m/%Y')
      ),
      `Next Cash Flow Dt` = case_when(
        is.numeric(`Next Cash Flow Dt`) ~ as.Date(`Next Cash Flow Dt`, origin = "1899-12-30"),
        TRUE ~ as.Date(`Next Cash Flow Dt`, format = '%d/%m/%Y')
      ),
      ISSUE_DT = case_when(
        is.numeric(ISSUE_DT) ~ as.Date(ISSUE_DT, origin = "1899-12-30"),
        TRUE ~ as.Date(ISSUE_DT, format = '%d/%m/%Y')
      ),
      int_acc_dt = case_when(
        is.numeric(int_acc_dt) ~ as.Date(int_acc_dt, origin = "1899-12-30"),
        TRUE ~ as.Date(int_acc_dt, format = '%d/%m/%Y')
      )
    )
  return(px)

}

# -------------------------------- lee_ic -----------------------------------
lee_ic <- function(name_ic){
  ic <- read_csv(name_ic, skip = 2) %>%
    mutate(
      Vencimiento = case_when(
      is.numeric(Vencimiento) ~ as.Date(Vencimiento, origin = "1899-12-30"),
      TRUE ~ as.Date(Vencimiento, format = '%d/%m/%Y'))
    )
  return(ic)
  
}

# -------------------------------- lee_bmk -----------------------------------
lee_bmk <- function(name_bmk){
  bmk <- read_csv(name_bmk, skip = 2) %>%
    mutate(
      MATURITY = case_when(
        is.numeric(MATURITY) ~ as.Date(MATURITY, origin = "1899-12-30"),
        TRUE ~ as.Date(MATURITY, format = '%d/%m/%Y'))
    ) %>%
    dplyr::rename(Vencimiento = MATURITY,
                  Frecuencia = CPN_FREQ) %>%
    select(ISIN, Instrumento, Principal, Cupon, Frecuencia, Vencimiento) %>%
    filter(nchar(ISIN)>3)
  
  return(bmk)
}


# -------------------------------- lee_findur_imp -----------------------------------

lee_findur_imp <- function(names){
  futuros_px_pre <- read.table(names$futuros, 
                               header = TRUE, 
                               sep = "")
  
  encabezado <-"HDR,IDX,index_name,gridpoint_name,val_set,val1,val2,date_match,field_name"
  
  n <- which(futuros_px_pre[,1] == encabezado)[1]
  
  futuros_px <- read.table(names$futuros, 
                           skip = n, 
                           header = TRUE, 
                           sep = ",")
  
  futuros_px <- as_tibble(futuros_px) %>%
    select(gridpoint_name, val1) %>%
    mutate(val1 = as.numeric(val1))
  
  names(futuros_px) <- c('ISIN', 'PX_BID')
  
  return(futuros_px)
}

# -------------------------------- lee_bv -----------------------------------
lee_bv <- function(names){
  bv <- read_csv(names$bv, skip = 1) %>%
    group_by(ISIN) %>%
    summarize(`Valor de Mercado BV` = sum(`Today Value`),
              `Intereses BV` = sum(`Today Accrued Int`),
              `Monto a Recibir BV` = sum(MTM)) %>%
    ungroup()
  
  return(bv)
}

# ------------------------------ lee_hist_tc -----------------------------------
lee_hist_tc <- function(names, date, div_dir){
  hist_tc <- read_csv(names$hist_tc) %>%
    filter(Fecha == date) %>%
    pivot_longer(-Fecha, names_to = 'Divisa Final', values_to = 'TC') %>%
    select(-Fecha) %>%
    mutate(TC = if_else(`Divisa Final` %in% div_dir,
                        1/TC, 
                        TC))
  
  return(hist_tc)
}

# FUNCIONES FRN ----------------------------------------------------------------

# ------------------------------- obtiene_tcpd ---------------------------------
# Función para obtener la tasa cupón promedio diaria de los FRN
obtiene_tcpd <- Vectorize(
  
  obtiene_tcpd <- function(isin, fecha_valor, ultimo_cupon, issue_dt) {
    if (fecha_valor <= issue_dt) {
      tcpd <- 0
    } else{
      fecha_1 <- if_else(fecha_valor == ultimo_cupon,
                         fecha_valor + 1,
                         fecha_valor)
      fecha_2 <- ultimo_cupon
      
      tcpd_tibble <- tasas_frn %>% 
        select(c('Fecha', isin)) %>%
        filter(Fecha < fecha_1 & Fecha >= fecha_2) %>%
        summarise(tcpd = mean(!!sym(isin)))
      
      tcpd <- tcpd_tibble$tcpd
    }
})

# FUNCIONES TIPS ----------------------------------------------------------------

# --------------------------- obtiene_ajuste_cpi -------------------------------
# Obtiene el ajuste de inflación para los TIPS
obtiene_ajuste_cpi <- function(ic, cpi){
  cpi_tips <- cpi %>%
    pivot_longer(-Fecha, names_to = 'Divisa', values_to  = 'CPI')
  
  ic_tips_aj <- ic %>%
    left_join(cpi_tips %>%
                dplyr::rename(`Fecha Valor` = Fecha, 
                              `CPI FV` = CPI), by = c('Fecha Valor', 'Divisa')) %>%
    left_join(cpi_tips %>%
                dplyr::rename(`int_acc_dt` = Fecha, 
                              `CPI IAD` = CPI), by = c('int_acc_dt', 'Divisa')) %>%
    mutate(`Ajuste CPI` = round2(truncate(`CPI FV`/`CPI IAD`,6),5))
  
  return(ic_tips_aj)
}

# FUNCIONES GENERALES VALUACIÓN ------------------------------------------------ 
# -------------------------- obtiene_dias_inter --------------------------------
obtiene_dias_inter <- Vectorize(
  
  obtiene_dias_inter <- function(tipo_inst, freq, siguiente_cpn, ultimo_cpn){
    
    if (tipo_inst %in% c('ABS', 'AgenR')) {
      dias_inter <- (12 / freq) * 30
    } else{
      dias_inter <- as.numeric(siguiente_cpn - ultimo_cpn)
    }
    return(dias_inter)
  }
)

# ----------------------------- precio_tasa -----------------------------------
# Función para pasar de tasa a precio en la PX
precio_tasa <- Vectorize(
  
  precio_tasa <- function(tipo_inst, dias_por_vencer, tasa){
    
    if (tipo_inst == 'Tbills') {
      precio <- round2((1-tasa*dias_por_vencer/36000)*100,8)
    } else{
      if(tipo_inst == 'TbillsJPY'){
        precio <- round2((1-tasa*dias_por_vencer/36500)*100,4)
      }else{
        if(tipo_inst == 'billsSGD'){
          precio <- round2((1-tasa*dias_por_vencer/36500)*100,3)
        }else{
          precio <- tasa
        }
      }
    }
    return(precio)
  }
)

# ----------------------------- sector_v -----------------------------------
# Función para obtener el sector de vencimiento
sector_v <- Vectorize(
  
  sector_v <- function(fecha, fecha_vencimiento){
    
    tiempo <- as.numeric(fecha_vencimiento - fecha)/365
    
    sector <- case_when(tiempo <= 0.25 ~ 'S1(0-3m)',
                        tiempo > 0.25 & tiempo <= 1 ~ 'S2(3m-1y)',
                        tiempo > 1 & tiempo <= 3 ~ 'S3(1-3y)',
                        tiempo > 3 & tiempo <= 5 ~ 'S4(3-5y)',
                        tiempo > 5 & tiempo <= 10 ~ 'S5(5-10y)',
                        tiempo > 10 ~ 'S6(>10y)'
                        )

    return(sector)
  }
)

# ----------------------------- obtiene_iden -----------------------------------
# Función para obtener el identificador de los instrumentos
obtiene_iden <- Vectorize(
  
  obtiene_iden <- function(isin, instrumento, futuro){
    
    identificador <- case_when(grepl('FUT', instrumento) ~ futuro,
                               substr(isin,1,2) %in% c('US', 'CA') ~ substr(isin,3,11),
                               TRUE ~ isin)
    return(identificador)
  }
)
 # --------------------------- obtiene_udma ------------------------------------
# Función para obtener el ultimo día del mes anterior a la fecha del proceso 
obtiene_udma <- Vectorize(
  
  udma <- function(date){
    
    primer_dia_mes <- ceiling_date(date %m-% months(1), 'months')
    udma <- dates(hol, primer_dia_mes, 1, "resta", 'MX')
    
    
    return(udma)
  }
)

# FUNCIONES VALOR DE MERCADO ---------------------------------------------------
obtiene_vm <- list(
  
  # General ----
  # Función para obtener el valor de mercado 
  general = Vectorize(
    
    general <- function(fecha_valor, fecha_vto, valor_nominal, precio){
      
      if(fecha_valor >= fecha_vto) {
        valor_mercado <- 0
      } else{
        if (fecha_valor == fecha_vto) {
          valor_mercado <- valor_nominal
        } else{
          valor_mercado <- valor_nominal * precio / 100
        }
      }
      return(valor_mercado)
    }
    ),
  
  # TIPS ----
  tips = Vectorize(
    
    tips <- function(fecha_valor, fecha_vto, valor_nominal, precio, aj_cpi){
      
      if(fecha_valor > fecha_vto) {
        valor_mercado <- 0
      } else{
        if (fecha_valor == fecha_vto) {
          valor_mercado <- valor_nominal * aj_cpi
        } else{
          valor_mercado <- valor_nominal * precio / 100 * aj_cpi
        }
      }
      return(valor_mercado)
    }
  )
)

# FUNCIONES INTERESES ----------------------------------------------------------

obtiene_int <- list(
  
  # AgenR ----
  # Función  para obtener intereses de las agencias a rendimiento
  agenr = Vectorize(
    
    agenr <- function(fecha_valor, fecha_vto, valor_nominal,cupon,freq,dias_acc,dias_inter, decimales) {
      
      if (dias_acc == dias_inter | dias_inter == 0 | fecha_valor >= fecha_vto) {
        int_agenr <- 0
      } else{
        int_agenr <- if_else(is.na(decimales), 
                             valor_nominal * (cupon / freq) / 100 * (dias_acc / dias_inter),
                             valor_nominal * round2(cupon / freq / 100, decimales) * (dias_acc / dias_inter)
                             )
      }
      return(int_agenr)
    }
  ),
  
  # AgenRCAD ----
  # Función  para obtener intereses de las agencias a rendimiento canadienses
  agenrcad = Vectorize(
    
    agenrcad <- function(fecha_valor, fecha_vto, valor_nominal,cupon,freq,dias_acc,dias_inter) {
      
      if (dias_acc == dias_inter | dias_inter == 0 | fecha_valor >= fecha_vto) {
        int_agenrcad <- 0
      } else{
        int_agenrcad <- if_else(dias_acc < 182.5, 
                                valor_nominal * (cupon / freq) / 100 * dias_acc * 2 / 365,
                                valor_nominal * (cupon / freq) / 100 * (1 + (182 - dias_acc) / 182))
      }
      return(int_agenrcad)
    }
  ),
  
  # FRNs ----
  # Función para obtener intereses de los frn 
  frn = Vectorize(
    frn <- function(fecha_valor, fecha_vto, valor_nominal, tcpd, dias_acc, dias_inter) {
      
      if(dias_acc == dias_inter | dias_inter == 0 | fecha_valor >= fecha_vto){
        int_frn <- 0
      } else{
        int_frn <- valor_nominal * tcpd * dias_acc / 100
      }
      return(int_frn)
    }
  ),
  
  # General ----
  # Función general para obtener intereses de instrumentos cuponados
  general = Vectorize(
    
    general <- function(fecha_valor, fecha_vto, valor_nominal,cupon,freq,dias_acc,dias_inter) {
      
      if (dias_acc == dias_inter | dias_inter == 0 | fecha_valor >= fecha_vto) {
        int <- 0
      } else{
        int <- valor_nominal * (cupon / freq) * (dias_acc / dias_inter) / 100
      }
      return(int)
    }
  ),
  
  # TIPS ----
  # Función  para obtener intereses de los tips
  tips = Vectorize(
    
    tips <- function(fecha_valor, fecha_vto, valor_nominal,cupon,freq,dias_acc,dias_inter, aj_cpi) {
      
      if (dias_acc == dias_inter | dias_inter == 0 | fecha_valor >= fecha_vto) {
        int_tips <- 0
      } else{
        int_tips <- valor_nominal * (cupon / freq) * (dias_acc / dias_inter) / 100 * aj_cpi
      }
      return(int_tips)
    }
  ),
  
  # TnotesAUD ----
  # Función  para obtener intereses de las notas australianas
  tnotesaud = Vectorize(
    
    tnotesaud <- function(fecha_valor, fecha_vto, valor_nominal,cupon,freq,dias_acc,dias_inter, aj_cpi) {
      
      if (dias_acc == dias_inter | dias_inter == 0 | fecha_valor >= fecha_vto) {
        int_tnotesaud <- 0
      } else{
        int_tnotesaud <- valor_nominal * round2((cupon / freq) * (dias_acc / dias_inter), 3) / 100
      }
      return(int_tnotesaud)
    }
  ),
  
  # TnotesCAD ----
  # Función  para obtener intereses de las notas canadienses
  tnotescad = Vectorize(
    
    tnotescad <- function(fecha_valor, fecha_vto, valor_nominal,cupon,freq,dias_acc,dias_inter, aj_cpi) {
      
      if (dias_acc == dias_inter | dias_inter == 0 | fecha_valor >= fecha_vto) {
        int_tnotescad <- 0
      } else{
        int_tnotescad <- if_else(dias_acc < 182.5,
                                 valor_nominal * (cupon / freq) / 100 * dias_acc * 2 / 365,
                                 valor_nominal * (cupon / freq) / 100 * (1 + (182 - dias_acc) * 2 / 365)
                                 )
      }
      return(int_tnotescad)
    }
  ),
  
  # TnotesGER ----
  # Función  para obtener intereses de las notas alemanas
  tnotesger = Vectorize(
    
    tnotesger <- function(fecha_valor, fecha_vto, valor_nominal,cupon,freq,dias_acc,dias_inter, prox_cpn, ult_cpn) {
      
      if (dias_acc == dias_inter | dias_inter == 0 | fecha_valor >= fecha_vto) {
        int_tnotesger <- 0
      } else{
        
        prox_cpn_1ano <- prox_cpn %m-% years(1) 
        prox_cpn_2anos <- prox_cpn %m-% years(2) 
        
        dias_acc_2anos <- as.numeric(prox_cpn_1ano-ult_cpn)
        dias_inter_2anos <- as.numeric(prox_cpn_1ano-prox_cpn_2anos)
        
        dias_acc_1ano <- (dias_acc-dias_acc_2anos)
        dias_inter_1ano <- as.numeric(prox_cpn-prox_cpn_1ano)
        
        ratio_1ano <- dias_acc_2anos/dias_inter_2anos
        ratio2anos <- dias_acc_1ano/dias_inter_1ano
        
        int_tnotesger <- if_else(prox_cpn_1ano == ult_cpn,
                                 valor_nominal * (cupon / freq) / 100 * dias_acc / dias_inter,
                                 valor_nominal * (cupon / freq) / 100 * (ratio_1ano + ratio2anos)
        )
      }
      return(int_tnotesger)
    }
  ),
  
  # TnotesJPY ----
  # Función  para obtener intereses de las notas japonesas
  tnotesjpy = Vectorize(
    
    tnotesjpy <- function(fecha_valor, fecha_vto, valor_nominal,cupon,freq,dias_acc,dias_inter, aj_cpi) {
      
      if (dias_acc == dias_inter | dias_inter == 0 | fecha_valor >= fecha_vto) {
        int_tnotesjpy <- 0
      } else{
        int_tnotesjpy <- truncate(valor_nominal * truncate((cupon / 100) * (dias_acc / 365), 9),0)
      }
      return(int_tnotesjpy)
    }
  )
  
)

# FUNCIONES VALUACION ----------------------------------------------------------

valua <- list(
  
  # Instrumentos AUD ----
  TbillsAUD =  function(ic){
    
    tbillsaud_val <- ic %>%
      mutate(
        `Valor de Mercado` = obtiene_vm[['general']](`Fecha Valor`,
                                                     Vencimiento,
                                                     Principal,
                                                     PX_BID), 
        
        `Intereses` = 0) 
    
    return(tbillsaud_val)
  },
  
  TnotesAUD =  function(ic){
    
    tnotesaud_val <- ic %>%
      mutate(
        `Valor de Mercado` = obtiene_vm[['general']](`Fecha Valor`,
                                                     Vencimiento,
                                                     Principal,
                                                     PX_BID), 
        
        `Intereses` = obtiene_int[['tnotesaud']](`Fecha Valor`,
                                                 Vencimiento,
                                                 Principal,
                                                 Cupon,
                                                 Frecuencia,
                                                 DAYS_ACC,
                                                 `Dias intercupones`)) 
    
    return(tnotesaud_val)
  },
  
  # Instrumentos CAD ----
  AgenCAD =  function(ic){
    
    agencad_val <- ic %>%
      mutate(
        `Valor de Mercado` = obtiene_vm[['general']](`Fecha Valor`,
                                                     Vencimiento,
                                                     Principal,
                                                     PX_BID), 
        
        `Intereses` = 0) 
    
    return(agencad_val)
  },
  
  AgenRCAD =  function(ic){
    
    agenrcad_val <- ic %>%
      mutate(
        `Valor de Mercado` = obtiene_vm[['general']](`Fecha Valor`,
                                                     Vencimiento,
                                                     Principal,
                                                     PX_BID), 
        
        `Intereses` = obtiene_int[['agenrcad']](`Fecha Valor`,
                                                Vencimiento,
                                                Principal,
                                                Cupon,
                                                Frecuencia,
                                                DAYS_ACC,
                                                `Dias intercupones`)) 
    
    return(agenrcad_val)
  },
  
  TbillsCAD =  function(ic){
    
    tbillscad_val <- ic %>%
      mutate(
        `Valor de Mercado` = obtiene_vm[['general']](`Fecha Valor`,
                                                     Vencimiento,
                                                     Principal,
                                                     PX_BID), 
        
        `Intereses` = 0) 
    
    return(tbillscad_val)
  },
  
  TnotesCAD =  function(ic){
    
    tnotescad_val <- ic %>%
      mutate(
        `Valor de Mercado` = obtiene_vm[['general']](`Fecha Valor`,
                                                     Vencimiento,
                                                     Principal,
                                                     PX_BID), 
        
        `Intereses` = obtiene_int[['tnotescad']](`Fecha Valor`,
                                                 Vencimiento,
                                                 Principal,
                                                 Cupon,
                                                 Frecuencia,
                                                 DAYS_ACC,
                                                 `Dias intercupones`)) 
    
    return(tnotescad_val)
  },
  
  # Instrumentos CNY ----
  TnotesCNY =  function(ic){
    
    tnotescny_val <- ic %>%
      mutate(
        `Valor de Mercado` = obtiene_vm[['general']](`Fecha Valor`,
                                                     Vencimiento,
                                                     Principal,
                                                     PX_BID), 
        
        `Intereses` = obtiene_int[['general']](`Fecha Valor`,
                                                 Vencimiento,
                                                 Principal,
                                                 Cupon,
                                                 Frecuencia,
                                                 DAYS_ACC,
                                                 `Dias intercupones`)) 
    
    return(tnotescny_val)
  },
  
  # Instrumentos EUR ----
  AgenREUR =  function(ic){
    
    agenreur_val <- ic %>%
      mutate(
        `Valor de Mercado` = obtiene_vm[['general']](`Fecha Valor`,
                                                     Vencimiento,
                                                     Principal,
                                                     PX_BID), 
        
        `Intereses` = obtiene_int[['general']](`Fecha Valor`,
                                               Vencimiento,
                                               Principal,
                                               Cupon,
                                               Frecuencia,
                                               DAYS_ACC,
                                               `Dias intercupones`)) 
    
    return(agenreur_val)
  },

  # Instrumentos FRA ----
  TbillsFRA =  function(ic){
    
    tbillsfra_val <- ic %>%
      mutate(
        `Valor de Mercado` = obtiene_vm[['general']](`Fecha Valor`,
                                                     Vencimiento,
                                                     Principal,
                                                     PX_BID), 
        
        `Intereses` = 0) 
    
    return(tbillsfra_val)
  },
  
  TnotesFRA =  function(ic){
    
    tnotesfra_val <- ic %>%
      mutate(
        `Valor de Mercado` = obtiene_vm[['general']](`Fecha Valor`,
                                                     Vencimiento,
                                                     Principal,
                                                     PX_BID), 
        
        `Intereses` = obtiene_int[['general']](`Fecha Valor`,
                                               Vencimiento,
                                               Principal,
                                               Cupon,
                                               Frecuencia,
                                               DAYS_ACC,
                                               `Dias intercupones`)) 
    
    return(tnotesfra_val)
  },
  
  # Instrumentos GBP ----
  TbillsGBP =  function(ic){
    
    tbillsgbp_val <- ic %>%
      mutate(
        `Valor de Mercado` = obtiene_vm[['general']](`Fecha Valor`,
                                                     Vencimiento,
                                                     Principal,
                                                     PX_BID), 
        
        `Intereses` = 0) 
    
    return(tbillsgbp_val)
  },
  
  TnotesGBP =  function(ic){
    
    tnotesgbp_val <- ic %>%
      mutate(
        `Valor de Mercado` = obtiene_vm[['general']](`Fecha Valor`,
                                                     Vencimiento,
                                                     Principal,
                                                     PX_BID), 
        
        `Intereses` = obtiene_int[['general']](`Fecha Valor`,
                                               Vencimiento,
                                               Principal,
                                               Cupon,
                                               Frecuencia,
                                               DAYS_ACC,
                                               `Dias intercupones`)) 
    
    return(tnotesgbp_val)
  },
  
  TIPSGBP = function(ic){
    
    tipsgbp_val <- obtiene_ajuste_cpi(ic, insumos$cpi) %>%
      mutate(
        `Valor de Mercado` = obtiene_vm[['tips']](`Fecha Valor`,
                                                  Vencimiento,
                                                  Principal,
                                                  PX_BID,
                                                  `Ajuste CPI`),
        
        `Intereses` = obtiene_int[['tips']](`Fecha Valor`,
                                            Vencimiento,
                                            Principal,
                                            Cupon,
                                            Frecuencia,
                                            DAYS_ACC,
                                            `Dias intercupones`,
                                            `Ajuste CPI`)
      ) %>% 
      select(-`Ajuste CPI`)
    
    return(tipsgbp_val)
  },
  
  
  # Instrumentos GER ----
  TbillsGER =  function(ic){
    
    tbillsger_val <- ic %>%
      mutate(
        `Valor de Mercado` = obtiene_vm[['general']](`Fecha Valor`,
                                                     Vencimiento,
                                                     Principal,
                                                     PX_BID), 
        
        `Intereses` = 0) 
    
    return(tbillsger_val)
  },
  
  TnotesGER =  function(ic){
    
    tnotesger_val <- ic %>%
      mutate(
        `Valor de Mercado` = obtiene_vm[['general']](`Fecha Valor`,
                                                     Vencimiento,
                                                     Principal,
                                                     PX_BID), 
        
        `Intereses` = obtiene_int[['tnotesger']](`Fecha Valor`,
                                                 Vencimiento,
                                                 Principal,
                                                 Cupon,
                                                 Frecuencia,
                                                 DAYS_ACC,
                                                 `Dias intercupones`,
                                                 `Next Cash Flow Dt`,
                                                 `ULTIMO CUPON`)) 
    
    return(tnotesger_val)
  },
  
  # Instrumentos JPY ----
  TbillsJPY =  function(ic){
    
    tbillsjpy_val <- ic %>%
      mutate(
        `Valor de Mercado` = obtiene_vm[['general']](`Fecha Valor`,
                                                     Vencimiento,
                                                     Principal,
                                                     PX_BID), 
        
        `Intereses` = 0) 
    
    return(tbillsjpy_val)
  },
  
  TnotesJPY =  function(ic){
    
    tnotesjpy_val <- ic %>%
      mutate(
        `Valor de Mercado` = obtiene_vm[['general']](`Fecha Valor`,
                                                     Vencimiento,
                                                     Principal,
                                                     PX_BID), 
        
        `Intereses` = obtiene_int[['tnotesjpy']](`Fecha Valor`,
                                                 Vencimiento,
                                                 Principal,
                                                 Cupon,
                                                 Frecuencia,
                                                 DAYS_ACC,
                                                 `Dias intercupones`)) 
    
    return(tnotesjpy_val)
  },
  
  
  # Instrumentos NZD ----
  TbillsNZD =  function(ic){
    
    tbillsnzd_val <- ic %>%
      mutate(
        `Valor de Mercado` = obtiene_vm[['general']](`Fecha Valor`,
                                                     Vencimiento,
                                                     Principal,
                                                     PX_BID), 
        
        `Intereses` = 0) 
    
    return(tbillsnzd_val)
  },
  
  TnotesNZD =  function(ic){
    
    tnotesnzd_val <- ic %>%
      mutate(
        `Valor de Mercado` = obtiene_vm[['general']](`Fecha Valor`,
                                                     Vencimiento,
                                                     Principal,
                                                     PX_BID), 
        
        `Intereses` = obtiene_int[['general']](`Fecha Valor`,
                                                 Vencimiento,
                                                 Principal,
                                                 Cupon,
                                                 Frecuencia,
                                                 DAYS_ACC,
                                                 `Dias intercupones`)) 
    
    return(tnotesnzd_val)
  },
  
  # Instrumentos SGD ----
  billsSGD =  function(ic){
    
    billssgd_val <- ic %>%
      mutate(
        `Valor de Mercado` = obtiene_vm[['general']](`Fecha Valor`,
                                                     Vencimiento,
                                                     Principal,
                                                     PX_BID), 
        
        `Intereses` = 0) 
    
    return(billssgd_val)
  },

  # Instrumentos USD ----
  ABS =  function(ic){
    
    abs_val <- ic %>%
      mutate(
        `Valor de Mercado` = obtiene_vm[['general']](`Fecha Valor`,
                                                     Vencimiento,
                                                     Principal,
                                                     PX_BID), 
        
        `Intereses` = obtiene_int[['general']](`Fecha Valor`,
                                               Vencimiento,
                                               Principal,
                                               Cupon,
                                               Frecuencia,
                                               DAYS_ACC,
                                               `Dias intercupones`)) 
    
    return(abs_val)
  },
  
  Agen =  function(ic){
    
    agen_val <- ic %>%
      mutate(
        `Valor de Mercado` = obtiene_vm[['general']](`Fecha Valor`,
                                                     Vencimiento,
                                                     Principal,
                                                     PX_BID), 
        
        `Intereses` = 0) 
    
    return(agen_val)
  },
  
  AgenR =  function(ic){
    
    agenr_val <- ic %>%
      mutate(
        `Valor de Mercado` = obtiene_vm[['general']](`Fecha Valor`,
                                                     Vencimiento,
                                                     Principal,
                                                     PX_BID), 
        
        `Intereses` = obtiene_int[['agenr']](`Fecha Valor`,
                                             Vencimiento,
                                             Principal,
                                             Cupon,
                                             Frecuencia,
                                             DAYS_ACC,
                                             `Dias intercupones`,
                                             Decimales)) 
    
    return(agenr_val)
  },
  
  FRNs = function(ic){
    
    frn_val <- ic %>%
      mutate(
        TCPD = obtiene_tcpd(ISIN,
                            `Fecha Valor`,
                            `ULTIMO CUPON`,
                            ISSUE_DT),
        
        `Valor de Mercado` = obtiene_vm[['general']](`Fecha Valor`,
                                                     Vencimiento,
                                                     Principal,
                                                     PX_BID),
        
        `Intereses` = obtiene_int[['frn']](`Fecha Valor`,
                                           Vencimiento,
                                           Principal,
                                           TCPD,
                                           DAYS_ACC,
                                           `Dias intercupones`)
      ) %>% 
      select(-TCPD)
    
    return(frn_val)
  },
  
  MTI =  function(ic){
    
    mti_val <- ic %>%
      mutate(
        `Valor de Mercado` = obtiene_vm[['general']](`Fecha Valor`,
                                                     Vencimiento,
                                                     Principal,
                                                     PX_BID), 
        
        `Intereses` = obtiene_int[['general']](`Fecha Valor`,
                                               Vencimiento,
                                               Principal,
                                               Cupon,
                                               Frecuencia,
                                               DAYS_ACC,
                                               `Dias intercupones`)) 
    
    return(mti_val)
  },
  
  Tbills =  function(ic){
    
    tbills_val <- ic %>%
      mutate(
        `Valor de Mercado` = obtiene_vm[['general']](`Fecha Valor`,
                                                     Vencimiento,
                                                     Principal,
                                                     PX_BID), 
        
        `Intereses` = 0) 
    
    return(tbills_val)
  },
  
  TIPS = function(ic){
    
    tips_val <- obtiene_ajuste_cpi(ic, insumos$cpi) %>%
      mutate(
        `Valor de Mercado` = obtiene_vm[['tips']](`Fecha Valor`,
                                                  Vencimiento,
                                                  Principal,
                                                  PX_BID,
                                                  `Ajuste CPI`),
        
        `Intereses` = obtiene_int[['tips']](`Fecha Valor`,
                                            Vencimiento,
                                            Principal,
                                            Cupon,
                                            Frecuencia,
                                            DAYS_ACC,
                                            `Dias intercupones`,
                                            `Ajuste CPI`)
      ) %>% 
      select(-`Ajuste CPI`)
    
    return(tips_val)
  },
  
  Tnotes =  function(ic){
    
    notes_val <- ic %>%
      mutate(
        `Valor de Mercado` = obtiene_vm[['general']](`Fecha Valor`,
                                                     Vencimiento,
                                                     Principal,
                                                     PX_BID), 
        
        `Intereses` = obtiene_int[['general']](`Fecha Valor`,
                                               Vencimiento,
                                               Principal,
                                               Cupon,
                                               Frecuencia,
                                               DAYS_ACC,
                                               `Dias intercupones`)) 
    
    return(notes_val)
  }
)

# FUNCIONES FUTUROS ----
futuros <- list(
  obtiene_vm_fut = Vectorize(
    
    obtiene_vm_fut <- function(valor_nominal, precio, value_1pt){
      precio_aux <- if_else(precio < 3, 
                            precio*100,
                            precio)
      
      valor_mercado <- valor_nominal * round2(precio_aux*value_1pt,2)
      
      return(valor_mercado)
    }
  ),
  
  valua_fut =  function(ic, date){
    
    fut_val <- ic %>%
      mutate(`Monto a Recibir` = futuros[['obtiene_vm_fut']](Principal,
                                                             PX_BID,
                                                             `Value of 1 pt`),
             `Fecha Valor` = date)
    
    return(fut_val)
  },
  
  obtiene_ctd = Vectorize(
    
    obtiene_ctd <- function(isin){
      isin_ctd <- bdp(paste0(isin, ' Comdty'), 'FUT_CTD_ISIN')[[1]]
      return(isin_ctd)
    }
  ),
  
  obtiene_instrumento = Vectorize(
    
    obtiene_instrumento <- function(isin){
      
      inst_ctd <- bdp(paste0(isin, ' Comdty'), 'FUT_CTD')[[1]]
      return(inst_ctd)
    }
  ),
  
  obtiene_conv_fac = Vectorize(
    
    obtiene_conv_fac <- function(isin){
      
      conv_factor <- bdp(paste0(isin, ' Comdty'), 'FUT_CNVS_FACTOR')[[1]]
      return(conv_factor)
    }
  ),
  
  obtiene_maturity = Vectorize(
    
    obtiene_maturity <- function(isin_ctd){
      
      mat_ctd <- bdp(paste0(isin_ctd, ' GOVT'), 'MATURITY')[[1]]
      return(as.Date(mat_ctd))
    }
  ),
  
  obtiene_cupon = Vectorize(
    
    obtiene_cupon <- function(isin_ctd){
      
      cupon_ctd <- bdp(paste0(isin_ctd, ' GOVT'), 'CPN')[[1]]
      return(cupon_ctd)
    }
  ),
  
  obtiene_freq = Vectorize(
    
    obtiene_freq <- function(isin_ctd){
      
      freq_ctd <- bdp(paste0(isin_ctd, ' GOVT'), 'CPN_FREQ')[[1]]
      return(freq_ctd)
    }
  )
)


# ---------------------------- imprime_fecha_ctd -----------------------------
imprime_fecha_ctd <- function(date, wb, name){
  writeData(
    wb = wb,
    sheet = "IC_CTD",
    date,
    startCol = 1,
    startRow = 2,
    colNames = F
  )
  
  saveWorkbook(wb, name, overwrite = T)
}

# ---------------------------- imprime_ic_ctd -----------------------------

imprime_ic_ctd <- function(date, wb, ic, name){
  
  writeData(
    wb = wb,
    sheet = "IC_CTD",
    date,
    startCol = 1,
    startRow = 2,
    colNames = F
  )
  
  writeData(
    wb = wb,
    sheet = "IC_CTD",
    ic,
    startCol = 1,
    startRow = 4,
    colNames = F
  )
  
  saveWorkbook(wb, name, overwrite = T)
}

# VALUACIONES -------
# --------------------------------- valua_rf -----------------------------------
valua_rf <- function(ic, datos, insumos, px, date){
  insumos <<- insumos 
  # Clasificamos los instrumentos, anadimos su catálogo de 
  ic_clas_px <-  ic %>%
    mutate(Vencimiento = as.Date(Vencimiento, format = '%d/%m/%Y'),
           Divisa = substr(ISIN, 1, 2),
           Inst = word(Instrumento)) %>% 
    left_join(datos$catalogo,
              by = c('Inst', 'Divisa')) %>%
    left_join(datos$divisas,
              by = 'Divisa') %>%
    left_join(datos$convenciones,
              by = 'Tipo de instrumento') %>%
    left_join(datos$redondeo,
              by = 'ISIN') %>%
    left_join(px %>%
                select(ISIN, 
                       PX_BID, 
                       `ULTIMO CUPON`, 
                       `Next Cash Flow Dt`, 
                       DAYS_ACC, 
                       ISSUE_DT,
                       int_acc_dt,
                       `Dur Adj Bid`,
                       `Yld Ytm Bid`,
                       `Cnvx Bid`), by = 'ISIN')
  
  # Obtenemos fecha valor, días intercupones y días por vencer
  ic_fv <- ic_clas_px %>%
    mutate(`Fecha Valor` = dates_v2(hol, date, Convencion, "suma", Calendario),
           `ULTIMO CUPON` = case_when(is.na(`ULTIMO CUPON`) ~ ISSUE_DT,
                                      TRUE ~ `ULTIMO CUPON`),
           `Dias intercupones` = obtiene_dias_inter(`Tipo de instrumento`, 
                                                    Frecuencia,
                                                    `Next Cash Flow Dt`,
                                                    `ULTIMO CUPON`), 
           `Dias por vencer` = as.numeric(Vencimiento - `Fecha Valor`))
  
  # Cambiamos los DAYS_ACC de los notas chinas 
  
  ic_fv <- ic_fv %>%
    mutate(DAYS_ACC = case_when(
      `Tipo de instrumento` == 'TnotesCNY' ~ case_when(
        `Next Cash Flow Dt` <= `Fecha Valor` ~ as.numeric(`Fecha Valor`-`Next Cash Flow Dt`),
        TRUE ~ as.numeric(`Fecha Valor` - `ULTIMO CUPON`)
      ),
      TRUE ~ DAYS_ACC
    ))
  
  # Transformamos la tasa a precio para los bills en USD, SGD y JPY
  ic_fv <- ic_fv %>%
    mutate(PX_BID = precio_tasa(`Tipo de instrumento`,
                                `Dias por vencer`,
                                PX_BID))
  
  # Separamos el tibble de instrumentos en una lista con un tibble por cada tipo de 
  # instrumento 
  ic_fv_list <- split(ic_fv, f = ic_fv$`Tipo de instrumento`)
  
  # Valuamos 
  valua_ic <- lapply(names(ic_fv_list), function(name) {
    valua[[name]](ic_fv_list[[name]])
  })
  names(valua_ic) <- names(ic_fv_list)
  
  # Regresamos la lista de tibbles a un sólo tibble 
  valua_ic <- ic %>%
    select(ISIN) %>%
    left_join(purrr::reduce(valua_ic, bind_rows),
              by = 'ISIN') %>%
    mutate(`Monto a Recibir` = `Valor de Mercado` + Intereses,
           `Fecha Valor` = as.Date(`Fecha Valor`))
  
  # Hacemos Mkt Val = 0 si los instrumentos ya vencieron 
  vencimientos <- insumos$cup_ven[[port]] %>%
    filter(`Vencimiento BBG` != 0) %>%
    pull(ISIN)
  
  valua_ic <- valua_ic %>%
    mutate(across(all_of(
      c('Valor de Mercado', 'Intereses', 'Monto a Recibir')
    ), ~ case_when(ISIN %in% vencimientos ~ 0, 
                   TRUE ~ .x)))
  
  return(valua_ic)
}

# --------------------------------- valua_rf_futuros -----------------------------------
valua_rf_futuros <- function(ic_futuros, datos, px_futuros, date){
  
  if(nrow(ic_futuros)>0){
    
    ic_fut <- ic_futuros %>%
      mutate(Inst = word(Instrumento), 
             Futuro = substr(ISIN, 1, nchar(ISIN)-2),
             Divisa = substr(ISIN, 1, 2)) %>%
      left_join(datos$catalogo %>%
                  dplyr::rename(Futuro = Divisa),
                by = c('Inst', 'Futuro')) %>%
      left_join(datos$catalogo_futuros, by = 'Futuro') %>%
      left_join(px_futuros, by = 'ISIN')
    
    # Valuamos 
    valua_fut <- futuros[['valua_fut']](ic_fut, date)
    
    # Obtenemos duracion como Plazo/Numero meses
    valua_fut <- valua_fut %>%
      mutate(`Duración futuro` = case_when(Tipo == 'Tasa' ~ Plazo/12)) %>%
      mutate(`Monto a Recibir CTD en USD` = `Principal` * `Contract Size`)
    
  }else{
    valua_fut <- tibble(ISIN = NA,
                        Instrumento = NA,
                        Principal = NA,
                        Cupon = NA,
                        Frecuencia = NA, 
                        Vencimiento = NA, 
                        Inst = NA,
                        Futuro = NA, 
                        Divisa = NA,
                        `Tipo de instrumento` = NA,
                        `Contract Size` = NA, 
                        `Value of 1 pt` = NA, 
                        Tipo = NA, 
                        `Divisa Final` = NA, 
                        Plazo = NA,
                        PX_BID = NA, 
                        `Monto a Recibir` = NA,
                        `Fecha Valor` = NA,
                        `Duración futuro` = NA, 
                        `Monto a Recibir CTD en USD` = NA)
  }
  
  return(valua_fut)
}

# --------------------------------- valua_ic_ctd -----------------------------------

valua_ic_ctd <- function(ic_fut_ctd, datos, insumos, tc, px, date){
  # Valuamos CTD en caso de existir 
  if(nrow(ic_fut_ctd)>0){
    ic_fut_ctd <- ic_fut_ctd %>%
      mutate(Vencimiento = as.Date(Vencimiento)) 
    
    valua_ic_ctd <- valua_rf(ic_fut_ctd, datos, insumos, px, date) %>%
      left_join(tc, by = 'Divisa Final') %>%
      mutate(`Valor de Mercado` = `Valor de Mercado`/TC,
             Intereses = Intereses/TC,
             `Monto a Recibir` = `Monto a Recibir`/TC) %>%
      mutate(SV = sector_v(`Fecha Valor`, Vencimiento),
             `SV fijo` = sector_v(obtiene_udma(date), Vencimiento))
    
    # Renombramos 
    nombres_ic_ctd <- setNames(insumos$plantilla$ic_ctd$`R names`,
                               insumos$plantilla$ic_ctd$`Excel names`)
    
    val_ic_ctd <- valua_ic_ctd %>% 
      dplyr::rename(all_of(nombres_ic_ctd)) %>%
      select(insumos$plantilla$ic_ctd$`Excel names`)
    
  }else{
    
    val_ic_ctd <- tibble(`ISIN CTD` = NA,
                         `Duración Ajustada Bruta CTD` = NA,
                         `Valor Mercado Principal CTD` = NA,
                         `Intereses Devengados CTD` = NA,
                         `Monto a Recibir CTD tasa` = NA,
                         `Yield to Maturity CTD` = NA,
                         `Convexidad CTD` = NA,
                         `Factor de Conversión CTD` = NA,
                         `Id isin` = NA,
                         SV = NA,
                         `SV fijo` = NA)
    
  }
  
  return(val_ic_ctd)
}

# --------------------------------- junta_ic_fut -----------------------------------
junta_ic_fut <- function(valua_ic, valua_fut, tc, insumos){
  
  val_ic <- valua_ic %>%
    full_join(valua_fut) %>%
    filter(!is.na(ISIN)) %>%
    left_join(tc, by = 'Divisa Final') %>%
    mutate(`Sector` = sector_v(`Fecha Valor`, Vencimiento),
           `Sector fijo` = sector_v(obtiene_udma(date_ayer), Vencimiento),
           `Identificador` = obtiene_iden(ISIN, Instrumento, Futuro),
           `Valor de Mercado USD` = `Valor de Mercado`/TC,
           `Intereses USD` = `Intereses`/TC,
           `Monto a Recibir USD` = `Monto a Recibir`/TC,
           `Monto a Recibir CTD en USD` = `Monto a Recibir CTD en USD`/TC,
           `Ajuste inflacion` = NA) 
  
  # Dejamos la duración del instrumento CTD para los futuros de bono y la duración 
  # del futuro para los futuros de tasa
  val_ic <- val_ic %>%
    mutate(`Dur Adj Bid` = case_when(!is.na(`Duración futuro`) ~ `Duración futuro`,
                                     TRUE ~ `Dur Adj Bid`
                                     )
           ) %>%
    select(-`Duración futuro`)
  
  # Renombramos 
  nombres_ic <- setNames(insumos$plantilla$ic$`R names`,
                         insumos$plantilla$ic$`Excel names`)
  
  val_ic <- val_ic %>% 
    dplyr::rename(all_of(nombres_ic)) %>%
    select(insumos$plantilla$ic$`Excel names`)
  
  return(val_ic)
}

# --------------------------------- junta_ic_fut_ctd -----------------------------------
junta_ic_fut_ctd <- function(valua_ic, valua_fut_ctd){
  
  # Dejamos el sector de vencimiento como el sector de vencimeinto de instrumento CTD
  # para los futuros de bono 
  # Dejamos también el MTM del instrumento CTD como el Monto a recibir CTD en USD
  # para los futuros de bono
  val_ic <- valua_ic %>%
    left_join(valua_fut_ctd) %>%
    mutate(
      `Sector de Inversión` = case_when(is.na(SV) ~ `Sector de Inversión`,
                                        TRUE ~ SV),
      `Sector de Inversión fijo a inicio de mes` = case_when(
        is.na(`SV fijo`) ~ `Sector de Inversión fijo a inicio de mes`,
        TRUE ~ `SV fijo`
      )
    ) %>%
    mutate(
      `Monto a Recibir CTD en USD` = case_when(
        !is.na(`Monto a Recibir CTD tasa`) ~ `Monto a Recibir CTD tasa`,
        TRUE ~ `Monto a Recibir CTD en USD`
      )
    ) %>%
    mutate(`ISIN CTD` = case_when(
      !is.na(`ISIN CTD`) ~ `ISIN CTD`,
      grepl('FUT', Instrumento, fixed = TRUE) ~ substr(`Id isin`, 
                                                       nchar(`Id isin`) - 1, 
                                                       nchar(`Id isin`) - 1)
    )) %>%
    select(-c(SV, `SV fijo`, `Monto a Recibir CTD tasa`))
  
  
  return(val_ic)
}

# --------------------------------- genera_valua ------------------------------
genera_valua <- function(cartera, datos, px_fut, fecha, ic_fut_ctd, insumos, hist_tc, px, name_valua, port){
  tasas_frn <<- insumos$tasas_frn
  port <<- port 
  # Separamos futuros 
  ic_clas_fut <- list(ic = cartera %>%
                        filter(!grepl('FUT', Instrumento)),
                      
                      fut = cartera %>%
                        filter(grepl('FUT', Instrumento))
  )
  
  withProgress(
    message = 'Valuando futuros...',
    detail = 'Esto puede tomar unos segundos, por favor espera.',
    value = 0, {
      
      # Futuros 
      val_ic_fut <- valua_rf_futuros(ic_clas_fut$fut, 
                                     datos, 
                                     px_fut, 
                                     fecha)
      
      incProgress(1/2)
      Sys.sleep(0.25)
      val_ic_ctd <- valua_ic_ctd(ic_fut_ctd, 
                                 datos, 
                                 insumos, 
                                 hist_tc, 
                                 px, 
                                 fecha)
      incProgress(2/2)
      Sys.sleep(0.25)
    }
  )
  
  
  withProgress(
    message = 'Valuando instrumentos de renta fija...',
    detail = 'Esto puede tomar unos segundos, por favor espera.',
    value = 0, {
      
      # Parte sin futuros 
      val_ic <- valua_rf(ic_clas_fut$ic, 
                         datos, 
                         insumos,
                         px, 
                         fecha)
      
      incProgress(2/2)
      Sys.sleep(0.25)
    }
  )
  
  
  # Juntamos renta fija y futuros 
  val_ic_prev <- junta_ic_fut(val_ic,
                              val_ic_fut,
                              hist_tc,
                              insumos)
  
  # Juntamos ic con la valuación CTD de los futuros
  val_ic <- junta_ic_fut_ctd(val_ic_prev, 
                             val_ic_ctd) %>%
    select(insumos$plantilla$valuaciones$Nombres)
  
  # Imprimimos 
  readr::write_excel_csv(val_ic, 
                         name_valua, 
                         na = "")
  
  mes <- c('El portafolio se valuo exitosamente')
  shinyalert("EXITO", mes, type = "success")
  
  return(val_ic)
}

# COMPARA ----
# --------------------------------- crea_compara ------------------------------
# Función que crea el compara entre la valuacion de SV y de BV 
crea_compara <- function(val_ic, bv){
  val_ic_compara <- val_ic %>%
    filter(!grepl('FUT', Instrumento)) %>%
    select(`Id isin`, `Tipo de instrumento`,
           `Valor Nominal`,
           `Precio limpio (DIVISA)`,
           `Valor Mercado Principal (DIVISA)`, 
           `Intereses Devengados (DIVISA)`, `Monto a Recibir (DIVISA)`)
  
  names(val_ic_compara) <- c('ISIN', 
                             'Tipo de instrumento',
                             'Nominal',
                             'Precio',
                             'Valor de Mercado SV',
                             'Intereses SV',
                             'Monto a Recibir SV')
  
  compara_val <- val_ic_compara %>%
    full_join(bv %>%
                filter(!is.na(ISIN)), by = 'ISIN') %>%
    mutate_all(~replace(., is.na(.), 0))
  
  compara_dif <- compara_val %>%
    filter(abs(`Valor de Mercado SV` - `Valor de Mercado BV`) > 2 |
             abs(`Intereses SV` - `Intereses BV`) > 2 |
             abs(`Monto a Recibir SV` - `Monto a Recibir BV`) > 2)
  
  return(list(compara_val = compara_val,
              compara_dif = compara_dif))
}

# ------------------------------------ compara ---------------------------------
compara <- function(compara_val, compara_dif, name_compara_plantilla, name_compara_hoy){
  
  if(nrow(compara_dif) > 0){
    # Si hay diferencias imprimimos compara auxiliar para que el usuario decida
    # con cual valuación quedarse
    compara_print <- compara_dif %>%
      mutate(Diferencia = `Monto a Recibir SV` - `Monto a Recibir BV`,
             Seleccion = 'Super Valua')
    
    imprime_compara_aux(compara_print, 
                        name_compara_plantilla, 
                        name_compara_hoy)
    
    mes <- c('Se registraron ',
             nrow(compara_print),
             ' diferencias. Revisar la hoja Compara Auxiliar en el Compara y elegir la valuacion correcta')
    
    shinyalert("ERROR", mes, type = "error")
    
  }else{
    compara_print <- crea_compara_print(compara_val)
    
    imprime_compara(compara_print, 
                    name_compara_plantilla, 
                    name_compara_hoy)
    
    mes <- c('No se registraron diferencias. Continuar con los resultados')
    shinyalert("EXITO", mes, type = "success")
  }
}




# ------------------------------- crea_compara_print ----------------------------
# Función que crea el compara de impresión 
crea_compara_print <- function(compara_val){
  tipo_inst <- c('ABS', 'Agen', 'AgenR', 'FRNs', 'MTI', 'Tbills', 'TIPS', 'Tnotes')
  divisas <- c('AUD', 'CAD', 'CNY', 'EUR', 'GBP', 'JPY', 'NZD', 'SGD', 'USD')
  
  compara <- compara_val %>%
    dplyr::mutate(`Tipo de instrumento` = if_else(`Tipo de instrumento` == 'billsSGD',
                                                  'TbillsSGD',
                                                  `Tipo de instrumento`),
                  `Tipo de instrumento` = if_else(`Tipo de instrumento` %in% tipo_inst,
                                                  paste0(`Tipo de instrumento`, "USD"),
                                                  `Tipo de instrumento`),
                  `Tipo de instrumento` = gsub("FRA$", "EUR", `Tipo de instrumento`),
                  `Tipo de instrumento` = gsub("GER$", "EUR", `Tipo de instrumento`)) %>%
    group_by(`Tipo de instrumento`) %>%
    summarize(`Monto a Recibir SV` = sum(`Monto a Recibir SV`),
              `Monto a Recibir BV` = sum(`Monto a Recibir BV`)) %>%
    mutate(Tipo = substr(`Tipo de instrumento`, 
                         1, 
                         nchar(`Tipo de instrumento`) - 3),
           Divisa = substr(`Tipo de instrumento`, 
                           nchar(`Tipo de instrumento`) - 2, 
                           nchar(`Tipo de instrumento`))) %>%
    select(-`Tipo de instrumento`) %>%
    full_join(tibble(Divisa = divisas), by = 'Divisa')
  
  compara_print <- lapply(split(compara, f = compara$Divisa),
                          function(x) tibble(Tipo = tipo_inst) %>%
                            full_join(x, by = 'Tipo') %>%
                            select(-Divisa) %>%
                            mutate_all(~replace(., is.na(.), 0)) %>%
                            filter(Tipo != 0)) 
  
  return(compara_print)
}


# ----------------------------- imprime_compara_aux ----------------------------
imprime_compara_aux <- function(compara_val_print, compara_wb, compara_name){
  
  wb_compara_aux <- loadWorkbook(compara_wb)
  
  writeData(wb_compara_aux,
            'Compara Auxiliar',
            compara_val_print,
            startCol = 1,
            startRow = 2,
            colNames = F)
  
  addStyle(wb_compara_aux, 
           'Compara Auxiliar', 
           createStyle(numFmt = "#,##0.00"),
           rows = c(2:100),
           cols = c(3,5:11),
           gridExpand = TRUE,
           stack = T)
  
  saveWorkbook(wb_compara_aux, compara_name, overwrite = T)
}

# ----------------------------- imprime_compara ----------------------------
imprime_compara <- function(compara_print, compara_wb, compara_name){
  
  wb_compara <- loadWorkbook(compara_wb)
  
  col_n = 3
  row_n = 3
  i = 1
  
  for(divisa in names(compara_print)){
    compara_print_divisa = compara_print[[divisa]] %>%
      select(-Tipo)
    
    writeData(wb_compara,
              'Compara',
              compara_print_divisa,
              startCol = col_n,
              startRow = row_n,
              colNames = F)
    
    addStyle(wb_compara, 
             'Compara', 
             createStyle(numFmt = "#,##0.00"),
             rows = c(row_n:(row_n+nrow(compara_print_divisa)-1)),
             cols = c(col_n:(col_n+1)),
             gridExpand = TRUE,
             stack = T)
    
    col_n <- if_else(i %% 3 == 0,
                     col_n + 5,
                     col_n)
    
    row_n <- if_else(i %% 3 == 0,
                     3,
                     row_n + 11)
    
    i <- i+1
  }
  
  saveWorkbook(wb_compara, compara_name, overwrite = T)
}

# CORRIGE COMPARA ----
# ----------------------------- cambia_valuacion ------------------------------
cambia_valuacion <- function(val_ic, compara_aux, tc, name_valua, datos){
  # En caso de existir diferencias, cambiamos la valuacion por la eleccion del usuario
  val_ic_nuevo <- val_ic %>%
    left_join(compara_aux %>%
                dplyr::rename(`Id isin` = ISIN) %>%
                select(-c(`Tipo de instrumento`,
                          Nocional,
                          Precio)),
              by = 'Id isin') %>%
    mutate(
      `Valor Mercado Principal (DIVISA)` = case_when(
        Seleccion == 'Bond Valuation' ~ `Valor de Mercado BV`,
        TRUE ~ `Valor Mercado Principal (DIVISA)`
      ),
      `Intereses Devengados (DIVISA)` = case_when(
        Seleccion == 'Bond Valuation' ~ `Intereses BV`,
        TRUE ~ `Intereses Devengados (DIVISA)`
      ),
      `Monto a Recibir (DIVISA)` = case_when(
        Seleccion == 'Bond Valuation' ~ `Monto a Recibir BV`,
        TRUE ~ `Monto a Recibir (DIVISA)`
      )
    )
  
  val_ic_nuevo <- val_ic_nuevo %>%
    mutate(Divisa = substr(`Id isin`, 1, 2)) %>%
    left_join(datos$divisas,
              by = 'Divisa') %>%
    left_join(tc, by = 'Divisa Final') %>%
    mutate(
      `Valor Mercado Principal (USD)` = case_when(
        Seleccion == 'Bond Valuation' ~ `Valor Mercado Principal (DIVISA)` / TC,
        TRUE ~ `Valor Mercado Principal (USD)`
      ),
      `Intereses Devengados (USD)` = case_when(
        Seleccion == 'Bond Valuation' ~ `Intereses Devengados (DIVISA)` / TC,
        TRUE  ~ `Intereses Devengados (USD)`
      ),
      `Monto a Recibir (USD)` = case_when(
        Seleccion == 'Bond Valuation' ~ `Monto a Recibir (DIVISA)` / TC,
        TRUE ~ `Monto a Recibir (USD)`
      )
    ) %>%
    select(names(val_ic))
  
  # Imprimimos 
  readr::write_excel_csv(val_ic_nuevo, 
                         name_valua, 
                         na = "")
  
  return(val_ic_nuevo)

}

# ----------------------------- compara_corregido ------------------------------
compara_corregido <- function(val_ic_nuevo, bv, name_compara){
  
  compara_list_corregido <- crea_compara(val_ic_nuevo, 
                                         bv)
  
  compara_print_corregido <- crea_compara_print(compara_list_corregido$compara_val)
  
  imprime_compara(compara_print_corregido, 
                  name_compara, 
                  name_compara)
  
  mes <- c('Se corrigio la valuacion y el compara de acuerdo al Compara Auxiliar. Revisar los archivos para conformar la valuacion')
  shinyalert("EXITO", mes, type = "info")
}

# FUNCIONES RESULTADOS ----
# -------------------------------- obtiene_base ---------------------------------
obtiene_base <- Vectorize(
  obtiene_base <- function(ins, mtm_t1_usd, compras_usd, ventas_usd){
    if(grepl("FUT", ins)){
      base <- mtm_t1_usd - compras_usd - ventas_usd
    }else{
      base <- mtm_t1_usd - compras_usd
    }
    return(base)
  }
)


# -------------------------------- obtiene_rend_tasa ---------------------------------
obtiene_rend_tasa <- Vectorize(
  obtiene_rend_tasa <- function(ins, res_tasa, mtm_t1_div, compras_div, ventas_div){
    if(res_tasa == 0){
      rend_tasa <- 0
    }else{
      if(grepl("FUT", ins)) {
        rend_tasa <- res_tasa / (mtm_t1_div - compras_div - ventas_div)
      } else{
        rend_tasa <- res_tasa / (mtm_t1_div - compras_div)
      }
    }
    return(rend_tasa)
  }
)

# -------------------------------- obtiene_rend ---------------------------------
obtiene_rend <- Vectorize(
  obtiene_rend <- function(res_usd, rend_tasa, rend_cambiario){
    if(res_usd == 0){
      rend <- 0
    }else{
      rend <- ((1 + rend_tasa) * (1 + rend_cambiario)) - 1
    }
    return(rend)
  }
)

# -------------------------------- obtiene_res ---------------------------------
obtiene_res = Vectorize(
  
  obtiene_res <- function(MTM, MTM_t_1, Compras, Ventas, Cupones, Vencimientos){
    
    resultado <- MTM - MTM_t_1 + Compras + Ventas + Cupones + Vencimientos
    return(resultado)
  }
)

# ----------------------------- obtiene_dur_dias -------------------------------
obtiene_dur_dias <- Vectorize(
  obtiene_dur_dias <- function(ins, tipo_fut, mtm_t_div, dur, dur_ctd, factor_ctd){
    if(mtm_t_div == 0){
      dur_dias <- 0 
    }else{
      if(grepl("FUT", ins) & tipo_fut == 'Bonos'){
        dur_dias <- 365 * (dur_ctd / factor_ctd)
      }else{
        dur_dias <- 365 * dur
      }
    }
    return(dur_dias)
  }
)

# ----------------------------- obtiene_dur_pond -------------------------------
obtiene_dur_pond <- Vectorize(
  obtiene_dur_pond <- function(tipo_ins, ins, mtm_t_usd, mtm_t_usd_ctd, dur_dias){
    if((!is.na(mtm_t_usd) & mtm_t_usd == 0) | (!is.na(mtm_t_usd_ctd) & mtm_t_usd_ctd == 0)){
      dur_pond <- 0 
    }else{
      if(grepl("FUT", ins)){
        dur_pond <- dur_dias * mtm_t_usd_ctd
      }else{
        if(tipo_ins == 'TIPS'){
          dur_pond <- dur_dias * mtm_t_usd * 2
        }else{
          dur_pond <- dur_dias * mtm_t_usd
        }
      }
    }
    return(dur_pond)
  }
)

# ----------------------------- obtiene_ytm_pond -------------------------------
obtiene_ytm_pond <- Vectorize(
  obtiene_ytm_pond <- function(ins, tipo_fut, mtm_t_usd, mtm_t_usd_ctd, ytm, ytm_ctd, px){
    if((!is.na(mtm_t_usd) & mtm_t_usd == 0) | (!is.na(mtm_t_usd_ctd) & mtm_t_usd_ctd == 0)){
      ytm_pond <- 0 
    }else{
      if(grepl("FUT", ins) & tipo_fut == 'Bonos'){
        ytm_pond <- ytm_ctd * mtm_t_usd_ctd
      }else{
        if(grepl("FUT", ins) & tipo_fut == 'Tasa'){
          ytm_pond <- (100 - px) * mtm_t_usd_ctd
        }else{
          ytm_pond <- ytm * mtm_t_usd
        }
      }
    }
    return(ytm_pond)
  }
)

# ----------------------------- obtiene_cvx_pond -------------------------------
obtiene_cvx_pond <- Vectorize(
  obtiene_cvx_pond <- function(tipo_ins, ins, tipo_fut, mtm_t_usd, mtm_t_usd_ctd, cvx, cvx_ctd){
    if((!is.na(mtm_t_usd) & mtm_t_usd == 0) | (!is.na(mtm_t_usd_ctd) & mtm_t_usd_ctd == 0)){
      cvx_pond <- 0 
    }else{
      if(grepl("FUT", ins) & tipo_fut == 'Bonos'){
        cvx_pond <- 365 * mtm_t_usd_ctd * cvx_ctd
      }else{
        if(grepl("FUT", ins) & tipo_fut == 'Tasa'){
          cvx_pond <- 0
        }else{
          if(tipo_ins == 'TIPS'){
            cvx_pond <- 365 * mtm_t_usd * cvx * 4
          }else{
            cvx_pond <- 365 * mtm_t_usd * cvx
          }
        }
      }
    }
    return(cvx_pond)
  }
)

# ----------------------------- obtiene_iden_res -------------------------------
obtiene_iden_res <- Vectorize(
  obtiene_iden_res <- function(isin, ins, futuro, tipo_fut){
    
    if(grepl("FUT", ins)){
      # Si el instrumento es un futuro, ocupamos el nombre del futuro si es de bono
      # y ocupamos el mes del futuo (F, G, H,...) si es de tasa
      
      iden_res <- case_when(tipo_fut == 'Bonos' ~ futuro,
                            TRUE ~ substr(isin,
                                          nchar(futuro) + 1,
                                          nchar(futuro) + 1)
                            )
    }else{
      
      iden_res <- case_when(substr(isin, 1, 2) %in% c('DE', 'FR') ~ 'EU',
                            TRUE ~ substr(isin, 1, 2)
                            )
    }
    return(iden_res)
  }
)



# -------------------------------- obtiene_mov ---------------------------------
obtiene_mov <- function(comprasventas, cuponesvencimientos, port){
  if(nrow(comprasventas)==0){
    compras_ventas <- tibble(`Id isin` = 'US91282CFC01',
                             `Compras Divisa` = NA, 
                             `Ventas Divisa` = NA)
  }else{
    compras_ventas <- comprasventas %>%
      group_by(isin, buy_sell) %>%
      summarize(proceeds = sum(proceeds)) %>%
      ungroup() %>%
      mutate(`Compras Divisa` = case_when(buy_sell == 'Buy' ~ proceeds, 
                                          TRUE ~ 0),
             `Ventas Divisa` = case_when(buy_sell == 'Sell' ~ proceeds, 
                                         TRUE ~ 0)) %>%
      group_by(isin) %>%
      summarize(`Compras Divisa` = sum(`Compras Divisa`),
                `Ventas Divisa` = sum(`Ventas Divisa`)) %>%
      ungroup() %>%
      select(isin, `Compras Divisa`, `Ventas Divisa`) %>%
      dplyr::rename(`Id isin` = isin)
  }
  
  
  if(nrow(cuponesvencimientos[[port]])==0){
    cup_ven <- tibble(`Id isin` = 'US91282CFC01',
                      `Cupones Divisa` = NA,
                      `Vencimientos Divisa` = NA)
  }else{
    cup_ven <- cuponesvencimientos[[port]] %>%
      select(`ISIN`, `Cupón BBG`, `Vencimiento BBG`) %>%
      dplyr::rename(`Id isin` = ISIN,
                    `Cupones Divisa` = `Cupón BBG`,
                    `Vencimientos Divisa` = `Vencimiento BBG`)
  }
  
  movimientos <- compras_ventas %>%
    full_join(cup_ven, by = 'Id isin') %>%
    mutate_all(~replace(., is.na(.), 0))
  
  return(movimientos)
}

# -------------------------------- junta_port ---------------------------------
junta_port <- function(port_hoy, port_ayer, movimientos, catalogo_resultados){
  # Clasificamos futuros 
  port_mov <- port_hoy %>%
    select(`Id isin`,
           Instrumento,
           `Precio limpio (DIVISA)`,
           `Monto a Recibir (DIVISA)`,
           `Monto a Recibir (USD)`,
           `Sector de Inversión`,
           `Duración Ajustada Bruta`,
           `Yield to Maturity`,
           `Convexidad`,
           `Monto a Recibir CTD en USD`,
           `Tipo de instrumento`,
           `Duración Ajustada Bruta CTD`,
           `Yield to Maturity CTD`,
           `Convexidad CTD`,
           `Factor de Conversión CTD`) %>%
    full_join(port_ayer %>%
                select(`Id isin`,
                       Instrumento,
                       `Monto a Recibir (DIVISA)`,
                       `Monto a Recibir (USD)`,
                       `Tipo de instrumento`,
                       `Sector de Inversión`) %>%
                dplyr::rename(`Monto a Recibir (DIVISA) t-1` = `Monto a Recibir (DIVISA)`,
                              `Monto a Recibir (USD) t-1` = `Monto a Recibir (USD)`,
                              `Sector de Inversión ayer` = `Sector de Inversión`),
              by = c('Id isin', 'Tipo de instrumento', 'Instrumento')) %>%
    mutate(`Sector de Inversión` = case_when(is.na(`Sector de Inversión`) ~ `Sector de Inversión ayer`,
                                             TRUE ~ `Sector de Inversión`)) %>%
    select(-`Sector de Inversión ayer`) %>%
    left_join(movimientos, by = 'Id isin') %>%
    left_join(catalogo_resultados, by = 'Tipo de instrumento')
  
  port_mov <- port_mov %>%
    mutate(across(c('Monto a Recibir (DIVISA)',
                    'Monto a Recibir (USD)',
                    'Monto a Recibir (DIVISA) t-1',
                    'Monto a Recibir (USD) t-1',
                    'Compras Divisa',
                    'Ventas Divisa',
                    'Cupones Divisa',
                    'Vencimientos Divisa'), ~ ifelse(is.na(.), 0, .)))
  
  return(port_mov)
}

# ----------------------------- obtiene_resultados ------------------------------
obtiene_resultados <- function(port_mov, hist_tc_t, hist_tc_t1){
  port_resultados <- port_mov %>%
    left_join(hist_tc_t, by = 'Divisa Final') %>%
    left_join(hist_tc_t1 %>%
                dplyr::rename(`TC t-1` = `TC`), by = 'Divisa Final') %>%
    mutate(
      `Monto a Recibir (DIVISA)` =
        case_when(
          `Divisa Final` == 'USD' &
            (is.na(`Monto a Recibir (DIVISA)`) |
               `Monto a Recibir (DIVISA)` == 0)  ~ `Monto a Recibir (USD)`,
          TRUE ~ `Monto a Recibir (DIVISA)`
        ),
      `Monto a Recibir (DIVISA) t-1` =
        case_when(
          `Divisa Final` == 'USD' &
            (is.na(`Monto a Recibir (DIVISA) t-1`) |
               `Monto a Recibir (DIVISA) t-1` == 0) ~ `Monto a Recibir (USD) t-1`,
          TRUE ~ `Monto a Recibir (DIVISA) t-1`
        )
    ) %>%
    mutate(
      `Compras USD` = `Compras Divisa` / TC,
      `Ventas USD` = `Ventas Divisa` / TC,
      `Cupones USD` = `Cupones Divisa` / TC,
      `Vencimientos USD` = `Vencimientos Divisa` / TC
    ) %>%
    mutate(
      `Resultado Divisa` = obtiene_res(
        `Monto a Recibir (DIVISA)`,
        `Monto a Recibir (DIVISA) t-1`,
        `Compras Divisa`,
        `Ventas Divisa`,
        `Cupones Divisa`,
        `Vencimientos Divisa`
      ),
      `Resultado USD` = obtiene_res(
        `Monto a Recibir (USD)`,
        `Monto a Recibir (USD) t-1`,
        `Compras USD`,
        `Ventas USD`,
        `Cupones USD`,
        `Vencimientos USD`
      )
    ) %>%
    mutate(
      Base = obtiene_base(
      `Instrumento`,
      `Monto a Recibir (USD) t-1`,
      `Compras USD`,
      `Ventas USD`
    )) %>%
    mutate(
      # Peso = Base / sum(Base),
      Peso = case_when(
        `Tipo de instrumento` != "TnotesCNY" ~ Base / sum(Base[`Tipo de instrumento` != "TnotesCNY"]),
        `Tipo de instrumento` == "TnotesCNY" ~ Base / sum(Base[`Tipo de instrumento` == "TnotesCNY"])
      ),
      `Resultado tasa` = `Resultado Divisa` / `TC t-1`,
      `Resultado cambiario` = `Resultado USD` - `Resultado tasa`
    ) %>%
    mutate(
      `Rendimiento Tasa` = obtiene_rend_tasa(
        `Instrumento`,
        `Resultado tasa`,
        `Monto a Recibir (DIVISA) t-1`,
        `Compras Divisa`,
        `Ventas Divisa`
      ),
      `Rendimiento Cambiario` = case_when(`Resultado cambiario` == 0 ~ 0,
                                          TRUE ~ `TC t-1` / `TC` - 1)
    ) %>%
    mutate(
      Rendimiento = obtiene_rend(
        `Resultado USD`,
        `Rendimiento Tasa`,
        `Rendimiento Cambiario`
      )
    ) %>%
    mutate(
      Contribución = Rendimiento * Peso
    )
  
  return(port_resultados)
}

# ---------------------------- obtiene_indicadores -----------------------------
obtiene_indicadores <- function(port_resultados){
  port_ind <- port_resultados %>%
    mutate(
      `Duración en días` = obtiene_dur_dias(
        `Instrumento`,
        `Tipo`,
        `Monto a Recibir (DIVISA)`,
        `Duración Ajustada Bruta`,
        `Duración Ajustada Bruta CTD`,
        `Factor de Conversión CTD`
      )
    ) %>%
    mutate(
      `Duración ponderada` = obtiene_dur_pond(
        `Tipo de instrumento`,
        `Instrumento`,
        `Monto a Recibir (USD)`,
        `Monto a Recibir CTD en USD`,
        `Duración en días`
      )
    ) %>%
    mutate(
      `YTM ponderada` = obtiene_ytm_pond(
        `Instrumento`,
        `Tipo`,
        `Monto a Recibir (USD)`,
        `Monto a Recibir CTD en USD`,
        `Yield to Maturity`,
        `Yield to Maturity CTD`,
        `Precio limpio (DIVISA)`
      )
    ) %>%
    mutate(
      `Convexidad ponderada` = obtiene_cvx_pond(
        `Tipo de instrumento`,
        `Instrumento`,
        `Tipo`,
        `Monto a Recibir (USD)`,
        `Monto a Recibir CTD en USD`,
        `Convexidad`,
        `Convexidad CTD`
      )
    ) %>%
    mutate(
      `MTM CTD USD` = case_when(
        is.na(`Monto a Recibir CTD en USD`) ~ `Monto a Recibir (USD)`,
        TRUE ~ `Monto a Recibir CTD en USD`
      )
    ) %>%
    mutate(
      `Identificador Instrumento` = obtiene_iden_res(
        `Id isin`,
        `Instrumento`,
        `Futuro`,
        `Tipo`)
    )
  
  return(port_ind)
}

# ------------------------------ genera_resultados -----------------------------
genera_resultados <- function(name_port_hoy, name_port_ayer, name_res, compras_ventas, cup_ven, insumos, datos, port){
  # Leemos port de ayer y de hoy 
  port_hoy <- tryCatch(read_csv(name_port_hoy, locale=locale(encoding="UTF8")),
                       error=function(e) 
                         return(read_csv(name_port_hoy, locale=locale(encoding="latin1"))))
  
  port_ayer <- tryCatch(read_csv(name_port_ayer, locale=locale(encoding="UTF8")),
                       error=function(e) 
                         return(read_csv(name_port_ayer, locale=locale(encoding="latin1"))))
  
  
  # Obtenemos movimientos del día 
  movimientos <- obtiene_mov(compras_ventas,
                             cup_ven,
                             port)
  
  # Junta port ayer y hoy con los movimientos del día 
  port_mov <- junta_port(port_hoy, 
                         port_ayer, 
                         movimientos, 
                         datos$catalogo_res)
  
  # Agrega el tipo de futuros en caso de existir
  port_mov <- port_mov %>%
    mutate(Futuro = substr(`Id isin`,1,nchar(`Id isin`)-2)) %>%
    left_join(datos$catalogo_futuros %>%
                select(Futuro, Tipo), by = 'Futuro') 
  
  # Obtiene el archivo de resultados del día
  port_resultados <- obtiene_resultados(port_mov,
                                        insumos$hist_tc$hoy,
                                        insumos$hist_tc$ayer)
  
  port_indicadores <- obtiene_indicadores(port_resultados)
  
  # Cambia nombres y selecciona las columnas del archivo
  nombres_res <- setNames(insumos$plantilla$resultados$`R names`,
                          insumos$plantilla$resultados$`Excel names`)
  
  resultados <- port_indicadores %>%
    select(insumos$plantilla$resultados$`R names`) %>%
    dplyr::rename(all_of(nombres_res)) 
  
  # Imprimimos 
  readr::write_excel_csv(resultados, 
                         name_res, 
                         na = "")
  
  mes <- c('Los resultados del portafolio fueron generados exitosamente')
  shinyalert("EXITO", mes, type = "success")
  
  return(resultados)
  
}

# FUNCIONES REBALANCEO -------------------------------------------------------
name_bmk_aux <- function(rutas, fecha){
  Sys.setlocale("LC_TIME", "English")
  ruta <- paste0(rutas$global, 
                 format(fecha, "%Y"), 
                 '/',
                 'BMK', 
                 '/',
                 format(fecha, "%Y%m"),
                 '/',
                 'BMK',
                 format(fecha, "%y%m"),
                 '_aux.csv')
  return(ruta)
}

name_bmk_reb <- function(rutas, fecha){
  Sys.setlocale("LC_TIME", "English")
  ruta <- paste0(rutas$global, 
                 format(fecha, "%Y"), 
                 '/',
                 'BMK', 
                 '/',
                 format(fecha, "%Y%m"),
                 '/',
                 'BMK',
                 format(fecha, "%y%m"),
                 '_1.csv')
  return(ruta)
}


# -------------------------------- lee_bmk_reb -----------------------------------
lee_bmk_reb <- function(name_bmk){
  bmk <- read_csv(name_bmk, skip = 2) %>%
    mutate(
      MATURITY = case_when(
        is.numeric(MATURITY) ~ as.Date(MATURITY, origin = "1899-12-30"),
        TRUE ~ as.Date(MATURITY, format = '%d/%m/%Y'))
    ) %>%
    dplyr::rename(Vencimiento = MATURITY,
                  Frecuencia = CPN_FREQ) %>%
    select(ISIN, Instrumento, Principal, Cupon, Frecuencia, Vencimiento, Indice) %>%
    filter(nchar(ISIN)>3)
  
  return(bmk)
}