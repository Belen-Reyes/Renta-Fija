# Funciones para actualizar las tasas FRN y el CPI
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

# ---------------------------------- round2 ----------------------------------
# Función auxiliar para redondear hacia arriba 
round2 = function(x, digits) {
  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^digits
  z*posneg
}

# ---------------------------------- truncate ----------------------------------
# Función auxiliar para truncar con decimales
truncate <- function(x, decimals = 6) {
  factor <- 10^decimals
  truncated_value <- trunc(x * factor) / factor
  return(truncated_value)
}

# ---------------------------- verifica_reapertura ----------------------------
# Verifica que exista la información de reapertura de todos los FRN de la cartera

verifica_reapertura <- function(frn, tasas_frn, nombre_frn){
  frn <<- frn 
  tasas_frn <<- tasas_frn
  frn_ic <- frn$ISIN
  frn_reapertura <- tasas_frn$reaperturas$ISIN
  
  if(!all(frn_ic %in% frn_reapertura)){
    
    frn_faltantes <- frn_ic[which(!frn_ic %in% frn_reapertura)]
    n <- length(frn_reapertura) + 2
    
    
    # Imrprimimos los ISIN de los instrumentos faltantes 
    writeData(
      wb = tasas_frn$index_wb,
      sheet = 'Reaperturas',
      tibble(ISIN = frn_faltantes),
      startCol = 1,
      startRow = n,
      colNames = F
    )
    
    saveWorkbook(tasas_frn$index_wb, 
                 nombre_frn, 
                 overwrite = T)
    
    # Imprimimos aviso
    # inst <- paste(frn_faltantes, collapse =", ")
    mes <- c('Se registraron nuevos instrumentos: ',
             paste(frn_faltantes, collapse =" "),
             'Agregar su informacion en la hoja Reapertura del archivo Tasas FRN 
             y volver a correr el boton')
    
    shinyalert("ERROR", mes, type = "error")
    
    while (length(frn_faltantes)>0) {
      Sys.sleep(2)
    }
  }
}

# --------------------------- actualiza_frn_index ----------------------------
# Actualizamos USBMMY3M Index en caso de ser necesario
actualiza_frn_index <- function(tasas_frn, names){
  last_date_cpi <- bdp(names$frn_index, "LAST_UPDATE_DT")[[1]]
  fechas <- as.Date(tasas_frn$index$Fecha)
  
  if(!last_date_cpi %in% fechas){
    
    last_cpi <- bdh(names$frn_index, 
                    'PX_LAST', 
                    start.date = last_date_cpi, 
                    end.date = last_date_cpi)[[2]]
    
    frn_index_update <- tasas_frn$index %>% 
      add_row(Fecha = last_date_cpi, PX_LAST = last_cpi) %>%
      mutate(Fecha = as.Date(Fecha))
    
    writeData(
      wb = tasas_frn$index_wb,
      sheet = names$frn_index,
      frn_index_update,
      startCol = 1,
      startRow = 2,
      colNames = F
    )
    
    saveWorkbook(tasas_frn$index_wb, names$frn, overwrite = T)
    
    mes <- 'El indice FRN fue actualizado correctamente. Continuar con el proceso'
    
    shinyalert("EXITO", mes, type = "success")

  }else{
    frn_index_update <- tasas_frn$index
    
    mes <- 'El indice FRN ya se encontraba actualizado. Continuar con el proceso'
    
    shinyalert("EXITO", mes, type = "success")
  }
  return(frn_index_update)
}

# --------------------------- actualiza_frn_spread ----------------------------
# Actualiza el spread de los FRN de la cartera
actualiza_frn_spread <- function(frn, tasas_frn, names){
  tasas_frn <- tasas_frn
  spreads_frn <- frn %>%
    mutate(Spread = bdp(paste0(ISIN, " GOVT"), "FLT_SPREAD")[[1]],
           Spread = round2(Spread, 1)) %>%
    select(ISIN, Spread)
  
  deleteData(tasas_frn$index_wb,
             sheet = "Spreads FRN",
             cols = 1:2,
             rows = 2:500,
             gridExpand = TRUE)
  
  
  writeData(
    wb = tasas_frn$index_wb,
    sheet = "Spreads FRN",
    spreads_frn,
    startCol = 1,
    startRow = 2,
    colNames = F
  )
  
  saveWorkbook(tasas_frn$index_wb, names$frn, overwrite = T)
  
  return(spreads_frn)
}

# --------------------------- obtiene_index_rate ----------------------------
# Obtiene la fecha correspondiente del index rate dada una fecha dada
# y un vector de lockouts 
obtiene_index_rate <- function(fecha, isin, fecha_sin_lo, t_1) {
  
  fechas_feriados <- as.Date(hol$US[!is.na(hol$US)])
  fechas_lockouts <- as.Date(lockouts_frn_dates[[isin]])
  fechas_lockouts <- c(fechas_feriados, 
                       fechas_lockouts)
  
  if(fecha %in% fechas_frn){
    fecha_index <- fechas_frn[which(fechas_frn == fecha) - 1]
  }else{
    if(fecha %in% fechas_lockouts & fecha_sin_lo %in% fechas_frn){
      fecha_index <- fechas_frn[which(fechas_frn == fecha_sin_lo) - 1]
    }else{
      if(fecha %in% fechas_lockouts & t_1 %in% fechas_frn){
        fecha_index <- fechas_frn[which(fechas_frn == t_1) - 1]
      }else{
        fechas_menores <- fechas_frn[which(fechas_frn <= fecha)]
        n <- min(as.numeric(abs(fechas_menores - fecha)))
        fecha_index <- as.Date(fecha - n)
        
        if(fecha %in% fechas_lockouts){
          fechas_menores <- fechas_frn[which(fechas_frn <= fecha_sin_lo)]
          n <- min(as.numeric(abs(fechas_menores - fecha_sin_lo)))
          fecha_index <- as.Date(fecha_sin_lo - n)
        }else{
          fecha_index <- fecha_index
        }
      }
    }
  }
  
  return(as.Date(fecha_index))
}

obtiene_index_rate_v <- Vectorize(obtiene_index_rate)
# --------------------------- index_rate ----------------------------
index_rate <- function(df) {
  isin <- unique(df$ISIN)
  
  # Agregamos feriados de US a los lockouts 
  # Obtenemos un tibble con los feriados de US para cada FRN 
  fechas_feriados <- hol['US'] %>%
    filter(!is.na(US))
  tibble_expandido <- map_dfc(1:ncol(lockouts_frn_dates), ~ fechas_feriados$US)
  names(tibble_expandido) <- names(lockouts_frn_dates)
  
  # Agregamos los feriados a los lockouts de cada FRN
  lockouts_feriados <- rbind(lockouts_frn_dates,
                              tibble_expandido)
  
  df <- df %>%
    mutate(`Fecha sin LO` = dates(lockouts_feriados, Fecha, 1, "resta", isin),
           `t_1` = Fecha - 1,
           `Fecha Index` = obtiene_index_rate_v(Fecha, ISIN, `Fecha sin LO`, t_1),
           `Fecha Index` = as.Date(`Fecha Index`)) %>%
    select(-c(`Fecha sin LO`, t_1))
  
  return(df)
}

# --------------------------- add_spread ----------------------------
# Function auxiliar para anadir el spread a cada tibble correspondiente 
add_spread <- function(tibble, spread_tibble) {
  spread_value <- spread_tibble %>%
    filter(ISIN == tibble$ISIN[1]) %>% 
    pull(Spread)
  
  tibble_spread <- tibble %>% 
    mutate(Spread = spread_value)
  
  return(tibble_spread)
}

# --------------------------- obtiene_tasas_frn ----------------------------
obtiene_tasas_frn <- function(frn, frn_index, frn_spread, date_frn, lockouts_frn_dates){
  fechas_frn <<- as.Date(frn_index$Fecha)
  lockouts_frn_dates <<- lockouts_frn_dates
  
  frn_list <- frn %>%
    group_by(ISIN) %>%
    group_map(~ {
      tibble(
        Fecha = seq(.x$`ULTIMO CUPON`, date_frn, by = "days")
      )
    }) %>%
    set_names(unique(frn$ISIN))
  
  frn_list <- imap(frn_list, function(tibble, isin) {
    tibble %>%
      mutate(ISIN = isin)
  })
  
  # fechas_frn <- as.Date(frn_index$Fecha)
  
  frn_dates_list <- lapply(frn_list, index_rate) 
  frn_index_list <- lapply(frn_dates_list,
                           function(x)
                             x %>%
                             left_join(frn_index %>%
                                         dplyr::rename(`Fecha Index` = Fecha),
                                       by = 'Fecha Index'))
  frn_spread_list <- map(frn_index_list, 
                         add_spread, 
                         spread_tibble = frn_spread)
  
  frn_tasas_list <- lapply(frn_spread_list,
                           function(x) x %>%
                             mutate(Tasa = round2((PX_LAST/100 + Spread/10000)/360*100,9)))
  
  return(frn_tasas_list)
  
}

# ------------------------------ imprime_tasas_frn -----------------------------
imprime_tasas_frn <- function(frn_tasas_list, wb, names_frn){
  # Combinamos todos los tibbles de la lista en uno 
  frn_tasas_tibble <- bind_rows(frn_tasas_list, 
                                .id = "ISIN")
  
  # Seleccionamos la fecha y su tasa correspondiente para cada ISIN
  frn_tasas <- frn_tasas_tibble %>%
    select(Fecha, ISIN, Tasa) %>%
    pivot_wider(names_from = ISIN, values_from = Tasa)
  
  deleteData(wb, 
             sheet = "Tasas FRN",
             cols = 1:500, 
             rows = 1:500, 
             gridExpand = TRUE)
  
  writeData(
    wb = wb,
    sheet = "Tasas FRN",
    frn_tasas,
    startCol = 1,
    startRow = 1,
    colNames = T,
    headerStyle = openxlsx_getOp("headerStyle")
  )
  
  saveWorkbook(wb, names_frn, overwrite = T)
  
  mes <- 'Se imprimio el historico de tasas FRN'
  
  shinyalert("EXITO", mes, type = "success")
  
}

# ------------------------------ obtiene_lockout ------------------------------ 
# Obtiene lockout de frns dependiendo de su fecha de reapertura y vencimiento  
obtiene_lockout <- function(df){
  fechas <- c(df$INT_ACC_DT,
              df$`Reapertura 1`,
              df$`Reapertura 2`,
              df$Vencimiento)
  
  fecha_1 <- ceiling_date(fechas[3] %m+% months(1), 'month') - 1
  seq_fechas <- seq(fecha_1, fechas[4], "3 months")
  seq_fechas <- if_else(format(seq_fechas, '%d') == '01', seq_fechas-1, seq_fechas)
  fechas_inter <- ceiling_date(seq_fechas, 'month') - 1
  
  fechas_final <- c(fechas[1], 
                    fechas[2], 
                    fechas[3],
                    fechas_inter)
  
  tibble_fechas <- tibble(Reapertura = fechas_final, 
                          t_1 = dates(hol, fechas_final, 1, 'resta', 'US'), 
                          t_2 = dates(hol, fechas_final, 2, 'resta', 'US'),
                          t_1_mx = dates(hol, fechas_final, 1, 'resta', 'MX'), 
                          t_2_mx = dates(hol, fechas_final, 2, 'resta', 'MX'))
  
  return(tibble_fechas)
}

# ------------------------------ imprime_lockouts ------------------------------ 
# Imprimimos lockouts de los frn 
imprime_lockouts <- function(lockouts_frn, wb, names_frn, frn_isin){
  deleteData(wb, 
             sheet = "Lockouts",
             cols = 1:500, 
             rows = 1:500, 
             gridExpand = TRUE)
  
  for(isin in frn_isin){
    
    dato <- pivot_longer(lockouts_frn[[isin]], 
                         everything(), 
                         names_to = 'tipo', 
                         values_to = 'dato') %>% 
      select(-tipo) %>%
      distinct(dato, .keep_all = TRUE)
    names(dato) <-  isin
    
    writeData(
      wb = wb,
      sheet = "Lockouts",
      dato,
      startCol = which(frn_isin == isin),
      startRow = 1,
      colNames = T,
      headerStyle = openxlsx_getOp("headerStyle")
    )
    
  }
  
  saveWorkbook(wb, names_frn, overwrite = T)
  
  lockouts_frn_dates <- read_excel(names_frn, 
                                   sheet = 'Lockouts')
  return(lockouts_frn_dates)
}

# ------------------------------ actualiza_cpi ------------------------------ 
# Actualiza el CPI diario en caso de que los 4 cpi's ya se hayan publicado 
actualiza_cpi <- function(cpi, name_cpi){
  cpi$cpi <- cpi$cpi %>%
    mutate(Fecha = as.Date(Fecha))
  
  fechas_cpi <- ceiling_date(bdp(cpi$paises$Ticker,
                                 'LAST_UPDATE_DT')[[1]] %m+%
                               months(2),
                             'month')
  
  # Verificamos si todos los CPI ya se publicaron
  # De esa forma ya podemos actualizar el histórico de CPI's 
  if(all(!(fechas_cpi %in% cpi$cpi$Fecha))){
    cpi_data <- tibble(Divisa = cpi$paises$Divisa,
                       Fecha = fechas_cpi,
                       CPI = bdp(cpi$paises$Ticker, 'PX_LAST')[[1]])
    
    cpi_last_data <- tibble(Divisa = tail(names(cpi$cpi),-1),
                            Fecha = tail(cpi$cpi$Fecha, 1),
                            CPI = as.numeric(tail(cpi$cpi,1)[1,-1]))
    
    
    cpi_combined_data <- bind_rows(cpi_data, cpi_last_data) %>%
      arrange(Divisa, Fecha)
    
    all_dates <- seq(min(cpi_combined_data$Fecha), 
                     max(cpi_combined_data$Fecha), by = "day")
    
    # Interpolamos 
    options(digits = 16)
    cpi_interpolado <- cpi_combined_data %>%
      group_by(Divisa) %>%
      complete(Fecha = all_dates) %>%
      arrange(Fecha) %>%
      mutate(CPI = approx(Fecha, CPI, xout = Fecha, rule = 2)$y,
             CPI = round2(truncate(CPI,6),5)) %>%
      ungroup()
    
    cpi_final <- cpi_interpolado %>% 
      pivot_wider(names_from = Divisa, values_from = CPI) %>%
      select(names(cpi$cpi))
    
    imprime_cpi(cpi_final, cpi, name_cpi)
    mes <- 'El CPI fue actualizado. Continuar con el proceso'
    
    shinyalert("EXITO", mes, type = "success")
  }else{
    mes <- 'El CPI ya se encontraba actualizado. Continuar con el proceso'
    
    shinyalert("EXITO", mes, type = "success")
  }
}

# ------------------------------ imprime_cpi ------------------------------ 
# Imprime el histórico de CPI diario en ruta 

imprime_cpi <- function(cpi_final, cpi, name_cpi){
  cpi <<- cpi
  # Seleccionamos la fecha y su tasa correspondiente para cada ISIN
  cpi_print <- tail(cpi_final,-1)
  n <- nrow(cpi$cpi)+2
  
  writeData(
    wb = cpi$cpi_wb,
    sheet = "CPI",
    cpi_print,
    startCol = 1,
    startRow = n,
    colNames = F,
    headerStyle = openxlsx_getOp("headerStyle")
  )
  
  saveWorkbook(cpi$cpi_wb, name_cpi, overwrite = T)
  
}