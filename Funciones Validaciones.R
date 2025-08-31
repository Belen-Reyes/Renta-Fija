# --------------------------------- lee_cup_ven -------------------------------
lee_cup_ven <- function(name_cup_ven, hoja){
  if(file.exists(name_cup_ven)){
    file <- read_excel(name_cup_ven, hoja)
  }else{
    mensaje <- 'No se encuentra el archivo de Cupones y Vencimientos. Cerrar la ventana, abrirla nuevamente y generar el archivo con los botones auxiliares'
    shinyalert("Error", mensaje, type = "error")
    while(!file.exists(name_cup_ven)){
      Sys.sleep(2)
    }
  }
  return(file)
}

# --------------------------------- lee_ic_ctd -------------------------------
lee_ic_ctd <- function(name_ic_ctd){
  if(file.exists(name_ic_ctd)){
    file <- read_excel(name_ic_ctd, skip = 2)
  }else{
    mensaje <- 'No se encuentra el archivo IC CTD. Cerrar la ventana, abrirla nuevamente y generar el archivo con los botones auxiliares'
    shinyalert("Error", mensaje, type = "error")
    while(!file.exists(name_ic_ctd)){
      Sys.sleep(2)
    }
  }
  return(file)
}


# --------------------------------- obtiene_mov2 -------------------------------

obtiene_mov2 <- function(comprasventas, cuponesvencimientos, port){
  if(nrow(comprasventas)==0){
    compras_ventas <- tibble(`ISIN` = 'US91282CFC01',
                             `Compras` = NA, 
                             `Ventas` = NA)
  }else{
    compras_ventas <- comprasventas %>%
      mutate(Monto = if_else(toolset %in% c('BondFut', 'RateFut'),
                             position,
                             notnl * position)) %>%
      group_by(isin, buy_sell) %>%
      summarize(Monto = sum(Monto)) %>%
      ungroup() %>%
      mutate(`Compras` = case_when(buy_sell == 'Buy' ~ Monto, 
                                   TRUE ~ 0),
             `Ventas` = case_when(buy_sell == 'Sell' ~ Monto, 
                                  TRUE ~ 0)) %>%
      select(isin, `Compras`, `Ventas`) %>%
      dplyr::rename(`ISIN` = isin)
  }
  
  if(nrow(cuponesvencimientos[[port]])==0){
    cup_ven <- tibble(`ISIN` = 'US91282CFC01',
                      `Cupones` = NA,
                      `Vencimientos` = NA)
  }else{
    cup_ven <- cuponesvencimientos[[port]] %>%
      select(`ISIN`, `Cupón BBG`, `Vencimiento BBG`) %>%
      dplyr::rename(`Cupones` = `Cupón BBG`,
                    `Vencimientos` = `Vencimiento BBG`)
  }
  
  movimientos <- compras_ventas %>%
    full_join(cup_ven, by = 'ISIN') %>%
    mutate_all(~replace(., is.na(.), 0))
  
  return(movimientos)
}

# --------------------------------- verifica_cup_ven -------------------------------
verifica_cup_ven <- function(cupones, vencimientos, cup_ven_bbg){
  # Obtenemos cupones y vencimientos de Findur 
  if(nrow(cupones)>0){
    cupones <- cupones %>%
      group_by(isin) %>%
      summarize(`Cupón Findur` = sum(position))
  }else{
    cupones <- tibble(isin = 'CA6832Z5L652',
                      `Cupón Findur` = 0)
  }
  
  if(nrow(vencimientos)>0){
    vencimientos <- vencimientos %>%
      group_by(isin) %>%
      summarize(`Vencimiento Findur` = sum(position))
  }else{
    vencimientos <- tibble(isin = 'CA6832Z5L652',
                           `Vencimiento Findur` = 0)
  }
  
  cup_ven_findur <- cupones %>%
    full_join(vencimientos, by = 'isin') %>%
    mutate_all(~replace(., is.na(.), 0))
  
  # Comparamos con los registrados en la carpeta de cupones y vencimientos
  if(nrow(cup_ven_bbg)==0){
    cup_ven_bbg <- tibble(ISIN = 'CA6832Z5L652',
                          `Cupón BBG` = 0,
                          `Vencimiento BBG` = 0)
  }
  
  comparativa_cup_ven <- cup_ven_bbg %>%
    full_join(cup_ven_findur %>%
                dplyr::rename(ISIN = isin), by = 'ISIN') %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    mutate(`Dif Cupon` = `Cupón Findur` - `Cupón BBG`,
           `Dif Vencimiento` = `Vencimiento Findur` - `Vencimiento BBG`)
  
  dif_cup_ven <- comparativa_cup_ven %>%
    filter(abs(`Dif Cupon`) > 1 | abs(`Dif Vencimiento`) > 1)
  
  if(nrow(dif_cup_ven) == 0){
    mensaje <- 'Se validaron correctamente los cupones y vencimientos'
    shinyalert("Exito", mensaje, type = "success")
    
  }else{
    mensaje <- c('Existen diferencias en el pago de cupon y/o vencimiento en los siguientes instrumentos: ', 
                 paste(dif_cup_ven$ISIN, collapse = " "))
    shinyalert("Error", mensaje, type = "error")
  }
}

# ------------------------------- modifica_cup_ven -----------------------------


# --------------------------------- verifica_ic -------------------------------
verifica_ic <- function(ic_hoy, ic_ayer, compras_ventas, cup_ven, date_ayer){
  cambio_ic <- ic_hoy %>%
    dplyr::rename(`Principal t` = Principal) %>%
    full_join(ic_ayer %>%
                dplyr::rename(`Principal t-1` = Principal)) %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    mutate(`Cambio Principal` = `Principal t` - `Principal t-1`)
  
  movimientos <- obtiene_mov2(compras_ventas,
                              cup_ven,
                              'goi')
  
  cambios <- cambio_ic %>% 
    left_join(movimientos, by = 'ISIN') %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    mutate(`Vencimientos aux` = if_else(`Vencimiento` <= date_ayer,
                                         `Principal t-1` * -1,
                                        0),
           Comprobacion = if_else(`Cambio Principal` != 0,
                                  `Cambio Principal` - (Compras + Ventas + Vencimientos +`Vencimientos aux`),
                                  0))
  dif_ic <- cambios %>%
    filter(Comprobacion != 0)
  
  if(nrow(dif_ic) == 0){
    mensaje <- 'Se validó correctamente la IC'
    shinyalert("Exito", mensaje, type = "success")
  }else{
    mensaje <- c('No se justificaron los cambios en la IC por compras/ventas en los siguientes instrumentos: ', 
                 paste(dif_ic$ISIN, collapse = " "))
    shinyalert("Error", mensaje, type = "error")
    }
}


# ------------------------------- verifica_ic_ctd -------------------------------
verifica_ic_ctd <- function(isin_ctd, isin_px){
  
  valida_ic_ctd <- setdiff(isin_ctd, 
                           isin_px)

  if(length(valida_ic_ctd) == 0){
    mensaje <- 'Se validó correctamente la IC_CTD'
    shinyalert("Exito", mensaje, type = "success")
  }else{
    mensaje <- c('Existen instrumentos en la IC_CTD que no existen en la PX: ', 
                 paste(valida_ic_ctd, collapse = " "))
    shinyalert("Error", mensaje, type = "error")
  }
}
