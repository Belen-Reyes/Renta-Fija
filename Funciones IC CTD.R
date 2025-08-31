# ------------------------------- genera_ic_ctd --------------------------------
genera_ic_ctd <- function(hol, date){
  # Rutas ----
  rutas <- list(
    j = 'J:/outdir/reports/',
    px = 'Z:/II Responsabilidades/2 Inversiones/datos/precios/',
    global = 'Z:/II Responsabilidades/26 BMK Global/',
    futuros = 'J:/sysdir/import/precios/px_findur/',
    proyecto = 'Z:/II Responsabilidades/26 BMK Global/Inversiones/Proyecto RF/'
  )
  
  # Nombres archivos ----
  names <- list(ic = name_ic(rutas, date),
                datos = name_datos(rutas),
                ctd = name_ic_ctd(rutas, date),
                ctd_plantilla = names_plantilla_ic_ctd(rutas))
  
  # Archivos ----
  insumos <- list(ic = lee_ic(names$ic))
  
  datos <- list(catalogo_futuros = read_excel(names$datos, sheet = 'Catalogo futuros'))
  
  ctd_plantilla <- list(wb = loadWorkbook(names$ctd_plantilla),
                        xlsx = read_excel(names$ctd_plantilla, skip = 2))
  
  # Obtenemos futuros 
  ic_fut <- insumos$ic %>%
    filter(grepl('FUT', Instrumento))
  
  if(nrow(ic_fut) > 0){
    
    ic_fut_bonos <- ic_fut %>%
      mutate(Futuro = substr(ISIN, 1, nchar(ISIN)-2)) %>%
      left_join(datos$catalogo_futuros, by = 'Futuro') %>%
      filter(Tipo == 'Bonos') %>%
      select(ISIN, Principal, `Contract Size`, Futuro)
    
    if(nrow(ic_fut_bonos) > 0){
      ic_fut_ctd <- ic_fut_bonos %>%
        mutate(`Principal` = Principal*`Contract Size`,
               `ISIN CTD` = futuros[['obtiene_ctd']](ISIN),
               `Instrumento` = futuros[['obtiene_instrumento']](ISIN),
               `Factor de conversion` = futuros[['obtiene_conv_fac']](ISIN),
               Cupon = futuros[['obtiene_cupon']](`ISIN CTD`),
               Frecuencia = futuros[['obtiene_freq']](`ISIN CTD`),
               Vencimiento = futuros[['obtiene_maturity']](`ISIN CTD`),
               Vencimiento = as.Date(Vencimiento)
        ) %>%
        dplyr::rename(`ISIN del futuro` = ISIN,
                      ISIN = `ISIN CTD`, 
                      Identificador = Futuro)
      
      ic_fut_ctd <- ic_fut_ctd %>%
        select(names(ctd_plantilla$xlsx))
      
      # Imprimimos
      imprime_ic_ctd(date, ctd_plantilla$wb, ic_fut_ctd, names$ctd)
      
    }else{
      # Imprimimos
      imprime_fecha_ctd(date, ctd_plantilla$wb, names$ctd)
    }
    
  }else{
    # Imprimimos
    imprime_fecha_ctd(date, ctd_plantilla$wb, names$ctd)
  }
  
}