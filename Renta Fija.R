# RENTA FIJA 
Sys.setlocale("LC_ALL", "Spanish")
source('Z:/II Responsabilidades/26 BMK Global/Inversiones/Proyecto RF/Valuaciones y Resultados/Código/Funciones Renta Fija.R')
source('Z:/II Responsabilidades/26 BMK Global/Inversiones/Proyecto RF/CPI y FRN/Código/Funciones Tasas FRN y CPI.R')

# # Revisamos conexión a bbg ----
# library(Rblpapi)
# blpConnect()
# 
# # Fechas ----
# hol <- read_excel('Z:/II Responsabilidades/50 Liquidez/Feriados.xlsx', sheet =  1)
# date <- Sys.Date()
# date_ayer <- dates(hol, date,1,"resta",'MX')

# -------------------------------- obtiene_valua_ic ----------------------------
obtiene_valua_ic <- function(hol, date_ayer){
  date_anteayer <- dates(hol, date_ayer,1,"resta",'MX')
  
  # Datos
  div_dir <- c('EUR', 'GBP', 'AUD', 'NZD') # divisas directas de la cartera
  
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
    
    datos_inv = name_datos_inv(rutas, date_ayer),
    
    bv = name_bv(rutas, date_ayer), 
    
    futuros = name_findur_imp(rutas, date_ayer),
    
    ctd = name_ic_ctd(rutas, date_ayer),
    
    cup_ven = name_cup_ven(rutas, date_ayer),
    
    frn = name_frn(rutas),
    
    cpi = name_cpi(rutas),
    
    datos = name_datos(rutas),
    
    hist_tc = name_hist_tc(rutas),
    
    plantilla_val = name_plantilla_val(rutas),
    
    res_ic = name_res_ic(rutas, date_ayer),
    
    res_bmk = name_res_bmk(rutas, date_ayer),
    
    bmk = list(
      hoy = name_bmk(rutas, date_ayer),
      ayer = name_bmk(rutas, date_anteayer)
    ),
    val_ic = list(
      hoy = name_val_ic(rutas, date_ayer),
      ayer = name_val_ic(rutas, date_anteayer)
    ),
    val_bmk = list(
      hoy = name_val_bmk(rutas, date_ayer),
      ayer = name_val_bmk(rutas, date_anteayer),
      ayer_r = name_val_bmk_r(rutas, date_anteayer)
    ),
    px = list(
      hoy = name_px(rutas, date_ayer),
      ayer = name_px(rutas, date_anteayer)
    ),
    compara = list(
      hoy = name_compara(rutas, date_ayer),
      plantilla = name_compara_p(rutas)
    )
  )
  
  # Leemos archivos ----
  insumos <- list(ic = lee_ic(names$ic),
                  
                  datos_inv = lee_datos_inv(names),
                  
                  px_futuros = lee_findur_imp(names),
                  
                  bond_valuation = lee_bv(names),
                  
                  hist_tc = list(
                    hoy = lee_hist_tc(names, date_ayer, div_dir),
                    ayer = lee_hist_tc(names, date_anteayer, div_dir)
                  ),
                  
                  bmk = list(
                    hoy = lee_bmk(names$bmk$hoy),
                    ayer = lee_bmk(names$bmk$ayer)
                  ),
                  px = list(
                    hoy = lee_px(names$px$hoy),
                    ayer = lee_px(names$px$ayer)
                  ),
                  cup_ven = list(
                    goi = read_excel(names$cup_ven, 'GOI'),
                    bmk = read_excel(names$cup_ven, 'BMK')
                  ),
                  tasas_frn = read_excel(names$frn, 
                                         sheet = 'Tasas FRN'),
                  
                  cpi = read_excel(names$cpi, 
                                   sheet = 'CPI'),
                  
                  ic_fut_ctd = read_excel(names$ctd, 
                                          skip = 2),
                  
                  plantilla = list(
                    ic = read_excel(names$plantilla_val, sheet = 'Val IC'),
                    ic_ctd = read_excel(names$plantilla_val, sheet = 'Val IC CTD'),
                    resultados = read_excel(names$plantilla_val, sheet = 'Resultados'),
                    valuaciones= read_excel(names$plantilla_val, sheet = 'Valuaciones')
                  ))
  
  
  datos <- list(catalogo = read_excel(names$datos, sheet = 'Catalogo'),
                catalogo_res = read_excel(names$datos, sheet = 'Catalogo resultados'),
                divisas = read_excel(names$datos, sheet = 'Divisas'),
                convenciones = read_excel(names$datos, sheet = 'Convenciones'), 
                redondeo = read_excel(names$datos, sheet = 'Agencias redondeo'),
                catalogo_futuros = read_excel(names$datos, sheet = 'Catalogo futuros'))
  
  # VALUA IC -----
  val_ic <- genera_valua(insumos$ic,
                         datos,
                         insumos$px_futuros,
                         date_ayer,
                         insumos$ic_fut_ctd,
                         insumos,
                         insumos$hist_tc$hoy,
                         insumos$px$hoy,
                         names$val_ic$hoy, 
                         'goi')
}

# -------------------------------- obtiene_valua_bmk ----------------------------
obtiene_valua_bmk <- function(hol, date_ayer){
  date_anteayer <- dates(hol, date_ayer,1,"resta",'MX')
  
  # Datos
  div_dir <- c('EUR', 'GBP', 'AUD', 'NZD') # divisas directas de la cartera
  
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
    
    datos_inv = name_datos_inv(rutas, date_ayer),
    
    bv = name_bv(rutas, date_ayer), 
    
    futuros = name_findur_imp(rutas, date_ayer),
    
    ctd = name_ic_ctd(rutas, date_ayer),
    
    cup_ven = name_cup_ven(rutas, date_ayer),
    
    frn = name_frn(rutas),
    
    cpi = name_cpi(rutas),
    
    datos = name_datos(rutas),
    
    hist_tc = name_hist_tc(rutas),
    
    plantilla_val = name_plantilla_val(rutas),
    
    res_ic = name_res_ic(rutas, date_ayer),
    
    res_bmk = name_res_bmk(rutas, date_ayer),
    
    bmk = list(
      hoy = name_bmk(rutas, date_ayer),
      ayer = name_bmk(rutas, date_anteayer)
    ),
    val_ic = list(
      hoy = name_val_ic(rutas, date_ayer),
      ayer = name_val_ic(rutas, date_anteayer)
    ),
    val_bmk = list(
      hoy = name_val_bmk(rutas, date_ayer),
      ayer = name_val_bmk(rutas, date_anteayer),
      ayer_r = name_val_bmk_r(rutas, date_anteayer)
    ),
    px = list(
      hoy = name_px(rutas, date_ayer),
      ayer = name_px(rutas, date_anteayer)
    ),
    compara = list(
      hoy = name_compara(rutas, date_ayer),
      plantilla = name_compara_p(rutas)
    )
  )
  
  # Leemos archivos ----
  insumos <- list(ic = lee_ic(names$ic),
                  
                  datos_inv = lee_datos_inv(names),
                  
                  px_futuros = lee_findur_imp(names),
                  
                  bond_valuation = lee_bv(names),
                  
                  hist_tc = list(
                    hoy = lee_hist_tc(names, date_ayer, div_dir),
                    ayer = lee_hist_tc(names, date_anteayer, div_dir)
                  ),
                  
                  bmk = list(
                    hoy = lee_bmk(names$bmk$hoy),
                    ayer = lee_bmk(names$bmk$ayer)
                  ),
                  px = list(
                    hoy = lee_px(names$px$hoy),
                    ayer = lee_px(names$px$ayer)
                  ),
                  cup_ven = list(
                    goi = read_excel(names$cup_ven, 'GOI'),
                    bmk = read_excel(names$cup_ven, 'BMK')
                  ),
                  tasas_frn = read_excel(names$frn, 
                                         sheet = 'Tasas FRN'),
                  
                  cpi = read_excel(names$cpi, 
                                   sheet = 'CPI'),
                  
                  ic_fut_ctd = read_excel(names$ctd, 
                                          skip = 2),
                  
                  plantilla = list(
                    ic = read_excel(names$plantilla_val, sheet = 'Val IC'),
                    ic_ctd = read_excel(names$plantilla_val, sheet = 'Val IC CTD'),
                    resultados = read_excel(names$plantilla_val, sheet = 'Resultados'),
                    valuaciones= read_excel(names$plantilla_val, sheet = 'Valuaciones')
                  ))
  
  
  datos <- list(catalogo = read_excel(names$datos, sheet = 'Catalogo'),
                catalogo_res = read_excel(names$datos, sheet = 'Catalogo resultados'),
                divisas = read_excel(names$datos, sheet = 'Divisas'),
                convenciones = read_excel(names$datos, sheet = 'Convenciones'), 
                redondeo = read_excel(names$datos, sheet = 'Agencias redondeo'),
                catalogo_futuros = read_excel(names$datos, sheet = 'Catalogo futuros'))
  
  # VALUA BMK -----
  val_bmk <- genera_valua(insumos$bmk$hoy,
                          datos,
                          insumos$px_futuros,
                          date_ayer,
                          tibble(),
                          insumos,
                          insumos$hist_tc$hoy,
                          insumos$px$hoy,
                          names$val_bmk$hoy,
                          'bmk')
  
  if(names$bmk$hoy != names$bmk$ayer){
    val_bmk_r <- genera_valua(insumos$bmk$hoy,
                              datos,
                              insumos$px_futuros,
                              date_anteayer,
                              tibble(),
                              insumos,
                              insumos$hist_tc$ayer,
                              insumos$px$ayer,
                              names$val_bmk$ayer_r,
                              'bmk')
  }
}

# -------------------------------- obtiene_valua_bmk_cp6 ----------------------------
obtiene_valua_bmk_cp6 <- function(hol, date_ayer){
  date_anteayer <- dates(hol, date_ayer,1,"resta",'MX')
  
  # Datos
  div_dir <- c('EUR', 'GBP', 'AUD', 'NZD') # divisas directas de la cartera
  
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
    
    datos_inv = name_datos_inv(rutas, date_ayer),
    
    bv = name_bv(rutas, date_ayer), 
    
    futuros = name_findur_imp(rutas, date_ayer),
    
    ctd = name_ic_ctd(rutas, date_ayer),
    
    cup_ven = name_cup_ven(rutas, date_ayer),
    
    frn = name_frn(rutas),
    
    cpi = name_cpi(rutas),
    
    datos = name_datos(rutas),
    
    hist_tc = name_hist_tc(rutas),
    
    plantilla_val = name_plantilla_val(rutas),
    
    res_ic = name_res_ic(rutas, date_ayer),
    
    res_bmk = name_res_bmk(rutas, date_ayer),
    
    bmk = list(
      hoy = name_bmk(rutas, date_ayer),
      ayer = name_bmk(rutas, date_anteayer)
    ),
    
    bmk_cp6 = list(
      hoy = name_bmk_cp6(rutas, date_ayer),
      ayer = name_bmk_cp6(rutas, date_anteayer)
    ), 
    
    val_ic = list(
      hoy = name_val_ic(rutas, date_ayer),
      ayer = name_val_ic(rutas, date_anteayer)
    ),
    val_bmk = list(
      hoy = name_val_bmk(rutas, date_ayer),
      ayer = name_val_bmk(rutas, date_anteayer),
      ayer_r = name_val_bmk_r(rutas, date_anteayer)
    ),
    val_bmk_cp6 = list(
      hoy = name_val_bmk_cp6(rutas, date_ayer),
      ayer = name_val_bmk_cp6(rutas, date_anteayer),
      ayer_r = name_val_bmk_r_cp6(rutas, date_anteayer)
    ),
    px = list(
      hoy = name_px(rutas, date_ayer),
      ayer = name_px(rutas, date_anteayer)
    ),
    compara = list(
      hoy = name_compara(rutas, date_ayer),
      plantilla = name_compara_p(rutas)
    )
  )
  
  # Leemos archivos ----
  insumos <- list(ic = lee_ic(names$ic),
                  
                  datos_inv = lee_datos_inv(names),
                  
                  px_futuros = lee_findur_imp(names),
                  
                  bond_valuation = lee_bv(names),
                  
                  hist_tc = list(
                    hoy = lee_hist_tc(names, date_ayer, div_dir),
                    ayer = lee_hist_tc(names, date_anteayer, div_dir)
                  ),
                  
                  bmk = list(
                    hoy = lee_bmk(names$bmk$hoy),
                    ayer = lee_bmk(names$bmk$ayer)
                  ),
                  
                  bmk_cp6 = list(
                    hoy = lee_bmk(names$bmk_cp6$hoy),
                    ayer = lee_bmk(names$bmk_cp6$ayer)
                  ),
                  px = list(
                    hoy = lee_px(names$px$hoy),
                    ayer = lee_px(names$px$ayer)
                  ),
                  cup_ven = list(
                    goi = read_excel(names$cup_ven, 'GOI'),
                    bmk = read_excel(names$cup_ven, 'BMK')
                  ),
                  tasas_frn = read_excel(names$frn, 
                                         sheet = 'Tasas FRN'),
                  
                  cpi = read_excel(names$cpi, 
                                   sheet = 'CPI'),
                  
                  ic_fut_ctd = read_excel(names$ctd, 
                                          skip = 2),
                  
                  plantilla = list(
                    ic = read_excel(names$plantilla_val, sheet = 'Val IC'),
                    ic_ctd = read_excel(names$plantilla_val, sheet = 'Val IC CTD'),
                    resultados = read_excel(names$plantilla_val, sheet = 'Resultados'),
                    valuaciones= read_excel(names$plantilla_val, sheet = 'Valuaciones')
                  ))
  
  
  datos <- list(catalogo = read_excel(names$datos, sheet = 'Catalogo'),
                catalogo_res = read_excel(names$datos, sheet = 'Catalogo resultados'),
                divisas = read_excel(names$datos, sheet = 'Divisas'),
                convenciones = read_excel(names$datos, sheet = 'Convenciones'), 
                redondeo = read_excel(names$datos, sheet = 'Agencias redondeo'),
                catalogo_futuros = read_excel(names$datos, sheet = 'Catalogo futuros'))
  
  # VALUA BMK CP6 -----
  val_bmk_cp6 <- genera_valua(insumos$bmk_cp6$hoy,
                          datos,
                          insumos$px_futuros,
                          date_ayer,
                          tibble(),
                          insumos,
                          insumos$hist_tc$hoy,
                          insumos$px$hoy,
                          names$val_bmk_cp6$hoy,
                          'bmk')
  
  if(names$bmk_cp6$hoy != names$bmk_cp6$ayer){
    val_bmk_r_cp6 <- genera_valua(insumos$bmk_cp6$hoy,
                              datos,
                              insumos$px_futuros,
                              date_anteayer,
                              tibble(),
                              insumos,
                              insumos$hist_tc$ayer,
                              insumos$px$ayer,
                              names$val_bmk_cp6$ayer_r,
                              'bmk')
  }
}
# -------------------------------- obtiene_compara_ic ----------------------------
obtiene_compara_ic <- function(hol, date_ayer){
  
  date_anteayer <- dates(hol, date_ayer,1,"resta",'MX')
  # Datos
  div_dir <- c('EUR', 'GBP', 'AUD', 'NZD') # divisas directas de la cartera
  
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
    
    datos_inv = name_datos_inv(rutas, date_ayer),
    
    bv = name_bv(rutas, date_ayer), 
    
    futuros = name_findur_imp(rutas, date_ayer),
    
    ctd = name_ic_ctd(rutas, date_ayer),
    
    cup_ven = name_cup_ven(rutas, date_ayer),
    
    frn = name_frn(rutas),
    
    cpi = name_cpi(rutas),
    
    datos = name_datos(rutas),
    
    hist_tc = name_hist_tc(rutas),
    
    plantilla_val = name_plantilla_val(rutas),
    
    res_ic = name_res_ic(rutas, date_ayer),
    
    res_bmk = name_res_bmk(rutas, date_ayer),
    
    bmk = list(
      hoy = name_bmk(rutas, date_ayer),
      ayer = name_bmk(rutas, date_anteayer)
    ),
    val_ic = list(
      hoy = name_val_ic(rutas, date_ayer),
      ayer = name_val_ic(rutas, date_anteayer)
    ),
    val_bmk = list(
      hoy = name_val_bmk(rutas, date_ayer),
      ayer = name_val_bmk(rutas, date_anteayer),
      ayer_r = name_val_bmk_r(rutas, date_anteayer)
    ),
    px = list(
      hoy = name_px(rutas, date_ayer),
      ayer = name_px(rutas, date_anteayer)
    ),
    compara = list(
      hoy = name_compara(rutas, date_ayer),
      plantilla = name_compara_p(rutas)
    )
  )
  
  # Leemos archivos ----
  insumos <- list(ic = lee_ic(names$ic),
                  
                  datos_inv = lee_datos_inv(names),
                  
                  px_futuros = lee_findur_imp(names),
                  
                  bond_valuation = lee_bv(names),
                  
                  hist_tc = list(
                    hoy = lee_hist_tc(names, date_ayer, div_dir),
                    ayer = lee_hist_tc(names, date_anteayer, div_dir)
                  ),
                  
                  bmk = list(
                    hoy = lee_bmk(names$bmk$hoy),
                    ayer = lee_bmk(names$bmk$ayer)
                  ),
                  px = list(
                    hoy = lee_px(names$px$hoy),
                    ayer = lee_px(names$px$ayer)
                  ),
                  cup_ven = list(
                    goi = read_excel(names$cup_ven, 'GOI'),
                    bmk = read_excel(names$cup_ven, 'BMK')
                  ),
                  tasas_frn = read_excel(names$frn, 
                                         sheet = 'Tasas FRN'),
                  
                  cpi = read_excel(names$cpi, 
                                   sheet = 'CPI'),
                  
                  ic_fut_ctd = read_excel(names$ctd, 
                                          skip = 2),
                  
                  plantilla = list(
                    ic = read_excel(names$plantilla_val, sheet = 'Val IC'),
                    ic_ctd = read_excel(names$plantilla_val, sheet = 'Val IC CTD'),
                    resultados = read_excel(names$plantilla_val, sheet = 'Resultados'),
                    valuaciones= read_excel(names$plantilla_val, sheet = 'Valuaciones')
                  ))
  
  
  datos <- list(catalogo = read_excel(names$datos, sheet = 'Catalogo'),
                catalogo_res = read_excel(names$datos, sheet = 'Catalogo resultados'),
                divisas = read_excel(names$datos, sheet = 'Divisas'),
                convenciones = read_excel(names$datos, sheet = 'Convenciones'), 
                redondeo = read_excel(names$datos, sheet = 'Agencias redondeo'),
                catalogo_futuros = read_excel(names$datos, sheet = 'Catalogo futuros'))
  
  # COMPARA -----------------------
  val_ic <- read_csv(names$val_ic$hoy)
  compara_list <- crea_compara(val_ic, 
                               insumos$bond_valuation)
  compara(compara_list$compara_val,
          compara_list$compara_dif,
          names$compara$plantilla,
          names$compara$hoy)
}

# ------------------------------ corrige_valuacion_ic ---------------------------
corrige_valuacion_ic <- function(hol, date_ayer){
  
  date_anteayer <- dates(hol, date_ayer,1,"resta",'MX')
  # Datos
  div_dir <- c('EUR', 'GBP', 'AUD', 'NZD') # divisas directas de la cartera
  
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
    
    datos_inv = name_datos_inv(rutas, date_ayer),
    
    bv = name_bv(rutas, date_ayer), 
    
    futuros = name_findur_imp(rutas, date_ayer),
    
    ctd = name_ic_ctd(rutas, date_ayer),
    
    cup_ven = name_cup_ven(rutas, date_ayer),
    
    frn = name_frn(rutas),
    
    cpi = name_cpi(rutas),
    
    datos = name_datos(rutas),
    
    hist_tc = name_hist_tc(rutas),
    
    plantilla_val = name_plantilla_val(rutas),
    
    res_ic = name_res_ic(rutas, date_ayer),
    
    res_bmk = name_res_bmk(rutas, date_ayer),
    
    bmk = list(
      hoy = name_bmk(rutas, date_ayer),
      ayer = name_bmk(rutas, date_anteayer)
    ),
    val_ic = list(
      hoy = name_val_ic(rutas, date_ayer),
      ayer = name_val_ic(rutas, date_anteayer)
    ),
    val_bmk = list(
      hoy = name_val_bmk(rutas, date_ayer),
      ayer = name_val_bmk(rutas, date_anteayer),
      ayer_r = name_val_bmk_r(rutas, date_anteayer)
    ),
    px = list(
      hoy = name_px(rutas, date_ayer),
      ayer = name_px(rutas, date_anteayer)
    ),
    compara = list(
      hoy = name_compara(rutas, date_ayer),
      plantilla = name_compara_p(rutas)
    )
  )
  
  # Leemos archivos ----
  insumos <- list(ic = lee_ic(names$ic),
                  
                  datos_inv = lee_datos_inv(names),
                  
                  px_futuros = lee_findur_imp(names),
                  
                  bond_valuation = lee_bv(names),
                  
                  hist_tc = list(
                    hoy = lee_hist_tc(names, date_ayer, div_dir),
                    ayer = lee_hist_tc(names, date_anteayer, div_dir)
                  ),
                  
                  bmk = list(
                    hoy = lee_bmk(names$bmk$hoy),
                    ayer = lee_bmk(names$bmk$ayer)
                  ),
                  px = list(
                    hoy = lee_px(names$px$hoy),
                    ayer = lee_px(names$px$ayer)
                  ),
                  cup_ven = list(
                    goi = read_excel(names$cup_ven, 'GOI'),
                    bmk = read_excel(names$cup_ven, 'BMK')
                  ),
                  tasas_frn = read_excel(names$frn, 
                                         sheet = 'Tasas FRN'),
                  
                  cpi = read_excel(names$cpi, 
                                   sheet = 'CPI'),
                  
                  ic_fut_ctd = read_excel(names$ctd, 
                                          skip = 2),
                  
                  plantilla = list(
                    ic = read_excel(names$plantilla_val, sheet = 'Val IC'),
                    ic_ctd = read_excel(names$plantilla_val, sheet = 'Val IC CTD'),
                    resultados = read_excel(names$plantilla_val, sheet = 'Resultados'),
                    valuaciones= read_excel(names$plantilla_val, sheet = 'Valuaciones')
                  ))
  
  
  datos <- list(catalogo = read_excel(names$datos, sheet = 'Catalogo'),
                catalogo_res = read_excel(names$datos, sheet = 'Catalogo resultados'),
                divisas = read_excel(names$datos, sheet = 'Divisas'),
                convenciones = read_excel(names$datos, sheet = 'Convenciones'), 
                redondeo = read_excel(names$datos, sheet = 'Agencias redondeo'),
                catalogo_futuros = read_excel(names$datos, sheet = 'Catalogo futuros'))
  
  # CORRIGE COMPARA ---------------------
  val_ic <- read_csv(names$val_ic$hoy)
  compara_aux <- read_excel(names$compara$hoy, 
                            sheet = 'Compara Auxiliar')
  
  val_ic_nuevo <- cambia_valuacion(val_ic, 
                                   compara_aux, 
                                   insumos$hist_tc$hoy,
                                   names$val_ic$hoy,
                                   datos)
  
  compara_corregido(val_ic_nuevo,
                    insumos$bond_valuation,
                    names$compara$hoy)
}

# ---------------------------- obtiene_resultados_ic ---------------------------
obtiene_resultados_ic <- function(hol, date_ayer){
  
  date_anteayer <- dates(hol, date_ayer,1,"resta",'MX')
  # Datos
  div_dir <- c('EUR', 'GBP', 'AUD', 'NZD') # divisas directas de la cartera
  
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
    
    datos_inv = name_datos_inv(rutas, date_ayer),
    
    bv = name_bv(rutas, date_ayer), 
    
    futuros = name_findur_imp(rutas, date_ayer),
    
    ctd = name_ic_ctd(rutas, date_ayer),
    
    cup_ven = name_cup_ven(rutas, date_ayer),
    
    frn = name_frn(rutas),
    
    cpi = name_cpi(rutas),
    
    datos = name_datos(rutas),
    
    hist_tc = name_hist_tc(rutas),
    
    plantilla_val = name_plantilla_val(rutas),
    
    res_ic = name_res_ic(rutas, date_ayer),
    
    res_bmk = name_res_bmk(rutas, date_ayer),
    
    bmk = list(
      hoy = name_bmk(rutas, date_ayer),
      ayer = name_bmk(rutas, date_anteayer)
    ),
    val_ic = list(
      hoy = name_val_ic(rutas, date_ayer),
      ayer = name_val_ic(rutas, date_anteayer)
    ),
    val_bmk = list(
      hoy = name_val_bmk(rutas, date_ayer),
      ayer = name_val_bmk(rutas, date_anteayer),
      ayer_r = name_val_bmk_r(rutas, date_anteayer)
    ),
    px = list(
      hoy = name_px(rutas, date_ayer),
      ayer = name_px(rutas, date_anteayer)
    ),
    compara = list(
      hoy = name_compara(rutas, date_ayer),
      plantilla = name_compara_p(rutas)
    )
  )
  
  # Leemos archivos ----
  insumos <- list(ic = lee_ic(names$ic),
                  
                  datos_inv = lee_datos_inv(names),
                  
                  px_futuros = lee_findur_imp(names),
                  
                  bond_valuation = lee_bv(names),
                  
                  hist_tc = list(
                    hoy = lee_hist_tc(names, date_ayer, div_dir),
                    ayer = lee_hist_tc(names, date_anteayer, div_dir)
                  ),
                  
                  bmk = list(
                    hoy = lee_bmk(names$bmk$hoy),
                    ayer = lee_bmk(names$bmk$ayer)
                  ),
                  px = list(
                    hoy = lee_px(names$px$hoy),
                    ayer = lee_px(names$px$ayer)
                  ),
                  cup_ven = list(
                    goi = read_excel(names$cup_ven, 'GOI'),
                    bmk = read_excel(names$cup_ven, 'BMK')
                  ),
                  tasas_frn = read_excel(names$frn, 
                                         sheet = 'Tasas FRN'),
                  
                  cpi = read_excel(names$cpi, 
                                   sheet = 'CPI'),
                  
                  ic_fut_ctd = read_excel(names$ctd, 
                                          skip = 2),
                  
                  plantilla = list(
                    ic = read_excel(names$plantilla_val, sheet = 'Val IC'),
                    ic_ctd = read_excel(names$plantilla_val, sheet = 'Val IC CTD'),
                    resultados = read_excel(names$plantilla_val, sheet = 'Resultados'),
                    valuaciones= read_excel(names$plantilla_val, sheet = 'Valuaciones'))
                  )
  
  
  datos <- list(catalogo = read_excel(names$datos, sheet = 'Catalogo'),
                catalogo_res = read_excel(names$datos, sheet = 'Catalogo resultados'),
                divisas = read_excel(names$datos, sheet = 'Divisas'),
                convenciones = read_excel(names$datos, sheet = 'Convenciones'), 
                redondeo = read_excel(names$datos, sheet = 'Agencias redondeo'),
                catalogo_futuros = read_excel(names$datos, sheet = 'Catalogo futuros'))
  
  # RESULTADOS GOI ----
  resultados_goi <- genera_resultados(names$val_ic$hoy,
                                      names$val_ic$ayer,
                                      names$res_ic,
                                      insumos$datos_inv$comprasventas,
                                      insumos$cup_ven,
                                      insumos, 
                                      datos,
                                      'goi')
}

# ---------------------------- obtiene_resultados_bmk ---------------------------
obtiene_resultados_bmk <- function(hol, date_ayer){
  
  date_anteayer <- dates(hol, date_ayer,1,"resta",'MX')
  # Datos
  div_dir <- c('EUR', 'GBP', 'AUD', 'NZD') # divisas directas de la cartera
  
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
    
    datos_inv = name_datos_inv(rutas, date_ayer),
    
    bv = name_bv(rutas, date_ayer), 
    
    futuros = name_findur_imp(rutas, date_ayer),
    
    ctd = name_ic_ctd(rutas, date_ayer),
    
    cup_ven = name_cup_ven(rutas, date_ayer),
    
    frn = name_frn(rutas),
    
    cpi = name_cpi(rutas),
    
    datos = name_datos(rutas),
    
    hist_tc = name_hist_tc(rutas),
    
    plantilla_val = name_plantilla_val(rutas),
    
    res_ic = name_res_ic(rutas, date_ayer),
    
    res_bmk = name_res_bmk(rutas, date_ayer),
    
    bmk = list(
      hoy = name_bmk(rutas, date_ayer),
      ayer = name_bmk(rutas, date_anteayer)
    ),
    val_ic = list(
      hoy = name_val_ic(rutas, date_ayer),
      ayer = name_val_ic(rutas, date_anteayer)
    ),
    val_bmk = list(
      hoy = name_val_bmk(rutas, date_ayer),
      ayer = name_val_bmk(rutas, date_anteayer),
      ayer_r = name_val_bmk_r(rutas, date_anteayer)
    ),
    px = list(
      hoy = name_px(rutas, date_ayer),
      ayer = name_px(rutas, date_anteayer)
    ),
    compara = list(
      hoy = name_compara(rutas, date_ayer),
      plantilla = name_compara_p(rutas)
    )
  )
  
  # Leemos archivos ----
  insumos <- list(ic = lee_ic(names$ic),
                  
                  datos_inv = lee_datos_inv(names),
                  
                  px_futuros = lee_findur_imp(names),
                  
                  bond_valuation = lee_bv(names),
                  
                  hist_tc = list(
                    hoy = lee_hist_tc(names, date_ayer, div_dir),
                    ayer = lee_hist_tc(names, date_anteayer, div_dir)
                  ),
                  
                  bmk = list(
                    hoy = lee_bmk(names$bmk$hoy),
                    ayer = lee_bmk(names$bmk$ayer)
                  ),
                  px = list(
                    hoy = lee_px(names$px$hoy),
                    ayer = lee_px(names$px$ayer)
                  ),
                  cup_ven = list(
                    goi = read_excel(names$cup_ven, 'GOI'),
                    bmk = read_excel(names$cup_ven, 'BMK')
                  ),
                  tasas_frn = read_excel(names$frn, 
                                         sheet = 'Tasas FRN'),
                  
                  cpi = read_excel(names$cpi, 
                                   sheet = 'CPI'),
                  
                  ic_fut_ctd = read_excel(names$ctd, 
                                          skip = 2),
                  
                  plantilla = list(
                    ic = read_excel(names$plantilla_val, sheet = 'Val IC'),
                    ic_ctd = read_excel(names$plantilla_val, sheet = 'Val IC CTD'),
                    resultados = read_excel(names$plantilla_val, sheet = 'Resultados'),
                    valuaciones= read_excel(names$plantilla_val, sheet = 'Valuaciones')
                  ))
  
  
  datos <- list(catalogo = read_excel(names$datos, sheet = 'Catalogo'),
                catalogo_res = read_excel(names$datos, sheet = 'Catalogo resultados'),
                divisas = read_excel(names$datos, sheet = 'Divisas'),
                convenciones = read_excel(names$datos, sheet = 'Convenciones'), 
                redondeo = read_excel(names$datos, sheet = 'Agencias redondeo'),
                catalogo_futuros = read_excel(names$datos, sheet = 'Catalogo futuros'))
  
  # Resultados BMK ----
  if(names$bmk$hoy != names$bmk$ayer){
    resultados_bmk <- genera_resultados(names$val_bmk$hoy,
                                        names$val_bmk$ayer_r,
                                        names$res_bmk,
                                        tibble(),
                                        insumos$cup_ven,
                                        insumos, 
                                        datos,
                                        'bmk')
  }else{
    resultados_bmk <- genera_resultados(names$val_bmk$hoy,
                                        names$val_bmk$ayer,
                                        names$res_bmk,
                                        tibble(),
                                        insumos$cup_ven,
                                        insumos, 
                                        datos,
                                        'bmk')
  }
}

# ---------------------------- obtiene_resultados_bmk_cp6 ---------------------------
obtiene_resultados_bmk_cp6 <- function(hol, date_ayer){
  
  date_anteayer <- dates(hol, date_ayer,1,"resta",'MX')
  # Datos
  div_dir <- c('EUR', 'GBP', 'AUD', 'NZD') # divisas directas de la cartera
  
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
    
    datos_inv = name_datos_inv(rutas, date_ayer),
    
    bv = name_bv(rutas, date_ayer), 
    
    futuros = name_findur_imp(rutas, date_ayer),
    
    ctd = name_ic_ctd(rutas, date_ayer),
    
    cup_ven = name_cup_ven(rutas, date_ayer),
    
    frn = name_frn(rutas),
    
    cpi = name_cpi(rutas),
    
    datos = name_datos(rutas),
    
    hist_tc = name_hist_tc(rutas),
    
    plantilla_val = name_plantilla_val(rutas),
    
    res_ic = name_res_ic(rutas, date_ayer),
    
    res_bmk = name_res_bmk(rutas, date_ayer),
    
    res_bmk_cp6 = name_res_bmk_cp6(rutas, date_ayer),
    
    bmk = list(
      hoy = name_bmk(rutas, date_ayer),
      ayer = name_bmk(rutas, date_anteayer)
    ),
    
    bmk_cp6 = list(
      hoy = name_bmk_cp6(rutas, date_ayer),
      ayer = name_bmk_cp6(rutas, date_anteayer)
    ),
    val_ic = list(
      hoy = name_val_ic(rutas, date_ayer),
      ayer = name_val_ic(rutas, date_anteayer)
    ),
    val_bmk = list(
      hoy = name_val_bmk(rutas, date_ayer),
      ayer = name_val_bmk(rutas, date_anteayer),
      ayer_r = name_val_bmk_r(rutas, date_anteayer)
    ),
    val_bmk_cp6 = list(
      hoy = name_val_bmk_cp6(rutas, date_ayer),
      ayer = name_val_bmk_cp6(rutas, date_anteayer),
      ayer_r = name_val_bmk_r_cp6(rutas, date_anteayer)
    ),
    px = list(
      hoy = name_px(rutas, date_ayer),
      ayer = name_px(rutas, date_anteayer)
    ),
    compara = list(
      hoy = name_compara(rutas, date_ayer),
      plantilla = name_compara_p(rutas)
    )
  )
  
  # Leemos archivos ----
  insumos <- list(ic = lee_ic(names$ic),
                  
                  datos_inv = lee_datos_inv(names),
                  
                  px_futuros = lee_findur_imp(names),
                  
                  bond_valuation = lee_bv(names),
                  
                  hist_tc = list(
                    hoy = lee_hist_tc(names, date_ayer, div_dir),
                    ayer = lee_hist_tc(names, date_anteayer, div_dir)
                  ),
                  
                  bmk = list(
                    hoy = lee_bmk(names$bmk$hoy),
                    ayer = lee_bmk(names$bmk$ayer)
                  ),
                  bmk_cp6 = list(
                    hoy = lee_bmk(names$bmk_cp6$hoy),
                    ayer = lee_bmk(names$bmk_cp6$ayer)
                  ),
                  px = list(
                    hoy = lee_px(names$px$hoy),
                    ayer = lee_px(names$px$ayer)
                  ),
                  cup_ven = list(
                    goi = read_excel(names$cup_ven, 'GOI'),
                    bmk = read_excel(names$cup_ven, 'BMK')
                  ),
                  tasas_frn = read_excel(names$frn, 
                                         sheet = 'Tasas FRN'),
                  
                  cpi = read_excel(names$cpi, 
                                   sheet = 'CPI'),
                  
                  ic_fut_ctd = read_excel(names$ctd, 
                                          skip = 2),
                  
                  plantilla = list(
                    ic = read_excel(names$plantilla_val, sheet = 'Val IC'),
                    ic_ctd = read_excel(names$plantilla_val, sheet = 'Val IC CTD'),
                    resultados = read_excel(names$plantilla_val, sheet = 'Resultados'),
                    valuaciones= read_excel(names$plantilla_val, sheet = 'Valuaciones')
                  ))
  
  
  datos <- list(catalogo = read_excel(names$datos, sheet = 'Catalogo'),
                catalogo_res = read_excel(names$datos, sheet = 'Catalogo resultados'),
                divisas = read_excel(names$datos, sheet = 'Divisas'),
                convenciones = read_excel(names$datos, sheet = 'Convenciones'), 
                redondeo = read_excel(names$datos, sheet = 'Agencias redondeo'),
                catalogo_futuros = read_excel(names$datos, sheet = 'Catalogo futuros'))
  
  # Resultados BMK CP6 ----
  if(names$bmk_cp6$hoy != names$bmk_cp6$ayer){
    resultados_bmk_cp6 <- genera_resultados(names$val_bmk_cp6$hoy,
                                        names$val_bmk_cp6$ayer_r,
                                        names$res_bmk_cp6,
                                        tibble(),
                                        insumos$cup_ven,
                                        insumos, 
                                        datos,
                                        'bmk')
  }else{
    resultados_bmk_cp6 <- genera_resultados(names$val_bmk_cp6$hoy,
                                        names$val_bmk_cp6$ayer,
                                        names$res_bmk_cp6,
                                        tibble(),
                                        insumos$cup_ven,
                                        insumos, 
                                        datos,
                                        'bmk')
  }
}







