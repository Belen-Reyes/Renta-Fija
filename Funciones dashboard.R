source('Z:/II Responsabilidades/26 BMK Global/Inversiones/Proyecto RF/Valuaciones y Resultados/Código/Renta Fija.R')
source('Z:/II Responsabilidades/26 BMK Global/Inversiones/Proyecto RF/CPI y FRN/Código/Tasas FRN y CPI.R')
source('Z:/II Responsabilidades/26 BMK Global/Inversiones/Proyecto RF/Validaciones/Validaciones.R')

name_rpt <- function(fecha){
  Sys.setlocale("LC_TIME", "English")
  ruta <- paste0('J:/outdir/reports/', 
                 format(fecha, "%y%b%d"), 
                 '/',
                 'Rpt_TCyTASAS_', 
                 format(fecha, "%Y%m%d"),
                 '.csv')
  return(ruta)
}

name_resultados <- function(fecha, port){
  
  if(port == 'GOI'){
    ruta <- paste0('Z:/II Responsabilidades/26 BMK Global/Global/GOI/Valua/',
                   'Resultados_',
                   format(fecha, "%Y%m%d"), 
                   '.csv')
  }else{
    ruta <- paste0('Z:/II Responsabilidades/26 BMK Global/Global/BMK/Valua/',
                   'ResultadosBMK_',
                   format(fecha, "%Y%m%d"), 
                   '.csv')
  }
  
  
  return(ruta)
}

obtiene_tabla_resultados <- function(nombre_res){
  resultados <- read_csv(nombre_res) %>%
    group_by(`Tipo de instrumento`) %>%
    summarize(`Resultado Tasa` = sum(`Resultado tasa`),
              `Resultado Cambiario` = sum(`Resultado cambiario`)) %>%
    mutate_if(is.numeric, ~ . / 1e6)
  
  return(resultados)
}

obtiene_totales <- function(tabla_resultados){
  totales <- tibble(
    `Resultado Total` = sum(tabla_resultados$`Resultado Tasa`) + sum(tabla_resultados$`Resultado Cambiario`),
    `Resultado Tasa` = sum(tabla_resultados$`Resultado Tasa`),
    `Resultado Cambiario` = sum(tabla_resultados$`Resultado Cambiario`)
  )
  
  return(totales)
  
}

obtiene_tabla_movimientos <- function(nombre_res){
  resultados <- read_csv(nombre_res) %>%
    group_by(`Tipo de instrumento`) %>%
    summarize(`Cantidad Compras USD` = sum(`Compras USD` != 0),
              `Cantidad Ventas USD` = sum(`Ventas USD` != 0),
              `Cantidad Cupones USD` = sum(`Cupones USD` != 0)) %>%
    filter(`Cantidad Compras USD` != 0 | `Cantidad Ventas USD` != 0 | `Cantidad Cupones USD` != 0)
  
  return(resultados)
}

obtiene_mov_curvas <- function(hol, date_ayer){
  date_anteayer <- dates(hol, date_ayer,1,"resta",'MX')
  div_dir <- c('EUR', 'GBP', 'AUD', 'NZD')
  
  # Rutas ----
  rutas <- list(
    j = 'J:/outdir/reports/',
    px = 'Z:/II Responsabilidades/2 Inversiones/datos/precios/',
    global = 'Z:/II Responsabilidades/26 BMK Global/',
    futuros = 'J:/sysdir/import/precios/px_findur/',
    proyecto = 'Z:/II Responsabilidades/26 BMK Global/Inversiones/Proyecto RF/'
  )
  
  # Nombres ----
  names <- list(
    curvas = list(
      hoy = name_rpt(date_ayer),
      ayer = name_rpt(date_anteayer)
    ),
    hist_tc = name_hist_tc(rutas),
    datos = name_datos(rutas)
    )
  
  # Insumos ----
  insumos <- list(
    curvas = list(
      hoy = read_csv(names$curvas$hoy),
      ayer = read_csv(names$curvas$ayer)
    ),
    divisas = list(
      hoy = lee_hist_tc(names, date_ayer, div_dir),
      ayer = lee_hist_tc(names, date_anteayer, div_dir)
    )
  )
  
  datos <- list(divisas = read_excel(names$datos, sheet = 'Divisas'),
                curvas = read_excel(names$datos, sheet = 'Curvas'))
  
  # Obtiene mov curvas ----
  mov_curvas <- datos$curvas %>%
    left_join(
      insumos$curvas$hoy %>%
        select(index_name,
               gpt_name,
               `spot-rate`) %>%
        dplyr::rename(
          Curva = index_name,
          Plazo = gpt_name,
          `Valor t` = `spot-rate`
        )
    ) %>%
    left_join(
      insumos$curvas$ayer %>%
        select(index_name,
               gpt_name,
               `spot-rate`) %>%
        dplyr::rename(
          Curva = index_name,
          Plazo = gpt_name,
          `Valor t-1` = `spot-rate`
        )
    ) %>%
    mutate(
      `Cambio pb` = (`Valor t`-`Valor t-1`)*100
    ) %>%
    mutate(
      Plazo = case_when(Plazo == '1MO' ~ '1M',
                        Plazo == '3MO' ~ '3M',
                        Plazo == '6MO' ~ '6M',
                        Plazo == '12MO' ~ '12M',
                        TRUE ~ Plazo)
    ) %>%
    filter(Plazo != 'O/N' & Plazo != '1M') %>%
    select(-Curva)
  
  return(mov_curvas)
}

obtiene_mov_divisas <- function(hol, date_ayer){
  date_anteayer <- dates(hol, date_ayer,1,"resta",'MX')
  div_dir <- c('EUR', 'GBP', 'AUD', 'NZD')
  
  # Rutas ----
  rutas <- list(
    j = 'J:/outdir/reports/',
    px = 'Z:/II Responsabilidades/2 Inversiones/datos/precios/',
    global = 'Z:/II Responsabilidades/26 BMK Global/',
    futuros = 'J:/sysdir/import/precios/px_findur/',
    proyecto = 'Z:/II Responsabilidades/26 BMK Global/Inversiones/Proyecto RF/'
  )
  
  # Nombres ----
  names <- list(
    curvas = list(
      hoy = name_rpt(date_ayer),
      ayer = name_rpt(date_anteayer)
    ),
    hist_tc = name_hist_tc(rutas),
    datos = name_datos(rutas)
  )
  
  # Insumos ----
  insumos <- list(
    curvas = list(
      hoy = read_csv(names$curvas$hoy),
      ayer = read_csv(names$curvas$ayer)
    ),
    divisas = list(
      hoy = lee_hist_tc(names, date_ayer, div_dir),
      ayer = lee_hist_tc(names, date_anteayer, div_dir)
    )
  )
  
  datos <- list(divisas = read_excel(names$datos, sheet = 'Divisas'),
                curvas = read_excel(names$datos, sheet = 'Curvas'))
  
  # Obtiene mov divisas ----
  mov_divisas <- datos$divisas %>%
    select(`Divisa Final`) %>%
    distinct(`Divisa Final`) %>%
    left_join(insumos$divisas$hoy %>%
                dplyr::rename(`Valor t` = TC)) %>%
    left_join(insumos$divisas$ayer %>%
                dplyr::rename(`Valor t-1` = TC)) %>%
    mutate(`Cambio Porcentual` = (`Valor t-1` / `Valor t` - 1)*100) %>%
    dplyr::rename(Divisa = `Divisa Final`)
  
  return(mov_divisas)
}


