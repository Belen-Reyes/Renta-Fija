# Paqueterias
library(shiny)
library(shinyalert)
library(shinythemes)
library(dipsaus)
library(here)

ui_rf <- navbarPage(
  theme = shinytheme('flatly'),
  title = "Porcion de Renta Fija",
  tabPanel("Proceso operativo",
           # shinythemes::themeSelector(), 
           theme = shinytheme('flatly'),
           sidebarLayout(
             sidebarPanel(
               dateInput(
                 "fecha",
                 h4("Fecha del reporte"),
                 value = date_ayer,
                 min = "2008-01-01",
                 max = date,
                 format = "dd-mm-yyyy",
                 startview = "month"
               )
             ),
             
             mainPanel(
               fluidRow(
                 h4('Paso 1'),
                 h5('Validar los insumos'),
                 actionButtonStyled(
                   'Boton_valida_cup_ven',
                   'Valida cupones y vencimientos',
                   btn_type = "button",
                   type = 'primary'
                 ),
                 
                 actionButtonStyled(
                   'Boton_valida_ic',
                   'Valida IC',
                   btn_type = "button",
                   type = 'primary'
                 ),
                 
                 actionButtonStyled(
                   'Boton_valida_px_ctd',
                   'Valida PX y CTD',
                   btn_type = "button",
                   type = 'primary'
                 ),
                 
                 h5(" ")
               ),
               
               fluidRow(
                 h4('Paso 2'),
                 h5('Actualizar insumos'),
                 actionButtonStyled(
                   'Boton_actualiza_frn',
                   'Actualiza tasas FRN',
                   btn_type = "button",
                   type = "default"
                 ),
                 
                 actionButtonStyled(
                   'Boton_actualiza_cpi',
                   'Actualiza CPI',
                   btn_type = "button",
                   type = "default"
                 ),
                 
                 h5(" ")
               ),
               
               fluidRow(
                 h4('Paso 3'),
                 h5('Valuaciones IC y BMK'),
                 actionButtonStyled(
                   'Boton_valua_ic',
                   'Valua IC',
                   btn_type = "button",
                   type = "warning"
                 ),
                 
                 actionButtonStyled(
                   'Boton_valua_bmk',
                   'Valua BMK',
                   btn_type = "button",
                   type = "warning"
                 ),
                 # 
                 # actionButtonStyled(
                 #   'Boton_valua_bmk_cp6',
                 #   'Valua BMK CP6',
                 #   btn_type = "button",
                 #   type = "warning"
                 # ),
                 
                 h5(" ")
               ),
               
               fluidRow(
                 h4('Paso 4'),
                 em(
                   'En caso de que existan diferencias en el compara correr el boton "Corrige Valuacion"'
                 ),
                 h5(' '),
                 actionButtonStyled(
                   'Boton_compara_ic',
                   'Compara IC',
                   btn_type = "button",
                   type = "danger"
                 ),
                 
                 actionButtonStyled(
                   'Boton_corrige_valuacion_ic',
                   'Corrige valuacion IC',
                   class = 'btn btn-danger disabled',
                   btn_type = "button"
                 ),
                 h5(" ")
               ),
               
               fluidRow(
                 h4('Paso 5'),
                 h5('Resultados IC y BMK'),
                 actionButtonStyled(
                   'Boton_resultados_ic',
                   'Resultados IC',
                   btn_type = "button",
                   type = "success"
                 ),
                 
                 actionButtonStyled(
                   'Boton_resultados_bmk',
                   'Resultados BMK',
                   btn_type = "button",
                   type = "success"
                 ),
                 # 
                 # actionButtonStyled(
                 #   'Boton_resultados_bmk_cp6',
                 #   'Resultados BMK CP6',
                 #   btn_type = "button",
                 #   type = "success"
                 # ),
                 
                 h5(" ")
               ),
               fluidRow(
                 h3(' '),
                 em(
                   'Botones auxiliares para generar los archivos de Cupones y Vencimientos, IC CTD en caso de que un día anterior no se hayan generado'
                 ),
                 h5(' '),
                 actionButtonStyled(
                   'Boton_genera_cup_ven',
                   'Genera cupones y vencimientos',
                   btn_type = "button",
                   type = 'default',
                   class="btn-sm"
                 ),
                 
                 actionButtonStyled(
                   'Boton_genera_ic_ctd',
                   'Genera IC CTD',
                   btn_type = "button",
                   type = 'default',
                   class="btn-sm"
                 ),
                 
                 h5(" ")
               ),
             )
           ))
 )


server_rf <- function(input, output, session) {
  
  date_ayer <- reactive({
    input$fecha
  })
  
  # PARTE 1: Validaciones ----
  observeEvent(input$Boton_valida_cup_ven,
               {
                 valida_cup_ven(hol, date_ayer())
               })
  observeEvent(input$Boton_valida_ic,
               {
                 valida_ic(hol, date_ayer())
               })
  observeEvent(input$Boton_valida_px_ctd,
              {
                valida_px_ic_ctd(hol, date_ayer())
              })
  
  # PARTE 1.1: Genera Cup Ven y IC_CTD ----
  observeEvent(input$Boton_genera_cup_ven,
               {
                 genera_cup_ven(hol, date_ayer())
               })
  observeEvent(input$Boton_genera_ic_ctd,
               {
                 genera_ic_ctd(hol, date_ayer())
               })
  
  # PARTE 2: Actualiza FRN Y CPI ----
  observeEvent(input$Boton_actualiza_frn,
               {
                 genera_frn(hol, date_ayer())
               })
  
  observeEvent(input$Boton_actualiza_cpi,
               {
                 genera_cpi(hol, date_ayer())
               })
  
  # PARTE 3: Valua IC y BMK ----
  observeEvent(input$Boton_valua_ic,
               {
                 obtiene_valua_ic(hol, date_ayer())
               })
  
  observeEvent(input$Boton_valua_bmk,
               {
                 obtiene_valua_bmk(hol, date_ayer())
               })
  
  # observeEvent(input$Boton_valua_bmk_cp6,
  #              {
  #                obtiene_valua_bmk_cp6(hol, date_ayer())
  #              })
  
  # PARTE 4: Compara IC ----
  observeEvent(input$Boton_compara_ic,
               {
                 obtiene_compara_ic(hol, date_ayer())
               })
  
  observeEvent(input$Boton_corrige_valuacion_ic,
               {
                 corrige_valuacion_ic(hol, date_ayer())
               })
  
  # PARTE 5: Resultados IC y BMK ----
  observeEvent(input$Boton_resultados_ic,
               {
                 obtiene_resultados_ic(hol, date_ayer())
               })
  
  observeEvent(input$Boton_resultados_bmk,
               {
                 obtiene_resultados_bmk(hol, date_ayer())
               })
  # 
  # observeEvent(input$Boton_resultados_bmk_cp6,
  #              {
  #                obtiene_resultados_bmk_cp6(hol, date_ayer())
  #              })
  
}

