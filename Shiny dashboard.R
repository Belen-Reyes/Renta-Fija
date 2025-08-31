library(shinydashboard)
library(shiny)
library(ggplot2)
library(formattable)
library(scales)

ui <- dashboardPage(
  dashboardHeader(title = "Resultados"),
  dashboardSidebar(
    dateInput(
      "fecha",
      h4("Fecha del reporte"),
      value = date_ayer,
      min = "2008-01-01",
      max = date,
      format = "dd-mm-yyyy",
      startview = "month"
    ),
    selectInput(
      "portafolio",
      h4("Portafolio"),
      selected = 'GOI',
      choices = c('GOI', 'BMK')
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
                .center-table {
                    display: flex;
                    justify-content: center;
                    align-items: center;
                    height: 100%;
                    width: 100%;
                }
                .box-body {
                    display: flex;
                    justify-content: center;
                    align-items: center;
                }
            "))
    ),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/dom-to-image/2.6.0/dom-to-image.min.js"),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jspdf/2.4.0/jspdf.umd.min.js"),
    tags$script(HTML(
      '
            window.jsPDF = window.jspdf.jsPDF;
            function capture(){
                domtoimage.toPng(document.querySelector("body"))
                .then(function(dataurl) {
                    var orientation = window.innerHeight >= window.innerWidth ? "portrait" : "landscape";
                    const doc = new jspdf.jsPDF({unit: "px", orientation: orientation, format: [8000, 5200]});
                    doc.addImage(dataurl, "JPG", 0, 0, 8000, 5200);
                    doc.save("Resultados.pdf");
                });
            }
            '
    )),
    # infoBoxes with fill=FALSE
    fluidRow(
      valueBoxOutput("resultado_total"),
      
      valueBoxOutput("resultado_tasa"),
      
      valueBoxOutput("resultado_cambiario")
    ),
    fluidRow(
      box(
        title = "Curvas soberanas",
        status = "success",
        width = 6,
        height = 400,
        plotOutput("cambio_curvas", height = 330)
      ),
      box(
        title = "Divisas",
        status = "warning",
        width = 3,
        height = 400,
        plotOutput("cambio_divisas", height = 330)
      ),
      box(
        title = "Divisas",
        status = "warning",
        width = 3,
        height = 400,
        div(class = "center-table", 
            tableOutput("cambio_divisas_num"))
      )
    ),
    fluidRow(
      box(
        title = "Top 5 Resultados de Tasa",
        status = "primary",
        plotOutput("topTasaPlot", height = 230),
        height = 300  # Establece la altura del cuadro
      ),
      box(
        title = "Top 5 Resultados Cambiarios",
        status = "primary",
        plotOutput("topCambiarioPlot", height = 230),
        height = 300  # Establece la altura del cuadro
      )
    ),
    actionButton("generate", "Imprimir PDF", onclick="capture()"),
)
)

server <- function(input, output) {
  
  date_ayer <- reactive({
    input$fecha
  })
  
  port <- reactive({
    input$portafolio
  })
  
  nombre_res <- reactive({
    name_resultados(date_ayer(), port())
  })
  
  tabla_resultados <- reactive({
    obtiene_tabla_resultados(nombre_res())
  })
  
  totales <- reactive({
    obtiene_totales(tabla_resultados())
  })
  
  mov_curvas <- reactive({
    obtiene_mov_curvas(hol, date_ayer())
  })
  
  mov_divisas <- reactive({
    obtiene_mov_divisas(hol, date_ayer()) %>%
      mutate(Divisa = factor(Divisa, levels = rev(c("AUD", "CAD", "CNYON", "EUR", "GBP", "JPY", "NZD", "SGD", "USD"))))
  }) 
  
  # PARTE 1: Resultados generales ----
  output$resultado_total <- renderValueBox({
    res_total <- formatC(totales()$`Resultado Total`, format = "f", big.mark = ",", digits = 2)
    
    valueBox(res_total, "Resultado Total", icon = icon("list"),
             color = "blue"
    )
  })
  
  output$resultado_tasa <- renderValueBox({
    res_tasa <- formatC(totales()$`Resultado Tasa`, format = "f", big.mark = ",", digits = 2)
    
    valueBox(res_tasa, "Resultado Tasa", icon = icon("list"),
             color = "green"
    )
  })
  
  output$resultado_cambiario <- renderValueBox({
    res_cambiario <- formatC(totales()$`Resultado Cambiario`, format = "f", big.mark = ",", digits = 2)
    
    valueBox(res_cambiario, "Resultado Cambiario", icon = icon("list"),
             color = "yellow"
    )
  })
  
  output$cambio_curvas <- renderPlot({
    df <- mov_curvas()
    df <- df %>%
      mutate(Plazo = factor(Plazo, levels = c(
        "O/N", "1M", "3M", "6M","12M", "2Y", "5Y", "10Y", "20Y", "30Y"
      )))

    ggplot(df, aes(x = Plazo, y = `Cambio pb`, fill = `Cambio pb` > 0)) +
      geom_bar(stat = "identity") +
      facet_wrap(~ Pais, scales = "free_x") +
      scale_fill_manual(values = c("TRUE" = "#00a65a", "FALSE" = "red")) +
      labs(x = "Plazo", y = "Cambio pb") +
      theme_bw() +
      theme(
        legend.position = "none",
        strip.background = element_blank(),
        strip.placement = "outside",
        axis.text.x = element_text(angle = 0, hjust = 0)
      )
  })
  
  output$cambio_divisas <- renderPlot({
    df_2 <- mov_divisas()

    ggplot(df_2, aes(x = Divisa, y = `Cambio Porcentual`, fill = `Cambio Porcentual` > 0)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("TRUE" = "#00a65a", "FALSE" = "red")) +
      labs(x = "Divisa", y = "Cambio Porcentual") +
      theme_bw() +
      coord_flip() +
      scale_y_continuous(labels = number_format(accuracy = 0.01)) +
      theme(
        legend.position = "none"
      )
  })
  
  output$cambio_divisas_num <- renderTable({
    mov_divisas() %>%
      select(Divisa, `Cambio Porcentual`)
  })
  
  output$topTasaPlot <- renderPlot({
    data <- tabla_resultados()
    
    # Filtrar los 5 resultados de tasa con mayor influencia (valor absoluto más grande)
    top_tasa <- data %>%
      mutate(abs_tasa = abs(`Resultado Tasa`)) %>%
      arrange(desc(abs_tasa)) %>%
      slice(1:5)
    
    # Crear la gráfica
    ggplot(top_tasa, aes(x = reorder(`Tipo de instrumento`, `Resultado Tasa`), y = `Resultado Tasa`, fill = `Resultado Tasa` > 0)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(`Resultado Tasa`, 2)), hjust = 1) +
      scale_fill_manual(values = c("TRUE" = "#00a65a", "FALSE" = "red")) +
      labs(x = "Tipo de Instrumento", y = "Resultado Tasa") +
      theme_bw() +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 1)
      ) +
      coord_flip()
  })
  
  output$topCambiarioPlot <- renderPlot({
    data <- tabla_resultados()
    
    # Filtrar los 5 resultados cambiarios con mayor influencia (valor absoluto más grande)
    top_cambiario <- data %>%
      mutate(abs_cambiario = abs(`Resultado Cambiario`)) %>%
      arrange(desc(abs_cambiario)) %>%
      slice(1:5)
    
    # Crear la gráfica
    ggplot(top_cambiario, aes(x = reorder(`Tipo de instrumento`, `Resultado Cambiario`), y = `Resultado Cambiario`, fill = `Resultado Cambiario` > 0)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(`Resultado Cambiario`, 2)), hjust = 1) +
      scale_fill_manual(values = c("TRUE" = "#00a65a", "FALSE" = "red")) +
      labs(x = "Tipo de Instrumento", y = "Resultado Cambiario") +
      theme_bw() +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 1)
      ) +
      coord_flip()
  })
  observeEvent(input$print, {
    session$sendCustomMessage(type = 'print', message = list())
  })
  
  
}
