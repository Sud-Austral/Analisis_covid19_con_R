#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(rsconnect)
library(shiny)
library(shinythemes)
library(tidyverse)
library(magrittr)
library(lubridate)
library(plotly)
library(xts)
library(dygraphs)
library(rnaturalearth)
library(ggplot2)
library(reticulate)
library(knitr)
library(tidyverse)
library(gghighlight)
library(magrittr)
library(readxl)
library(readr)
library(ape)
library(ggdendro)
library(bookdown)
require(rgdal)
require(ggplot2)
library(sf)
library(ggrepel)
library(janitor)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
  shinythemes::shinytheme("cerulean"),
  titlePanel("Analisis de Covid19"),
  
  sidebarLayout(
    sidebarPanel(
      dateInput("date1", "Fecha de inicio", value="2020-01-24"),
      dateInput("date2", "Fecha de fin", value=today()),
      uiOutput("Comuna"),
      checkboxInput("logscale", "Log Y: ", value= FALSE)
    ),

    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Tabla", tableOutput("contents")),
                  tabPanel("Plot 1", plotOutput("plot1")),
                  tabPanel("Plot 2", dygraphOutput("plot2")),
                  tabPanel("R_0", plotOutput("plot3")),
                  tabPanel("Casos acumulados", plotlyOutput("plot4"))
                  )
    )
  )
   
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  mydata <- reactive({
    
# Cargamos la base de datos de DataIntelligence
      covid19 <- read_xlsx("data_20_mayo.xlsx")
      # covid19cod_comun <-
      #   read_xlsx("data_20_mayo_sec.xlsx")
    # Arreglamos el formato de la fecha:
    covid19$Fecha <- excel_numeric_to_date(covid19$Fecha)
    return(covid19 %>% filter(between(Fecha, input$date1, input$date2 )))
  })
  
  
  output$Comuna <- renderUI({
    Comunaes <- mydata() %>% select(Comuna) %>% arrange(Comuna) %>% unique()
    selectInput("Comuna", "Seleccione la Comuna:", choices = Comunaes)
  })
  
  
  
  
  output$contents <- renderTable({
    mydata() %>% filter(Comuna == input$Comuna)
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$plot3 <- renderPlotly({
    
    library(janitor)
    library(dplyr)
    library(readxl)
    # la siguiente linea impide el despliegue de advertencias en forma local:
    options(warn = -1)
    
    library(tidyverse)
    library(gghighlight)
    library(magrittr)
    library(readxl)
    library(readr)
    library(ape)
    library(ggdendro)
    library(rmarkdown)
    
    library(ggplot2)
    library(gganimate)
    theme_set(theme_bw())
    
    
    

    
    # se declara el vector de R0
    a <- c()
    
    # Se selecciona un subset con el primer codigo de la comuna:
    # comuna_en_cuestion = covid19[covid19$Codigo_comuna == covid19cod_comun$Codigo_comuna[1], ]
    
    # se selecciona la comuna de alto hospicio
    comuna_en_cuestion = mydata() %>% filter(Comuna == input$Comuna)
    
    # se debe obtener el lugar de la primera y la ultima observacion
    # nrow entrega el numero de filas de un array
    
    
    longitud = nrow(comuna_en_cuestion)
    
    
    # comuna_en_cuestion = data.frame(comuna_en_cuestion2)
    comuna_en_cuestion = data.frame(comuna_en_cuestion)
    comuna_en_cuestion
    # hay que ir construyendo subset de datos para iquique que vayan del 1 al 1 al 1 al 59 (59 subsets)
    # for (j in 2:longitud)
    # {
    
    
    # esta matriz no comienza del registro numero 1
    # ya tenemos el array listo
    
    # comuna_en_cuestion2 = comuna_en_cuestion[1:j,]
    
    
    
    #pop_santiago <- covid19cod_comun$poblacion[1]
    
    # carguemos sun numero artificial a population
    population <- 1000
    
    
    
    # Calculamos los infechatados, los recuperados y los susceptibles por dias:
    inFechatados.por.dia = aggregate(comuna_en_cuestion$Casos_Diarios ~ comuna_en_cuestion$Fecha, FUN =
                                       sum)
    
    recuperados.por.dia = aggregate(comuna_en_cuestion$Recuperados_Diarios ~
                                      comuna_en_cuestion$Fecha, FUN = sum)
    
    comuna_en_cuestion_mod = data.frame(comuna_en_cuestion$Fecha,
                                        inFechatados.por.dia,
                                        recuperados.por.dia)
    comuna_en_cuestion_mod <-
      comuna_en_cuestion_mod %>% mutate(suma_rec_inf = cumsum(
        comuna_en_cuestion.Casos_Diarios + comuna_en_cuestion.Recuperados_Diarios
      ))
    
    comuna_en_cuestion_mod <-
      comuna_en_cuestion_mod %>% mutate(
        susceptibles = population - cumsum(
          comuna_en_cuestion.Casos_Diarios + comuna_en_cuestion.Recuperados_Diarios
        )
      )
    comuna_en_cuestion_mod
    
    
    
    
    
    
    
    
    x = comuna_en_cuestion_mod$comuna_en_cuestion.Recuperados_Diarios
    x
    x[26]
    y = population * log(comuna_en_cuestion_mod$susceptibles)
    y
    
    
    # 
    # 
    
    m <- seq(2, longitud)  
    
    for(i in m) {
      #Tenemos los vectores sobre los que ejecutar los calculos:
      xx<- x[1:i]
      yy<- y[1:i]
      estimacion.R0 = -summary(lm(yy ~ xx))$coefficients[2]
      
      a[i] <- estimacion.R0
      
      # cod <- comuna_en_cuestion$Codigo_comuna
      
      com <- comuna_en_cuestion$Comuna %>% unique()
      
      # print(estimacion.R0)
      # print(covid19$Comuna[i])
      
      cat("longitud",longitud,"El R", com, "es", estimacion.R0, "\n")
      # }   
      # cat(com,estimacion.R0,"\n")
    }
    
    for(i in 5) {
      #Tenemos los vectores sobre los que ejecutar los calculos:
      x<- x[1:i]
      y<- y[1:i]
      estimacion.R0 = -summary(lm(y ~ x))$coefficients[2]
      
      #a[j] <- estimacion.R0
      
      # cod <- comuna_en_cuestion$Codigo_comuna
      
      com <- comuna_en_cuestion$Comuna %>% unique()
      
      # print(estimacion.R0)
      # print(covid19$Comuna[i])
      
      cat("longitud",longitud,"El R", com, "es", estimacion.R0, "\n")
      # }   
      # cat(com,estimacion.R0,"\n")
    }

    
    x <-  1:longitud
    
    eee <- data.frame(x,a)
    
    p <- ggplot(
      eee,
      aes(x, a)
    ) +
      geom_line(colour="#000099") +
      labs(x = "Dia de la infección", y = "R_0") +
      theme(legend.position = "top")

    p + geom_point(colour ="#CC6666") + transition_reveal(x)

    
  })
  
  
  
  
  
  
  
  
  output$plot4 <- renderPlotly({
    

    covid19_ayp = mydata() %>% filter(Comuna == input$Comuna)
    comun <- input$Comuna
    lafecha <- covid19_ayp$Fecha
    
    muertes_acum <- covid19_ayp$Muertes_acum
    casos_activos_acum <- covid19_ayp$Casos_Activos_acum
    recuperados_acum <- covid19_ayp$Recuperados_acum
    
    casos_acumulados <- covid19_ayp$Casos_Acum
    
    # se construye un dataframe
    data <- data.frame(lafecha, muertes_acum, casos_activos_acum, recuperados_acum, casos_acumulados)
    
    # se define el grafico padre:
    fig <- plot_ly(data, x = ~lafecha, y = ~muertes_acum, type = 'bar', name = 'Muertes acumuladas',
                   marker = list(color = 'rgb(255, 255, 255)')) 
    
    # # Barras de casos_activos_acum
    fig <- fig %>% add_trace(y = ~casos_activos_acum, name = 'Casos activos acumulados', marker = list(color = 'rgb(0, 0, 255)'))
    # Barras de recuperados_acum
    fig <- fig %>% add_trace(y = ~recuperados_acum, name = 'Casos recuperados acumulados', marker = list(color = 'rgb(0, 128, 0)'))
    
    # se anade una linea:
    fig <- fig %>% add_trace(x = ~lafecha, y = ~casos_acumulados, name = 'Casos acumulados', type = 'scatter', mode = 'lines', line = list(color = 'rgb(255, 0, 0)', width = 2))
    
    fig <- fig %>% layout(title = 'Analisis del Covid19 en las comunas de Chile',
                          xaxis = list(
                            title = "",
                            tickfont = list(
                              size = 14,
                              color = 'rgb(107, 107, 107)')),
                          yaxis = list(
                            title = 'Casos',
                            titlefont = list(
                              size = 16,
                              color = 'rgb(107, 107, 107)'),
                            tickfont = list(
                              size = 14,
                              color = 'rgb(107, 107, 107)')),
                          legend = list(x = 0, y = 1, bgcolor = 'rgba(255, 255, 255, 0)', bordercolor = 'rgba(255, 255, 255, 0)'),
                          barmode = 'group', bargap = 0.15, bargroupgap = 0.1)
    fig
    
  })
  
  
}


shinyApp(ui = ui, server = server)
