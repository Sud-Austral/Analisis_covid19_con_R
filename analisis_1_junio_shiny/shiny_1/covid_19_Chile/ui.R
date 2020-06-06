## User Interface or ui.R source file for:
## Interactive PLS Path Modeling Online Application

library(shiny)
# use fluidPage() and sidebarLayout() instead.

ui <- fluidPage(
        theme = shinytheme("superhero"),
        #  titlePanel("Analisis de Covid19"),
        
        
        titlePanel(
                fluidRow(
                        column(12, strong("Covid 19 en Chile")),
                        
                        column(12, downloadButton("cons", "Descargue la tabla"))
                )
        ),
        
        
        

  
  
  
  
  
  
  sidebarLayout(
  sidebarPanel(
    fileInput('file1', 'Ingrese el archivo CSV:',
              accept=c('text/csv','text/comma-separated-values,text/plain')),
    #tags$hr(),
    checkboxInput('header', 'Data file has variable names as column headers.', TRUE),
    radioButtons('sep', 'Data File separator value:',
                 c(Comma=',',
                   Semicolon=';',
                   Tab='\t'),
                 'Comma'),
    fileInput('file2', 'Upload SmartPLS .splsm model file:'),
    checkboxInput("bval", "Run bootstrap validation", FALSE),
    conditionalPanel(
      condition = "input.bval== true",
      sliderInput("boots","Number of Bootstrap Resamples:",
                  min = 100,max = 500,value=200,step=100)
    ),
    selectInput("scheme", "Choose inner weighting scheme:",
                choices = c("path", "factor", "centroid")),
    checkboxInput('scl', 'Scale input data. ', TRUE),
    selectInput("downdata", "Choose Results to Download: ",
                choices = c("Total Effects", "LV Overview", 
                            "LV Reliability", "Cross Loadings",
                            "Weights and Loadings", "Bootstrap: Path CIs",
                            "Bootstrap: Loading CIs","Bootstrap: Weight CIs",
                            "Bootstrap: Effect CIs","Bootstrap: R-Squared CIs")),
    selectInput("downplot", "Choose Plot to Download:",
                choices = c("Path Model Plot", "Loadings Plot", 
                            "Weights Plot", "LV Scores Plot", 
                            "LV Predictions Plot", "LV Residuals Plot")),
    downloadButton("downloadData", "Download Selected Results"),
    downloadButton("downloadPlot", "Download Selected Plot")

 ),
  
  
  
  
  
  
  
  
  
  
  # esto queda igual:
  
  mainPanel(
    
    tabsetPanel(type = "tabs",
      tabPanel("Datos", 
               h4("Datos originales cargados"),
               tableOutput("contents")),
      tabPanel("La matemática del R0", 
               h4("Path Model Coefficients"),
               plotOutput("plotinner",height="650px")),
      tabPanel("Bootstrap: Path CIs", 
               h4("95% Confidence Intervals for Inner Path Model Coefficients"),
               tableOutput("paths")),
      tabPanel("Loadings Plot", 
               h4("Measurement (Outer) Model Path Loadings"),
               plotOutput("plotload",height="650px")),
      tabPanel("Bootstrap: Loading CIs", 
               h4("95% Confidence Intervals for Outer Path Loadings"),
               tableOutput("loads")),
      tabPanel("Weights Plot", 
               h4("Measurement (Outer) Model Path Weights"),
               plotOutput("plotweight",height="650px")),
      tabPanel("Weights Evolution Plot", 
               h4("PLS Algorithm Iteration-Driven Adjustments to Weights"),
               plotOutput("weightsevol",height="650px")),
      tabPanel("Bootstrap: Weight CIs", 
               h4("95% Confidence Intervals for Outer Path Weights"),
               tableOutput("weights")),
      tabPanel("Total Effects", 
               h4("Direct, Indirect and Total Effects"),
               tableOutput("eff")),
      tabPanel("Bootstrap: Effect CIs", 
               h4("95% Confidence Intervals for Total Effects"),
               tableOutput("effects")),
      tabPanel("LV Overview", 
               h4("Latent Variable Summary or Overview Table"),
               tableOutput("inners")),
      tabPanel("LV Reliability",
               h4("Latent Variable Reliability or Unidimensionality Table"),
               tableOutput("uni")),
      tabPanel("Cross Loadings",
               h4("Loadings and Cross Loadings Table"),
               tableOutput("outerc")),
      tabPanel("Weights and Loadings", 
               h4("Measurement Model Weights and Loadings"),
               tableOutput("outerm")),
      tabPanel("Bootstrap: R-Squared CIs", 
               h4("95% Confidence Intervals for R-Squared Explained Variance Values"),
               tableOutput("rsq")),
      tabPanel("LV Scores Plot", 
               h4("Density and Rug Plots of All Standardized Latent Variable Score Distributions"),
               plotOutput("densplotf",height="650px")),
      tabPanel("LV Predictions Plot", 
               h4("Density and Rug Plots of Endogenous Latent Variable Predicted Score Distributions"),
               plotOutput("densplotp",height="650px")),
      tabPanel("Mapas", 
               h4("Mapa de Santiago"),
               plotOutput("densplotr",height="650px"))
    )
  )
)
)

