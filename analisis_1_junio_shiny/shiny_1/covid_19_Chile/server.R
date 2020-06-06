## Interactive PLS Path Modeling Online application server.R

# to load needed libraries:
library("semPLS")
library("plspm")
library("XML")
library("shiny")

# Define server logic to Show PLSPM results
shinyServer(function(input, output) {
 
  # reactively read in PLS path modeling data as CSV file
  readd <- reactive({
    
    # input$file1 will be NULL initially. 
    # After the user selects and uploads a 
    # file, it will be a data frame with 
    # 'name', 'size', 'type', and 'datapath' 
    # columns. The 'datapath' column will 
    # contain the local filenames where the 
    # data can be found.
    
    inFile1 <- input$file1
    if (is.null(inFile1))
      return(NULL)
    data <- read.csv(inFile1$datapath, 
                     header=input$header,
                     sep=input$sep)
    return(data)
  })
  
  # reactiviely read in SmartPLS model (.splsm) file
  readm <- reactive({
    inFile2 <- input$file2
    if (is.null(inFile2))
      return(NULL)
    model <- read.splsm(inFile2$datapath)
    return(model)
  })
  
  #----------------------------------------------
  # FUNCTIONS TO CREATE plspm() FUNCTION ARGUMENTS:
  # from the SmartPLS model file import
  
  # Non-reactive function to create inner_matrix
  # second argument for plspm() function 
  make.inner <- function(modl=model){
    inner <- t(modl$D)
  }
 
  # Non-reactive function to create outer_list
  # third argument for plspm() function
  make.outer <- function(modl=model,d=data){
    # initialize outer as a list
    outer <- list()
    # counts number of blocks 
    # (latent variables) in the model
    nblocks <- length(modl$blocks)
    # initializes z
    z <- numeric(nblocks)
    # initial df object to hold output
    # vectors for LV mv positions in data
    comp <- data.frame()
    # loop thru nblocks counting number mv's
    for(i in 1:nblocks){
      # store number mvs per block in z[i]
      z[i] <- length(modl$blocks[[i]])
      # find positions in data for mvs
      for(j in 1:z[i]){
        data.index <- grep((modl$blocks[[i]][j]),names(d))
        # store as rows in comp
        comp[i,j] <- data.index
      }
    }
    # transpose rows and columns in comp
    df <- t(comp)
    for(i in 1:nblocks){
      vec <- unname(df[,i])
      vec <- c(na.omit(vec))
      outer[[i]] <- vec
    }
    return(outer)
  }
  
  # Non-reactive function to strip out modes from
  # imported SmartPLS model file for each LV
  # and return them in an LV-named vector.
  get.modes <- function(modl=model){
    # Retrieve names of latent variables
    lv.names <- names(modl$blocks)
    # Get count of number of latent variables
    num.lvs <- length(modl$blocks)
    # Initialize a variable to store
    # names of LVs and their modes
    lv.modes <- numeric(num.lvs)
    # loop through each LV and retrieve model
    for(i in 1:num.lvs){
      # retrieve mode stored as only attribute
      # of each block (same as LV)
      lv.modes[i] <- attributes(modl$blocks[[i]])
      # replace LV component index in lv.modes
      # with the name of the LV for that mode
      names(lv.modes) <- lv.names
    }
    # convert lv.modes list into a named vector
    # of LVs and their modes and return it
    return(unlist(lv.modes))
  }
 
  # fit model reactively using semPLS package
  semPLS <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    inFile2 <- input$file2
    if (is.null(inFile2))
      return(NULL)
    if (input$scheme=="path"){
      wscheme='pw'
    }
    if (input$scheme=="factor"){
      wscheme="factorial"
    }
    if (input$scheme=="centroid"){
      wscheme="centroid"
    }
    semPLS.fit <- sempls(readm(),readd(),wscheme=wscheme)
    return(semPLS.fit)
  })
    
  # fit model reactively using plspm package
  pls <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    inFile2 <- input$file2
    if (is.null(inFile2))
      return(NULL)
    plspm.fit <- plspm(readd(),
                       make.inner(modl=readm()),
                       make.outer(modl=readm(),d=readd()),
                       get.modes(modl=readm()),
                       scheme=input$scheme,
                       scaled=input$scl,
                       boot.val=input$bval,
                       br=input$boots)
    return(plspm.fit)
  })
  
  # FOLLOWING ARE ALL REACTIVE RENDER FUNCTIONS
  # TO CREATE THE TAB-BASED UI OUTPUT IN MAINPANEL
  
  # display read-in data as "Raw Data" tab
  output$contents <- renderTable(readd())
  
  # plot path coefficients in inner model
  output$plotinner <- renderPlot({
    plot(pls(), box.prop = 0.55, box.size = 0.10, box.cex = 1.5,
         box.col = "cadetblue1", lcol = "black",
         txt.col = "black", arr.pos = 0.5, cex.txt = 1.5)
  })
  
  # plot loadings in outer model
  output$plotload <- renderPlot({
    plot(pls(), what="loadings", box.prop = 0.55, box.size = 0.10, box.cex = 1.5,
         box.col = "cadetblue1", lcol = "black",
         txt.col = "black", arr.pos = 0.5, cex.txt = 1.5)
  })
  
  # plot weights in outer model
  output$plotweight <- renderPlot({
    plot(pls(), what="weights", box.prop = 0.55, box.size = 0.10, box.cex = 1.5,
         box.col = "cadetblue1", lcol = "black",
         txt.col = "black", arr.pos = 0.5, cex.txt = 1.5)
  })
 
  # computes inner dimensionality summary as output$inner
  output$inners <- renderTable({
    # returns the inner summary
    pls()$inner.sum
  })
  
  # computes inner summary as output$uni
  output$uni <- renderTable({
    # returns the inner summary
    pls()$unidim
  })
  
  # computes weights and loadings summary as output$outerm
  output$outerm <- renderTable({
    do.call(rbind,pls()$outer.mod) 
  })
  
  # computes loadings and cross loadings summary as output$outerc
  output$outerc <- renderTable({
    do.call(rbind,pls()$outer.cor)
  })
   
  # direct and indirect effects
  output$eff <- renderTable({
    pls()$effects
  })

  # computes and returns bootstrap path coef CIs
  output$paths <- renderTable({
    pls()$boot[[3]]
  })
  
  # computes and returns bootstrap effects CIs
  output$effects <- renderTable({
    pls()$boot[[5]]
  })
  
  # computes and returns bootstrap R-Square CIs
  output$rsq <- renderTable({
    pls()$boot[[4]]
  })
  
  # computes and returns bootstrap outer loading CIs
  output$loads <- renderTable({
    pls()$boot[[2]]
  })
  
  # computes and returns bootstrapped weight CIs
  output$weights <- renderTable({
    pls()$boot[[1]]
  })
  
  # plots evolution of outer weights
  output$weightsevol <- renderPlot({
    plot(semPLS())
  })

  # density plot of factor scores
  output$densplotf <- renderPlot({
    plot(densityplot(semPLS(),use="fscores"))
  })
  
  # density plot of predictions
  output$densplotp <- renderPlot({
    plot(densityplot(semPLS(),use="prediction"))
  })
  
  # density plot of residuals
  output$densplotr <- renderPlot({

                  img(src="data_met.gif")
  })
  
  dat <- reactive({
    if (input$downdata=="Total Effects") {
      out <- pls()$effects
    } else if (input$downdata=="LV Overview") {
      out <- pls()$inner.sum
    } else if (input$downdata=="LV Reliability") {
      out <- pls()$unidim
    } else if (input$downdata=="Cross Loadings") {
      out <- do.call(rbind,pls()$outer.cor)
    } else if (input$downdata=="Bootstrap: Path CIs") {
      out <- pls()$boot[[3]]
    } else if (input$downdata=="Bootstrap: Loading CIs") {
      out <- pls()$boot[[2]]
    } else if (input$downdata=="Bootstrap: Weight CIs") {
      out <- pls()$boot[[1]]
    } else if (input$downdata=="Bootstrap: Effect CIs") {
      out <- pls()$boot[[5]]
    } else if (input$downdata=="Bootstrap: R-Squared CIs") {
      out <- pls()$boot[[4]]
    } else out <- do.call(rbind,pls()$outer.mod)
    return(out)
  })

  plt <- reactive({
    if (input$downplot=="Path Model Plot") {
      graph <- plot(pls(), box.prop = 0.55, box.size = 0.10, box.cex = 1.5,
                    box.col = "cadetblue1", lcol = "black",
                    txt.col = "black", arr.pos = 0.5, cex.txt = 1.5)
    } else if (input$downplot=="Loadings Plot") {
      graph <- plot(pls(), what="loadings", box.prop = 0.55, box.size = 0.10, box.cex = 1.5,
                    box.col = "cadetblue1", lcol = "black",
                    txt.col = "black", arr.pos = 0.5, cex.txt = 1.5)
    } else if (input$downplot=="Weights Plot") {
      graph <- plot(pls(), what="weights", box.prop = 0.55, box.size = 0.10, box.cex = 1.5,
                    box.col = "cadetblue1", lcol = "black",
                    txt.col = "black", arr.pos = 0.5, cex.txt = 1.5)
    } else if (input$downplot=="LV Scores Plot") {
      graph <- plot(densityplot(semPLS(),use="fscores"))
    } else if (input$downplot=="LV Predictions Plot") {
      graph <- plot(densityplot(semPLS(),use="prediction"))
    } else graph <- plot(densityplot(semPLS(),use="residuals"))
    return(graph)
  })
  
  # Download Handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$downdata, '-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(x=dat(), file)
    })
  
  output$downloadPlot <- downloadHandler( 
    filename = function() { paste(input$downplot, '-', Sys.Date(), '.png', sep='') }, 
    content = function(file) { 
      png(file, width=800, height=800) 
      print(plt())
      dev.off() 
      })
  })
  

