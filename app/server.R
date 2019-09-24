library(shiny)
library(ggplot2)
library(DT)
library(plyr)
library(dplyr)
library(googleVis)
library(leaps)
library(reshape2)

## Initiate the severer
shinyServer(
  function(input, output, session) {
    
    
    # Variable #1
    output$varselect1 <- renderUI({
      selectInput("var1", label = "Choose your y:",
                  choices = names(dataset()), selected = names(dataset())[1])  
    })
    # Variable #2
    output$varselect2 <- renderUI({
      selectInput("var2", label = "Choose your x1:",
                  choices = names(dataset()), selected = names(dataset())[2])  
    })
    # Variable 3
    output$varselect3 <- renderUI({
      selectInput("var3", label = "Choose your x2:",
                  choices = names(dataset()), selected = names(dataset())[3])  
    })
    # Variable 4
    output$varselect4 <- renderUI({
      selectInput("var4", label = "Choose your x3:",
                  choices = names(dataset()), selected = names(dataset())[4])  
    })
    # Variable 5
    output$varselect5 <- renderUI({
      selectInput("var5", label = "Choose your x4:",
                  choices = names(dataset()), selected = names(dataset())[5])  
    })
    # Variable 6
    output$varselect6 <- renderUI({
      selectInput("var6", label = "Choose your x5:",
                  choices = names(dataset()), selected = names(dataset())[6])
    })
    
    ## Read in the data
    dataset <- reactive({
      infile <- input$datafile
      if (is.null(infile)) {
        return(NULL)
      }
      else {read.csv(infile$datapath)}
    })
                      
    # Print out the formula
    output$modelFormula <- renderUI({
      formula <- paste(input$var1, '~', input$var2, '+',  input$var3, '+',  input$var4, '+',  input$var5, '+',  input$var6)
      return(HTML("<h4>
                  <strong>Regression Equation</strong>
                  </h4>", formula)
             )
    })
    
    # Regression formula
    regFormula <- reactive({
      as.formula(paste(input$var1, '~', input$var2, '+',  input$var3, '+',  input$var4, '+',  input$var5, '+',  input$var6))
    })
    
    # Model
    model <- reactive({
      lm(regFormula(), data=dataset())
    })
    
    # Auto model
    autoModel <- reactive({
      regsubsets(
        regFormula(),
        data=dataset(),
        method='backward',
        nbest=1
      )
    })
    
    # Data view 
    output$view <- renderTable({
      head(dataset(),  n=input$obs)
    })
    
    # Summary statistics
    output$summary <- renderPrint({
      summary(cbind(dataset()[input$var1], dataset()[input$var2], dataset()[input$var3], dataset()[input$var4],
                    dataset()[input$var5], dataset()[input$var6]))
    })
    
    # Histograms
    output$distPlot_iv1 <- renderPlot({
      df <- dataset()
      x <- df[[input$var2]]
      hist(x, col = 'darkgreen', border = 'white', main = input$var2, xlab = "Observations")
    })
    
    output$distPlot_iv2 <- renderPlot({
      df <- dataset()
      x <- df[[input$var3]]
      hist(x, col = 'darkgreen', border = 'white', main = input$var3, xlab = "Observations")
    })
    
    output$distPlot_iv3 <- renderPlot({
      df <- dataset()
      x <- df[[input$var4]]
      hist(x, col = 'darkgreen', border = 'white', main =  input$var4, xlab = "Observations")
    })
    
    output$distPlot_iv4 <- renderPlot({
      df <- dataset()
      x <- df[[input$var5]]
      hist(x, col = 'darkgreen', border = 'white', main = input$var5, xlab = "Observations")
    })
    
    output$distPlot_iv5 <- renderPlot({
      df <- dataset()
      x <- df[[input$var6]]
      hist(x, col = 'darkgreen', border = 'white', main = input$var6, xlab = "Observations")
    })
    
    output$distPlot_dv <- renderPlot({
      df <- dataset()
      x <- df[[input$var1]]
      hist(x, col = 'blue', border = 'white', main = input$var1, xlab = "Observations")
    })
    
    # Print out nice text for the histo page
    output$histogramText <- renderUI({
      return(HTML("<h5>
                  <strong>Color Key:</strong>
                  </h5>
                  <h5 style='color:#008000'>
                  <strong>- Independent Variables</strong>
                  </h5>
                  <h5 style='color:#0000ff'>
                  <strong>- Dependent Variable</strong>
                  </h5> "
        )
      )
    })

    # Correlation matrix
    output$corr <- renderGvis({
      d <- dataset()[,sapply(dataset(),is.integer)|sapply(dataset(),is.numeric)] 
      cor <- as.data.frame(round(cor(d), 2))
      cor <- cbind(Variables = rownames(cor), cor)
      gvisTable(cor) 
    })
    
    # Correlation Heatmap
    output$corrHeatMap <- renderPlot({
      cor <- round(cor(dataset()), 2)
      # Get upper triangle of the correlation matrix
      get_upper_tri <- function(cor){
        cor[lower.tri(cor)]<- NA
        return(cor)
      }
      # Call the upper tri function
      upper_tri <- get_upper_tri(cor)
      melted_cormat <- melt(upper_tri, na.rm = TRUE)
      # Plot
      ggplot(data=melted_cormat, 
             aes(
               Var2,
               Var1,
               fill=value)) +
        scale_fill_gradient2(
          low="tomato",
          high="limegreen",
          mid="grey100", 
          midpoint=0,
          limit=c(-1,1),
          space="Lab", 
          name="Pearson\nCorrelation") +
        theme(
          axis.text.x=element_text(
            angle = 45,
            vjust = 1,
            size = 16,
            hjust = 1),
          axis.text.y=element_text(size = 16),
          axis.title.x=element_text(colour="white"),
          axis.title.y=element_text(colour="white")) +
        geom_tile(color="white") +
        geom_text(aes(label = value)) +
        coord_fixed()
    })
    
    # Print out nice text for the correlation page
    output$correlationText <- renderUI({
      return(HTML("<h5>
                  <strong>Color Key:</strong>
                  </h5>
                  <h5 style='color:limegreen'>
                  <strong>- Positive Correlation</strong>
                  </h5>
                  <h5 style='color:grey'>
                  <strong>- No Correlation</strong>
                  </h5>
                  <h5 style='color:tomato'>
                  <strong>- Negtive Correlation</strong>
                  </h5> "
        )
      )
    })

    # Model
    output$model <- renderPrint({
      summary(model())
    })
    
    # Model ANOVA
    output$modeAnova <- renderPrint({
      anova(model())
    })
    
    # Model Conf. Intervals
    output$modelConfInterval <- renderPrint({
      confint(model(), level=0.90)
    })
    
    # Model CoVaiance
    output$modelCoVariance <- renderPrint({
      vcov(model())
    })
    
    # Auto Model
    output$autoModel <- renderPrint({
      summary(autoModel())
    })
    
    # Auto Model diagnostics
    output$autoModel2 <- renderPrint({
      #coef(model(), 1:5)
      vcov(autoModel(), 1:5)
    })

    # Model residuals
    output$residuals <- renderPlot({
      opar <- par(
        mfrow = c(2,2),
        oma = c(0, 0, 1.1, 0)
        )
      plot(model(), 
           las = 1,
           cex.lab=1.5,
           cex.axis=1.5,
           cex.main=3,
           cex.sub=1.5
           )
      par(opar)
    })
    
    # Model residuals histo
    output$residuals_hist <- renderPlot({
        hist(
          model()$residuals,
          main="Residuals Distribution",
          xlab='Residuals',
          col='darkgreen'
          ) 
    })

    # # hotable
    # output$hotable1 <- renderHotable({
    #   df <- data.frame(String = c('a', 'b', 'c', 'd', 'e','a', 'b', 'c', 'd', 'e'), 
    #                    Numeric1 = numeric(10), 
    #                    Numeric2 = numeric(10))
    #   return(df)
    # }, readOnly = FALSE)
    
    # df <- reactive({
    #   hot.to.df(input$hotable1) # this will convert your input into a data.frame
    # })

    # download report
    output$downloadReport <- downloadHandler(
      filename = function() {
        paste('my-report', sep = '.', switch(
          input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
        ))
      },
      
      content = function(file) {
        src <- normalizePath('report.Rmd')
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'report.Rmd')
        
        library(rmarkdown)
        out <- render('report.Rmd', switch(
          input$format,
          PDF = pdf_document(), HTML = html_document(), Word = word_document()
        ))
        file.rename(out, file)
  })
})
    
    
    
    
    
