# The Regression Shop
# V 1.0
# This web app is built to perform both simple and multiple linear regression.
library(shiny)
library(ggplot2)
library(DT)
library(plyr)
library(dplyr)
library(googleVis)
library(leaps)
library(reshape2)
library(rmarkdown)
library(tinytex)
library(memisc)

# Initiate the severer
shinyServer(
  function(input, output, session) {
    # Variable #1
    output$varselect1 <- renderUI({
      selectInput("var1",
                  label="Choose your y:",
                  choices=names(dataset()),
                  selected=names(dataset())[1],
                  multiple=FALSE)  
    })
    
    # Variable #2
    output$varselect2 <- renderUI({
      selectInput("var2",
                  label="Choose your x's:",
                  choices=names(dataset()),
                  selected=names(dataset())[2],
                  multiple=TRUE)  
    })
    
    # Read in the data
    dataset <- reactive({
      infile <- input$datafile
      if (is.null(infile)) {
        return(NULL)
      }
      else {read.csv(infile$datapath)}
    })
                      
    # Print out the formula
    output$modelFormula <- renderUI({
      if (length(input$var2) == 1) {
        equation1 <- as.formula(paste(input$var1, '~', input$var2))
        return(HTML("<h4>
                  <strong>Regression Equation:</strong>
                    </h4>", equation1)
        )
      }
      else if (length(input$var2) == 2) {
        equation2 <- as.formula(paste(input$var1, '~', input$var2[1], '+',  input$var2[2]))
        return(HTML("<h4>
                  <strong>Regression Equation:</strong>
                    </h4>", equation2)
        )
      }
      else if (length(input$var2) == 3) {
        equation3 <- as.formula(paste(input$var1, '~', input$var2[1], '+',  input$var2[2], '+',  input$var2[3]))
        return(HTML("<h4>
                  <strong>Regression Equation:</strong>
                    </h4>", equation3)
        )
      } 
      else if (length(input$var2) == 4) {
        equation4 <- as.formula(paste(input$var1, '~', input$var2[1], '+',  input$var2[2], '+',  input$var2[3], '+',  input$var2[4]))
        return(HTML("<h4>
                  <strong>Regression Equation:</strong>
                    </h4>", equation4)
        )
      }
      else if (length(input$var2) == 5) {
        equation5 <- as.formula(paste(input$var1, '~', input$var2[1], '+',  input$var2[2], '+',  input$var2[3], '+',  input$var2[4], '+',  input$var2[5]))
        return(HTML("<h4>
                  <strong>Regression Equation:</strong>
                    </h4>", equation5)
        )
      }
    })
    
    # Regression formula
    regFormula <- reactive({
      if (length(input$var2) == 1) {
        equation1 <- as.formula(paste(input$var1, '~', input$var2))
        return(equation1)
      }
      else if (length(input$var2) == 2) {
        equation2 <- as.formula(paste(input$var1, '~', input$var2[1], '+',  input$var2[2]))
        return(equation2)
      }
      else if (length(input$var2) == 3) {
        equation3 <- as.formula(paste(input$var1, '~', input$var2[1], '+',  input$var2[2], '+',  input$var2[3]))
        return(equation3)
      } 
      else if (length(input$var2) == 4) {
        equation4 <- as.formula(paste(input$var1, '~', input$var2[1], '+',  input$var2[2], '+',  input$var2[3], '+',  input$var2[4]))
        return(equation4)
      }
      else if (length(input$var2) == 5) {
        equation5 <- as.formula(paste(input$var1, '~', input$var2[1], '+',  input$var2[2], '+',  input$var2[3], '+',  input$var2[4], '+',  input$var2[5]))
        return(equation5)
      }
    })
    
    # Model
    model <- reactive({
      regression_model <- lm(regFormula(), data=dataset())
      return(regression_model)
    })
    
    # Auto model
    autoModel <- reactive({
      auto_regression_model <- regsubsets(
        regFormula(),
        data=dataset(),
        method='backward',
        nbest=1
      )
      return(auto_regression_model)
    })
    
    # Data view 
    output$view <- renderTable({
      head(dataset(),  n=input$obs)
    })
    
    # Summary statistics
    output$summary <- renderPrint({
      summary(cbind(dataset()[input$var1], dataset()[input$var2]))
    })
    
    # Independent Variable Histograms
    output$distPlot_iv <- renderPlot({
      # Histogram plot
      histo_plot <- function(melted_df) {
        ggplot(melted_df,
               aes(x=value)) +
          geom_histogram(
            bins=10,
            col='black',
            fill='darkgreen',
            alpha=0.5) + 
          facet_wrap(
            ~variable,
            scales='free_x') +
          xlab("Observations") +
          ylab("Frequency") +
          theme(
            axis.text.y=element_text(size=13),
            axis.text.x=element_text(size=13),
            axis.title.x=element_text(size=18),
            axis.title.y=element_text(size=18),
            strip.text=element_text(size=19)
          )
      }

      if (length(input$var2) == 1) {
        df <- subset(dataset(), select = input$var2[1])
        melted_df <- melt(df, na.rm = FALSE)
        return(histo_plot(melted_df))
      }
      else if (length(input$var2) == 2) {
        df <- subset(dataset(), select = c(input$var2[1], input$var2[2]))
        melted_df <- melt(df, na.rm = FALSE)
        return(histo_plot(melted_df))
      }
      else if (length(input$var2) == 3) {
        df <- subset(dataset(), select = c(input$var2[1], input$var2[2], input$var2[3]))
        melted_df <- melt(df, na.rm = FALSE)
        return(histo_plot(melted_df))
      } 
      else if (length(input$var2) == 4) {
        df <- subset(dataset(), select = c(input$var2[1], input$var2[2], input$var2[3], input$var2[4]))
        melted_df <- melt(df, na.rm = FALSE)
        return(histo_plot(melted_df))
      }
      else if (length(input$var2) == 5) {
        df <- subset(dataset(), select = c(input$var2[1], input$var2[2], input$var2[3], input$var2[4], input$var2[5]))
        melted_df <- melt(df, na.rm = FALSE)
        return(histo_plot(melted_df))
      }
    })

    # Dependent variable Histogram
    output$distPlot_dv <- renderPlot({
      # Histogram plot
      dependent_histo_plot <- function(melted_df) {
        ggplot(melted_df,
               aes(x=value)) +
          geom_histogram(
            bins=10,
            col='black',
            fill='blue',
            alpha=0.75) + 
          facet_wrap(~variable) +
          xlab("Observations") +
          ylab("Frequency") +
          theme(
            axis.text.y=element_text(size=13),
            axis.text.x=element_text(size=13),
            axis.title.x=element_text(size=18),
            axis.title.y=element_text(size=18),
            strip.text=element_text(size=19)
          )
      }
      # Subset data
      df <- subset(dataset(), select=input$var1)
      melted_df <- melt(df, na.rm = FALSE)
      return(dependent_histo_plot(melted_df))
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
      calculate_correlation <- function(corr_data){
        correlation_coef <- as.data.frame(round(cor(corr_data), 2))
        correlation <- cbind(Variables = rownames(correlation_coef), correlation_coef)
        return(correlation)
      }
      # Roll through the inputs
      if (length(input$var2) == 1) {
        corr_data <- subset(dataset(), select = c(input$var1, input$var2[1]))
        corr_table <- gvisTable(calculate_correlation(corr_data))
        return(corr_table)
      }
      else if (length(input$var2) == 2) {
        corr_data <- subset(dataset(), select = c(input$var1, input$var2[1],  input$var2[2]))
        corr_table <- gvisTable(calculate_correlation(corr_data))
        return(corr_table)
      }
      else if (length(input$var2) == 3) {
        corr_data <- subset(dataset(), select = c(input$var1, input$var2[1],  input$var2[2], input$var2[3]))
        corr_table <- gvisTable(calculate_correlation(corr_data))
        return(corr_table)
      } 
      else if (length(input$var2) == 4) {
        corr_data <- subset(dataset(), select = c(input$var1, input$var2[1],  input$var2[2], input$var2[3], input$var2[4]))
        corr_table <- gvisTable(calculate_correlation(corr_data))
        return(corr_table)
      }
      else if (length(input$var2) == 5) {
        corr_data <- subset(dataset(), select = c(input$var1, input$var2[1],  input$var2[2], input$var2[3], input$var2[4], input$var2[5]))
        corr_table <- gvisTable(calculate_correlation(corr_data))
        return(corr_table)
      }
    })

    # Correlation Heatmap
    output$corrHeatMap <- renderPlot({
      # Heat map
      plot_heat_map <- function(melted_cormat) {
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
      }
      calculate_correlation <- function(corr_data){
        corr_data[is.na(corr_data)] <- 0
        correlation <- round(cor(corr_data), 2)
        return(correlation)
      }
      # Roll through the inputs
      if (length(input$var2) == 1) {
        corr_data <- subset(dataset(), select = c(input$var1, input$var2[1]))
        cor <- calculate_correlation(corr_data)
        melted_cormat <- melt(cor, na.rm = FALSE)
        return(plot_heat_map(melted_cormat))
      }
      else if (length(input$var2) == 2) {
        corr_data <- subset(dataset(), select = c(input$var1, input$var2[1],  input$var2[2]))
        cor <- calculate_correlation(corr_data)
        melted_cormat <- melt(cor, na.rm = FALSE)
        return(plot_heat_map(melted_cormat))
      }
      else if (length(input$var2) == 3) {
        corr_data <- subset(dataset(), select = c(input$var1, input$var2[1],  input$var2[2], input$var2[3]))
        cor <- calculate_correlation(corr_data)
        melted_cormat <- melt(cor, na.rm = FALSE)
        return(plot_heat_map(melted_cormat))
      } 
      else if (length(input$var2) == 4) {
        corr_data <- subset(dataset(), select = c(input$var1, input$var2[1],  input$var2[2], input$var2[3], input$var2[4]))
        cor <- calculate_correlation(corr_data)
        melted_cormat <- melt(cor, na.rm = FALSE)
        return(plot_heat_map(melted_cormat))
      }
      else if (length(input$var2) == 5) {
        corr_data <- subset(dataset(), select = c(input$var1, input$var2[1],  input$var2[2], input$var2[3], input$var2[4], input$var2[5]))
        cor <- calculate_correlation(corr_data)
        melted_cormat <- melt(cor, na.rm = FALSE)
        return(plot_heat_map(melted_cormat))
      }
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
      mtable(model())
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
      #summary(autoModel())
      if (length(input$var2) == 1) {
        return("Need more than one variable here!!!")
      }
      else if (length(input$var2) == 2) {
        auto_model_summary <- summary(autoModel())
        return(auto_model_summary)
      }
      else if (length(input$var2) == 3) {
        auto_model_summary <- summary(autoModel())
        return(auto_model_summary)
      } 
      else if (length(input$var2) == 4) {
        auto_model_summary <- summary(autoModel())
        return(auto_model_summary)
      }
      else if (length(input$var2) == 5) {
        auto_model_summary <- summary(autoModel())
        return(auto_model_summary)
      }
    })

    # Auto Model diagnostics
    output$autoModel2 <- renderPrint({
      # Roll through the inputs
      if (length(input$var2) == 1) {
        return("Need more than one variable here!!!")
      }
      else if (length(input$var2) == 2) {
        auto_model_coef <- coef(autoModel(), 1:2)
        return(auto_model_coef)
      }
      else if (length(input$var2) == 3) {
        auto_model_coef <- coef(autoModel(), 1:3)
        return(auto_model_coef)
      } 
      else if (length(input$var2) == 4) {
        auto_model_coef <- coef(autoModel(), 1:4)
        return(auto_model_coef)
      }
      else if (length(input$var2) == 5) {
        auto_model_coef <- coef(autoModel(), 1:5)
        return(auto_model_coef)
      }
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
      residual_histo_plot <- function(model_residuals) {
        hist(
          model()$residuals,
          main="Residuals Distribution",
          xlab='Residuals',
          col='orange'
          )
      }
      # Roll through the inputs
      if (length(input$var2) == 1) {
        model_residuals <- model()$residuals
        return(residual_histo_plot(model_residuals))
      }
      else if (length(input$var2) == 2) {
        model_residuals <- model()$residuals
        return(residual_histo_plot(model_residuals))
      }
      else if (length(input$var2) == 3) {
        model_residuals <- model()$residuals
        return(residual_histo_plot(model_residuals))
      } 
      else if (length(input$var2) == 4) {
        model_residuals <- model()$residuals
        return(residual_histo_plot(model_residuals))
      }
      else if (length(input$var2) == 5) {
        model_residuals <- model()$residuals
        return(residual_histo_plot(model_residuals))
      }
    })

    # Download report
    output$downloadReport <- downloadHandler(
      filename = function() {
        paste('my-report', sep = '.', switch(
          input$format, HTML = 'html', Word = 'docx'
        ))
      },
      content = function(file) {
        src <- normalizePath('report.Rmd')
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'report.Rmd')
        # Render it!
        out <- render('report.Rmd', switch(
          input$format,
          HTML = html_document(), Word = word_document()
        ))
        file.rename(out, file)
  })
})
