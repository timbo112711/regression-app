# The Regression Shop
# V 1.0
# This web app is built to perform both simple and multiple linear regression.
library(shiny)

## Start the UI renderer
shinyUI(
  # Create the page that has a sidebar
  pageWithSidebar(
    # The TITLE!
    headerPanel("The Regression Shop"),
    # Create the side bar panel (this is where the up-loader and drop downs live)
    sidebarPanel(
      # The file input/uploader
      fileInput('datafile', 
                'Choose CSV file',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain')
                ),
      textInput(
        "name",
        label=h5("Name"),
        value="Name"
        ),
      HTML('</br>'),
      # Variable 1
      uiOutput("varselect1"),
      # Variable 2
      uiOutput("varselect2"),
      # Instructions for using this app
      h2("Instructions"),
      p("Step 1: Upload your dataset."),
      p("Step 2: Choose your y variable. This is the variable you are looking to predict."),
      p("Step 3: Choose your x variable(s). The variable(s) you want to use to predict your y."),
      p("Step 4: Use the Data, Summary Statistics, Histograms, and Correlation tabs to perform EDA
        (Exploratory Data Analysis). This is an important step which will allow you to find issues with your data."),
      p("Step 5: Use the Modeling and Residuals tabs to build and assess your model. IMPORTANT NOTE: You are allowed to
        take away, or add in, variables via step 3. This will change your model in real time."),
      h4("Auto Variable Selection Modeling Feature"),
      p("The last tab uses an automatic variable selection method to auotmatically select the best variables for your model.
        Please see that tab for more detail and instructions."),
      HTML('</br>'),
      radioButtons(
        'format',
        h5('Document format'),
        c('HTML', 'Word'),
        inline = TRUE
        ),
      downloadButton('downloadReport')
      ),
    # The main panel where all the shit happens
    mainPanel(
      # Make it so errors don't show up on the page
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"),
      tabsetPanel(type = "tabs",
                  tabPanel("Data",
                           HTML("</br> Please upload your dataset! </br> </br>"),
                           numericInput(
                             "obs",
                             label=h5("Number of observations to view"),
                             10
                            ),
                           tableOutput("view")
                           ),
                  
                  tabPanel("Summary Statistics",
                           h4("Summary Statistics For Each Variable"),
                           verbatimTextOutput("summary"),
                           textInput(
                             "text_summary",
                             label="Interpretation",
                             value="Enter text...")
                           ),
                  tabPanel("Histograms",
                           h4("Distributions For Each Variable"),
                           uiOutput("histogramText"),
                           plotOutput("distPlot_iv"),
                           textInput(
                             "text_hist_iv1",
                             label="Interpretation",
                             value="Enter text..."
                             ),
                           plotOutput("distPlot_dv"),
                           textInput(
                             "text_hist_dv",
                             label="Interpretation",
                             value="Enter text..."
                             )
                           ),
                  tabPanel("Correlations",
                           h4("Correlation Between Variables"),
                           uiOutput("correlationText"),
                           HTML('</br> </br>'),
                           plotOutput("corrHeatMap",
                                      height=500,
                                      width=700
                                      ),
                           htmlOutput("corr"),
                           HTML('</br> </br>'),
                           textInput(
                             "text_correlation",
                             label="Interpretation",
                             value="Enter text...")
                           ),
                  
                  tabPanel("Modeling",
                           uiOutput("modelFormula"),
                           HTML('</br> </br>'),
                           h4("Model Output"),
                           verbatimTextOutput("model"),
                           HTML('</br> </br>'),
                           h4("Analysis of Variance For Model Coefs"),
                           verbatimTextOutput("modeAnova"),
                           HTML('</br> </br>'),
                           h4("Confidence Intervals"),
                           verbatimTextOutput("modelConfInterval"),
                           HTML('</br> </br>'),
                           h4("Estimated Covariances Between The Parameter Estimates"),
                           verbatimTextOutput("modelCoVariance"),
                           HTML('</br> </br>'),
                           textInput(
                             "text_model",
                              label="Interpretation",
                              value="Enter text..."
                             )
                          ),
                  
                  tabPanel("Residuals",
                           h4("Model Diagnostics: Residual Plots"),
                           HTML('</br> </br>'),
                           plotOutput("residuals",
                                      height=1000),
                           plotOutput("residuals_hist"),
                           textInput(
                             "text_residuals",
                              label="Interpretation",
                              value="Enter text..."
                             )
                          ),
                           
                   tabPanel("Auto Variable Selection Modeling",
                            h4("Auto Modeling Output"),
                            h5("ALL VARIABLES MUST BE SELECTED FOR THIS FEATURE."),
                            p("The below table displays the auto variable selection using the backward selection method.
                              There are 5 subsets of data being taking into consideration when running throught the auto variable selection.
                              Each subset may or may not have all of the variables present in the data. The first subset (1) contains the most
                              relevant variables, second subset (2) contains the second most relevant variables, and so on till the 5th subset
                              is reached."),
                            p("The results will be displayed in a table where the rows are each subset and the columns are each variable. If a 
                              variable has an * under it, this means that the variable was included in the model for that subset."),
                            verbatimTextOutput("autoModel"),
                            HTML('</br> </br>'),
                            h4("Auto Modeling Coeficients For Each Subset"),
                            p("The results shown below are for each unique subset of variables. The first subset (1) contains the most
                              relevant variables, second subset (2) contains the second most relevant variables, and so on till the 5th subset
                              is reached."),
                            verbatimTextOutput("autoModel2"),
                            HTML('</br> </br>'),
                            textInput(
                              "text_atuo_model",
                              label="Interpretation",
                              value="Enter text..."
                            )
                          )
                        )
                      )
                    )
                  )

