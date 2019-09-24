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
      # Variable 3
      uiOutput("varselect3"),
      # Variable 4
      uiOutput("varselect4"),
      # Variable 5
      uiOutput("varselect5"),
      # Variable 6
      uiOutput("varselect6"),
      HTML('</br>'),
      radioButtons(
        'format',
        h5('Document format'),
        c('PDF', 'HTML', 'Word'),
        inline = TRUE
        ),
      downloadButton('downloadReport'),
      # Instructions for using this app
      h2("Instructions"),
      p("Coming soon to an App near you!!!!!!!")
      #img(src='Harmelin_Media_Logo.png', height="80%", width="80%", align = "center")
      ),
    # The main panel where all the shit happens
    mainPanel(
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
                           plotOutput("distPlot_iv1"),
                           textInput(
                             "text_hist_iv1",
                             label="Interpretation",
                             value="Enter text..."
                             ),
                           
                           plotOutput("distPlot_iv2"),
                           textInput(
                             "text_hist_iv2",
                             label="Interpretation",
                             value="Enter text..."
                             ),
                  
                           plotOutput("distPlot_iv3"),
                           textInput(
                             "text_hist_iv3",
                              label="Interpretation",
                             value="Enter text..."
                             ),
                          
                           plotOutput("distPlot_iv4"),
                           textInput(
                             "text_hist_iv4",
                             label="Interpretation",
                             value="Enter text..."
                             ),
      
                           plotOutput("distPlot_iv5"),
                           textInput(
                             "text_hist_iv5",
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

