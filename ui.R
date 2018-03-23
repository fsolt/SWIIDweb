library(shiny)

shinyUI(fluidPage( 
    helpText(" "),
    tags$head(tags$style(HTML("
        .selectize-input, .selectize-dropdown {
                              font-size: 90%;
                              }
                              "))),
    fluidRow(        
        column(2,
               
               selectInput("country1", label="Country", "United States"), 
               
               selectInput("country2", label="Country", "select"),
               
               selectInput("country3", label="Country", "select"),
               
               selectInput("country4", label="Country", "select"),
               
               
               br(),
               sliderInput("dates", label="Select Years:",
                           min = 1960, max = 2016, 
                           value = c(1975, 2016), sep = "")
        ),
        
        column(3,
               selectInput("series1", label="Variable", 
                           choices = list("Gini, Disposable Income" = "gini_disp"), 
                           selected = "gini_disp"),
               
               selectInput("series2", label="Variable",
                           choices = list("Gini, Disposable Income" = "gini_disp"), 
                           selected = "gini_disp"),
               
               selectInput("series3", label="Variable",
                           choices = list("Gini, Disposable Income" = "gini_disp"), 
                           selected = "gini_disp"),
               
               selectInput("series4", label="Variable",
                           choices = list("Gini, Disposable Income" = "gini_disp"),
                           selected = "gini_disp"),
               
               br(),
               selectInput("theme", "Theme", 
                           choices = list("Standard" = "none",
                                          "Light" = "light",
                                          "Economist" = "econ",
                                          "Few" = "few",
                                          "FiveThirtyEight" = "fte",
                                          "Highcharts" = "hc",
                                          "Pander" = "pander",
                                          "Solarized" = "sol",
                                          "Stata" = "stata",
                                          "Tufte" = "tufte",
                                          "WSJ" = "wsj"),
                           selected = "none"),
               
               downloadButton('downloadPlot', 'Download PDF')
        ),
        
        column(7,
               plotOutput("plot")
        )
    )
))