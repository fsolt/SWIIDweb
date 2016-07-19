library(shiny)

shinyUI(fluidPage( 
    helpText(" "),
    fluidRow(        
        column(2,
               
               selectInput("country1", label="Country", "United States"), 
               
               selectInput("country2", label="Country", "select"),
               
               selectInput("country3", label="Country", "select"),
               
               selectInput("country4", label="Country", "select"),
               
               
               br(),
               sliderInput("dates", label="Select Years:",
                           min = 1960, max = 2015, 
                           value = c(1975, 2015), sep = "")
        ),
        
        column(2,
               selectInput("series1", label="Variable", 
                           choices = list("Net Inequality" = "gini_net"), 
                           selected = "gini_net"),
               
               selectInput("series2", label="Variable",
                           choices = list("Net Inequality" = "gini_net"), 
                           selected = "gini_net"),
               
               selectInput("series3", label="Variable",
                           choices = list("Net Inequality" = "gini_net"), 
                           selected = "gini_net"),
               
               selectInput("series4", label="Variable",
                               choices = list("Net Inequality" = "gini_net"),
                               selected = "gini_net"),
               
               br(),
               selectInput("theme", "Theme", 
                           choices = list("Standard" = "none",
                                          "Light" = "light",
                                          "Economist" = "econ",
                                          "Solarized" = "sol",
                                          "Stata" = "stata",
                                          "Tufte" = "tufte"),
                           selected = "none"),
               
               downloadButton('downloadPlot', 'Download PDF')
        ),
        
        column(8,
               plotOutput("plot")
        )
    )
))