library(shiny)

shinyUI(fluidPage( 
    helpText(" "),
    fluidRow(        
        column(2,
               
               selectInput("country1", label="Country", "United States"), 
               
               selectInput("country2", label="Country", "select a country"),
               
               conditionalPanel(
                   condition = "input.country2 != 'select a country'",             
                   selectInput("country3", label="Country", "select a country")
               ),
               
               conditionalPanel(
                   condition = "input.country3 != 'select a country'",             
                   selectInput("country4", label="Country", "select a country")
               ),
               
               
               br(),
               sliderInput("dates", label="Select Years:",
                           min = 1960, max = 2013, 
                           value = c(1975, 2013), format = "####")
        ),
        
        column(2,
               selectInput("series1", label="Variable", 
                           choices = list("Net Inequality" = "gini_net"), 
                           selected = "gini_net"),
               
               #            conditionalPanel(
               #              condition = "input.country2 != 'select a country'",
               selectInput("series2", label="Variable",
                           choices = list("Net Inequality" = "gini_net"), 
                           selected = "gini_net"),
               #            ),
               
               conditionalPanel(
                   condition = "input.country2 != 'select a country'",
                   selectInput("series3", label="Variable",
                               choices = list("Net Inequality" = "gini_net"), 
                               selected = "gini_net")
               ), 
               
               conditionalPanel(
                   condition = "input.country3 != 'select a country'",
                   selectInput("series4", label="Variable",
                               choices = list("Net Inequality" = "gini_net"),
                               selected = "gini_net")
               ),
               
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