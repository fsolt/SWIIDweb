library(shiny)

shinyUI(fluidPage( 
    helpText(" "),
    mainPanel(        
        tabsetPanel(
            tabPanel(title = "Cross-Country Comparison", 
                     column(2,
                            
                            selectInput("country1", label="Country", "United States"), 
                            
                            selectInput("country2", label="Country", "select"),
                            
                            selectInput("country3", label="Country", "select"),
                            
                            selectInput("country4", label="Country", "select"),
                            
                            
                            br(),
                            numericInput("ci", label = "Confidence Interval (%):", value = 95, 
                                         min = 0, max = 100, step = 1),
                            checkboxInput("cilayer", label = "Double CI View", value = FALSE)
                            
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
                                                       "FiveThirtyEight" = "fte",
                                                       "Solarized" = "sol",
                                                       "Stata" = "stata",
                                                       "Tufte" = "tufte",
                                                       "Few" = "few"),
                                        selected = "none"),
                            
                            downloadButton('downloadPlot', 'Download PDF')
                     ),
                     
                     column(8,
                            sliderInput("dates", label="Select Years:",
                                        min = 1960, max = 2015, 
                                        value = c(1975, 2015), sep = "", width = "90%"),
                            plotOutput("plot")
                     )
                     
                     ),
            
            tabPanel(title = "Single Country-Year Info",
                     fluidRow(column(4, offset = 2, selectInput("country", label="Country", "United States")), 
                              column(2, numericInput("year", label="Year", 2014)), 
                              column(3, selectInput("series", label="Variable", 
                                                    choices = list("Net Inequality" = "gini_net"), 
                                                    selected = "gini_net"))),
                              
                     fluidRow(column(9, offset = 2, tableOutput("countryYear")) )
                     )
        )
    )
))