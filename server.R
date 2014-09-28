library(shiny)
library(ggplot2)
library(reshape2)
library(plyr)
library(gridExtra)
library(ggthemes)

ch4 <- list("Net Inequality" = "gini_net",
            "Market Inequality" = "gini_market",
            "Relative Redistribution" = "rel_red",
            "Absolute Redistribution" = "abs_red")
ch2 <- list("Net Inequality" = "gini_net",
            "Market Inequality" = "gini_market")
ch1 <- list("Net Inequality" = "gini_net")

swiid <- read.csv("SWIIDv5_0summary.csv", as.is=T)
swiid <- ddply(swiid, .(country), mutate, obs = length(gini_net))
swiid <- swiid[swiid$obs>1, ]
cc <- ddply(swiid, .(country), summarize, ch = ifelse(sum(!is.na(rel_red))>0, "ch4", "ch2" ))

shinyServer(function(input, output, session) {
  
  observe({
    updateSelectInput(session, "country1", choices = cc$country, selected = "United States")
  })
  observe({
    updateSelectInput(session, "country2", choices = c("select a country", cc$country), selected = "select a country")
  })
  observe({
    updateSelectInput(session, "country3", choices = c("select a country", cc$country), selected = "select a country")
  })
  observe({
    updateSelectInput(session, "country4", choices = c("select a country", cc$country), selected = "select a country")
  })
  
  observe({
    cc1 = cc[cc==input$country1, "ch"]
    updateSelectInput(session, "series1", choices = get(cc1), selected="gini_net" )    
  })
  
  observe({
    if(input$country2 != "select a country") {
      cc2 = cc[cc==input$country2, "ch"]
    } else cc2 <- "ch1"
    updateSelectInput(session, "series2", choices = get(cc2), selected="gini_net" )    
  })
  
  observe({
    if(input$country3 != "select a country") {
      cc3 = cc[cc==input$country3, "ch"]
    } else cc3 <- "ch1"
    updateSelectInput(session, "series3", choices = get(cc3), selected="gini_net" )    
  })
  
  observe({
    if(input$country4 != "select a country") {
      cc4 = cc[cc==input$country4, "ch"]
    } else cc4 <- "ch1"
    updateSelectInput(session, "series4", choices = get(cc4), selected="gini_net" )    
  })
  
  plotInput <- reactive({
    # Get data for selected countries and series
    s1 <- data.frame(swiid[swiid$country==input$country1, 
                           c("country", "year", input$series1, paste0(input$series1, "_se"))])
    
    if(input$country2 != "select a country"){
      s2 <- data.frame(swiid[swiid$country==input$country2, 
                             c("country", "year", input$series2, paste0(input$series2, "_se"))])
      s1 <- merge(s1, s2, all=T)
      
      if(input$country3 != "select a country"){
        s2 <- data.frame(swiid[swiid$country==input$country3, 
                               c("country", "year", input$series3, paste0(input$series3, "_se"))])
        s1 <- merge(s1, s2, all=T)
        if(input$country4 != "select a country"){
          s2 <- data.frame(swiid[swiid$country==input$country4, 
                                 c("country", "year", input$series4, paste0(input$series4, "_se"))])
          s1 <- merge(s1, s2, all=T)
        }   
      } 
    }    
    s1 <- melt(s1, id.vars=c("country", "year"), na.rm=T)
    s2 <- s1[grepl("se", s1$variable), c("country", "year", "value")]
    s1 <- s1[!grepl("se", s1$variable), ]
    s1 <- cbind(s1, s2[, 3])
    names(s1)[5] <- "value_se"
    s1$variable <- gsub("gini_net", "Gini Index, Net Income", s1$variable)
    s1$variable <- gsub("gini_market", "Gini Index, Market Income", s1$variable)
    s1$variable <- gsub("rel_red", "Relative Redistribution", s1$variable)
    s1$variable <- gsub("abs_red", "Absolute Redistribution", s1$variable)
    
    # Modify ylabel and legend title to reflect selected countries and series
    if (length(table(s1$variable))==1) {
      ylabel <- paste("SWIID", s1$variable[1])
      s1$series <- s1$country
    } else ylabel <- ""
    if (length(table(s1$country))==1) {
      c.title <- s1$country[1]
      s1$series <- s1$variable
    } else c.title <- ""
    if (length(table(s1$variable))>1 & length(table(s1$country))>1) {
      s1$series <- paste(s1$country, s1$variable, sep=", ")
    }
    
    # Basic plot
    p <- ggplot(s1, aes(x=year, y=value, colour=series)) + 
      geom_line() +
      geom_ribbon(aes(ymin = value-1.96*value_se, ymax = value+1.96*value_se, 
                      fill=series, linetype=NA), alpha = .25) +
      coord_cartesian(xlim=c(input$dates[1],input$dates[2])) +
      labs(x = "Year", y = ylabel)
    
    # Apply themes and add source
    if (input$theme=="light") {
      arrangeGrob(
        p + theme_light() + scale_fill_discrete(name = c.title) + scale_colour_discrete(name = c.title),
        sub=textGrob("Source: Standardized World Income Inequality Database v5.0 (Solt 2014).", x=0, hjust=-0.1, vjust=0.1, 
                     gp=gpar(fontsize=10)) 
      )      
    } else if (input$theme=="tufte") {
      arrangeGrob(
        p + theme_tufte() + scale_fill_grey(name = c.title) + 
          scale_colour_grey(name = c.title),
        sub=textGrob("Source: Standardized World Income Inequality Database v5.0 (Solt 2014).", x=0, hjust=-0.1, vjust=0.1, 
                     gp=gpar(fontsize=10, fontfamily="serif")) 
      )     
    } else if (input$theme=="econ") {
      arrangeGrob(
        p + theme_economist() + scale_fill_economist(name = c.title) + scale_colour_economist(name = c.title),
        sub=textGrob("Source: Standardized World Income Inequality Database v5.0 (Solt 2014).", x=0, hjust=-0.05, vjust=-1, 
                     gp=gpar(fontsize=10))
      )     
    } else if (input$theme=="sol") {
      arrangeGrob(
        p + theme_solarized() + scale_fill_solarized("blue", name = c.title) + 
          scale_colour_solarized("blue", name = c.title),
        sub=textGrob("Source: Standardized World Income Inequality Database v5.0 (Solt 2014).", x=0, hjust=-0.05, vjust=-1, 
                     gp=gpar(fontsize=10))
      )     
    } else if (input$theme=="stata") {
      arrangeGrob(
        p + theme_stata() + scale_fill_stata(name = c.title) + 
          scale_colour_stata(name = c.title),
        sub=textGrob("Source: Standardized World Income Inequality Database v5.0 (Solt 2014).", x=0, hjust=-0.05, vjust=-1, 
                     gp=gpar(fontsize=10))
      )     
    } else {
      arrangeGrob(
        p + scale_fill_discrete(name = c.title) + scale_colour_discrete(name = c.title),
        sub=textGrob("Source: Standardized World Income Inequality Database v5.0 (Solt 2014).", x=0, hjust=-0.1, vjust=0.1, 
                     gp=gpar(fontsize=10))      
      ) 
    }
    
  })
  
  output$plot <- renderPlot({     
    print(plotInput())    
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() { paste0('SWIID', '.pdf') },
    content = function(file) {
      pdf(file, width = 6, height = 4)
        print(plotInput())
      dev.off()
    })
  
})