library(shiny)
library(datasets)
library(ggplot2)
library(reshape2)
library(dplyr)
library(grid)
library(gridExtra)
library(ggthemes)

ch4 <- list("Net Inequality" = "gini_net",
            "Market Inequality" = "gini_market",
            "Relative Redistribution" = "rel_red",
            "Absolute Redistribution" = "abs_red")
ch2 <- list("Net Inequality" = "gini_net",
            "Market Inequality" = "gini_market")
ch1 <- list("Net Inequality" = "gini_net")

swiid <- read.csv("SWIIDv5_1summary.csv", as.is=T) %>% 
    group_by(country) %>% 
    mutate(obs = n()) %>% 
    ungroup() %>% 
    filter(obs > 1)

cc <- swiid %>% 
    group_by(country) %>% 
    summarize(ch = ifelse(sum(!is.na(rel_red))>0, "ch4", "ch2" ))

shinyServer(function(input, output, session) {
    
    observe({
        updateSelectInput(session, "country1", choices = cc$country, selected = "United States")
    }) # reactive program for the 1st country choice.
    
    observe({
        cc1 = as.character(cc[cc==input$country1, "ch"])
        updateSelectInput(session, "series1", choices = get(cc1), selected="gini_net" )  
    }) # first country's data type 
    
    
    
    
    observe({
        updateSelectInput(session, "country2", choices = c("select", cc$country), selected = "select")
    }) # reactive program for the 2nd country choice.
    
    observe({
        if(input$country2 != "select") {
            cc2 = as.character(cc[cc==input$country2, "ch"])
        } else cc2 <- "ch1"
        updateSelectInput(session, "series2", choices = get(cc2), selected="gini_net" )  
    }) # second country's data type 
    
    
    
    
    
    observe({
        updateSelectInput(session, "country3", choices = c("select", cc$country), selected = "select")
    }) # reactive program for the 3rd country choice.

    observe({
        if(input$country3 != "select") {
            cc3 = as.character(cc[cc==input$country3, "ch"])
        } else cc3 <- "ch1"
        updateSelectInput(session, "series3", choices = get(cc3), selected="gini_net" )  
    }) # third country's data type 
    
    
    
    
    
    observe({
        updateSelectInput(session, "country4", choices = c("select", cc$country), selected = "select")
    }) # reactive program for the 4th country choice.
    
    observe({
        if(input$country4 != "select") {
            cc4 = as.character(cc[cc==input$country4, "ch"])
        } else cc4 <- "ch1"
        updateSelectInput(session, "series4", choices = get(cc4), selected="gini_net" )  
    }) # fourth country's data type 
    
    
    
    
    
    plotInput <- reactive({
        # Get data for selected countries and series
        s1 <- data.frame(swiid[swiid$country==input$country1, 
                               c("country", "year", input$series1, paste0(input$series1, "_se"))])
        
        if(input$country2 != "select") {
            s2 <- data.frame(swiid[swiid$country==input$country2, 
                                   c("country", "year", input$series2, paste0(input$series2, "_se"))])
            s1 <- merge(s1, s2, all=T)
        }
            
        if(input$country3 != "select") {
                s3 <- data.frame(swiid[swiid$country==input$country3, 
                                       c("country", "year", input$series3, paste0(input$series3, "_se"))])
                s1 <- merge(s1, s3, all=T)
        }
                
        if(input$country4 != "select"){
            s4 <- data.frame(swiid[swiid$country==input$country4, 
                                   c("country", "year", input$series4, paste0(input$series4, "_se"))])
            s1 <- merge(s1, s4, all=T)
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
        s1 <- s1[s1$year >= input$dates[1] & s1$year <= input$dates[2], ]
        
        # Modify ylabel and legend title to reflect selected countries and series
        if (length(table(s1$variable))==1) {
            ylabel <- paste("SWIID", s1$variable[1])
            s1$series <- s1$country
        } else ylabel <- ""
        if (length(table(s1$country))==1) {
            c_title <- s1$country[1]
            s1$series <- s1$variable
        } else c_title <- ""
        if (length(table(s1$variable))>1 & length(table(s1$country))>1) {
            s1$series <- paste(s1$country, s1$variable, sep=", ")
        }

        note1 <- "Solid lines indicate mean estimates; shaded regions indicate the associated 95% confidence intervals.\nSource: Standardized World Income Inequality Database v5.1 (Solt 2016)."
                
        # Basic plot
        
        tvalue <- abs(qt((1 - input$ci/100)/2, 1000))
        tvalueFix <- c(abs(qt((1 - 95/100)/2, 1000)),abs(qt((1 - 99/100)/2, 1000))) 
        
        
        p <- ggplot(s1, aes(x=year, y=value, colour=series)) + 
                    geom_line() +
                    geom_ribbon(aes(ymin = value-tvalue*value_se, ymax = value+tvalue*value_se, fill=series, linetype=NA), alpha = .25) +
                    coord_cartesian(xlim=c(input$dates[1],input$dates[2])) +
                    labs(list(x = "Year", y = ylabel, caption = note1)) 
        
        # for doulbe CI view (95 + 99)
       if(input$cilayer){
         p <- ggplot(s1, aes(x=year, y=value, colour=series)) + 
                       geom_line() +
                       geom_ribbon(aes(ymin = value-tvalueFix[2]*value_se, ymax = value+tvalueFix[2]*value_se, fill=series, linetype=NA), alpha = .4) +
                       geom_ribbon(aes(ymin = value-tvalueFix[1]*value_se, ymax = value+tvalueFix[1]*value_se, linetype=NA), fill="grey70", alpha = .15) +
                       coord_cartesian(xlim=c(input$dates[1],input$dates[2])) +
                       labs(list(x = "Year", y = ylabel, plot.caption = note1))
        } 

        
    

        hjust1 <- 0
        hjust2 <- 0
        vjust1 <- .2
        vjust2 <- .03
        
        # Apply themes and add source
        if (input$theme=="light") {
            p + theme_light() + 
                scale_fill_discrete(name = c_title) + 
                scale_colour_discrete(name = c_title) +
                theme(plot.caption = element_text(size = 7))
        } else if (input$theme=="tufte") {
            p + theme_tufte() + 
                scale_fill_grey(name = c_title) + 
                scale_colour_grey(name = c_title) +
                theme(plot.caption = element_text(size = 7))
        } else if (input$theme=="econ") {
            p + theme_economist() + 
                scale_fill_economist(name = c_title) + 
                scale_colour_economist(name = c_title) +
                theme(plot.caption = element_text(size = 7))
        } else if (input$theme=="fte") {
            p + theme_fivethirtyeight() + 
                scale_fill_fivethirtyeight(name = c_title) + 
                scale_colour_fivethirtyeight(name = c_title) +
                theme(plot.caption = element_text(size = 7))
        } else if (input$theme=="few") {
            p + theme_few() + 
                scale_fill_few(name = c_title) + 
                scale_colour_few(name = c_title) +
                theme(plot.caption = element_text(size = 7))
        } else if (input$theme=="sol") {
            p + theme_solarized() + 
                scale_fill_solarized("blue", name = c_title) + 
                scale_colour_solarized("blue", name = c_title) +
                theme(plot.caption = element_text(size = 7))
        } else if (input$theme=="stata") {
            p + theme_stata() + 
                scale_fill_stata(name = c_title) + 
                scale_colour_stata(name = c_title) +
                theme(plot.caption = element_text(size = 7))
        } else {
            p + scale_fill_discrete(name = c_title) +
                scale_colour_discrete(name = c_title) +
                theme(plot.caption = element_text(size = 7))
        }
        
    })
    
    
    output$plot <- renderPlot({     
        print(plotInput())    
    })
    
    output$downloadPlot <- downloadHandler(
        filename = function() { paste0('SWIID', '.pdf') },
        content = function(file) {
            pdf(file, width = 7, height = 4)
            print(plotInput())
            dev.off()
        })

    
    
    observe({
        updateSelectInput(session, "country", choices = cc$country, selected = "United States")
    }) # reactive program for the 1st country choice.
    
    observe({
        cc1 = as.character(cc[cc==input$country, "ch"])
        updateSelectInput(session, "series", choices = get(cc1), selected="gini_net" )  
    }) # first country's data type 
    
    customData <- reactive({
        tvalue <- abs(qt((1 - 95/100)/2, 1000))
        se <- paste0(input$series, "_se")
        temp_swiid <- swiid %>% 
            filter(country == input$country, year == as.numeric(input$year)) %>% 
            select(one_of("country", "year", input$series, se)) 
        if (nrow(temp_swiid) != 0){
            temp_swiid$CI <- c(temp_swiid[, input$series] - tvalue * temp_swiid[, se], temp_swiid[, input$series] + tvalue * temp_swiid[, se]) %>% unlist %>% round(digits = 3) %>% paste(collapse = ", ")
            select(temp_swiid, one_of("country", "year","CI"))
        }else{
            paste0("Data for ", input$country, " in ", input$year, " is still in collection. Check it later.")
        }
    })
    
    
    output$countryYear <- renderTable({customData()})
    
})
