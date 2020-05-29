#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# import libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(countrycode)

source("functions/world_map.R")
source("functions/corona_data.R")

# get world map
world_map <- create_world_map()

# downlaod corona data
download.corona.data()

# pre-process corona dataset and return dataset
per_pop <- 100000 # used for normalizing deaths and cases per country
corona <- get.corona.data(world_map = world_map, norm.size = per_pop)

# get all continents and countries in dataset
continents <- levels(as.factor(corona$continentExp))
countries <- levels(as.factor(corona$country))

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    # update continent checkboxes
    updateCheckboxGroupInput(session, 
                            "continents",
                            choices = continents,
                            selected = continents)
    
    # update country selection options based continents that have been selected
    observeEvent(input$continents, {
        # when no continents are selected use all continents otherwise use the selected continents
        if (is.null(input$continents)){
            continents.2.use <- continents
        }
        else {
            continents.2.use <- input$continents
        }
        
        # get countries in continents
        corona.2.use <- corona %>% filter(continentExp %in% continents.2.use)
        names <- unique(corona.2.use$country)
        
        # update country selection
        updateSelectInput(session,
                          "countries",
                          choices = names,
                          selected = 'Netherlands')
    })
    

    # Update country selection when button "all_selection" is pressed
    observeEvent(input$all_selection, {
        # when no continents are selected use all continents otherwise use the selected continents
        if (is.null(input$continents)){
            continents.2.use <- continents
        }
        else {
            continents.2.use <- input$continents
        }
        
        # get countries in continents
        corona.2.use <- corona %>% filter(continentExp %in% continents.2.use)
        names <- unique(corona.2.use$country)
        
        # update country selection
        updateSelectInput(session,
                          "countries",
                          choices = names,
                          selected = names)
    })
    
    # Update country selection when button "all_remove" is pressed
    observeEvent(input$all_remove, {
        # when no continents are selected use all continents otherwise use the selected continents
        if (is.null(input$continents)){
            continents.2.use <- continents
        }
        else {
            continents.2.use <- input$continents
        }
        
        # get countries in continents
        corona.2.use <- corona %>% filter(continentExp %in% continents.2.use)
        names <- unique(corona.2.use$country)
        
        # update country selection
        updateSelectInput(session,
                          "countries",
                          choices = names,
                          selected = '')
    })
    
    # update slider based on min and max date in corona dataset
    updateSliderInput(session,
                      'date_slider',
                      min = min(corona$dateRep),
                      max = max(corona$dateRep))
    
    # when no country has been selected, generate warning message in line plot tab
    output$warning_tab_line_plot <- renderText({
        if (!is.null(input$countries)){
            text <- ""
            }
        else {
            text <- "Please select a country to be plotted!"
        }
    })
    
    # when no country has been selected, generate warning message in geo plot tab
    output$warning_tab_geo_plot <- renderText({
        if (!is.null(input$countries)){
            text <- ""
        }
        else {
            text <- "Please select a country to be plotted!"
        }
    })
    
    # create line plot figure
    output$plot_var <- renderPlot({
        if (!is.null(input$countries)){
            corona %>%
                filter(country %in% input$countries &
                           dateRep >= input$date_slider[1] &
                           dateRep <= input$date_slider[2] ) %>%
                ggplot(aes_string(x='dateRep',
                           y=paste0(input$relative, input$variable_plot),
                           group='country',
                           color='country')) +
                geom_line() +
                ylim(0, NA) +
                xlab('Time')
        }
        })
    
    # creae geo plot 
    output$plot_geo <- renderLeaflet({ 
        if (!is.null(input$countries)){
            # prepare corona data
            data <- corona %>%
                filter(country %in% input$countries) %>%
                group_by(country) %>%
                summarise_at(vars(countryterritoryCode, total_cases, total_deaths, relative_total_cases, relative_total_deaths), funs(last)) %>%
                mutate(hover = paste0('<b>Country: ', country, '</b><br>',
                                      'Cases: ', total_cases, '<br>', 
                                      'Deaths: ', total_deaths, '<br>', 
                                      'Relative cases: ', round(relative_total_cases, 2), '<br>',
                                      'Relative deaths: ', round(relative_total_deaths, 2), '<br>')) %>%
                    arrange(countryterritoryCode)
            
            # get countries from world nap
            leaflet_data <- subset(world_map, iso3 %in% data$countryterritoryCode)
            data <- data.frame(iso3 = leaflet_data$iso3) %>% 
                left_join(data, by=c('iso3' = 'countryterritoryCode'))
            
            # set color bins
            bins <- c(0, 30, 60, 90, 120, 150, Inf)
            color.bins <- colorBin('Blues', domain=data$rel_sum_cases, bins=bins)
            
            # create plot
            leaflet(leaflet_data) %>% 
                 addTiles() %>% 
                 addPolygons(weight=1, 
                             label=lapply(data$hover, HTML), 
                             fillColor = ~color.bins(data$relative_total_cases),
                             fillOpacity = 0.8,
                             color='grey',
                             highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>%
                addLegend(position = "bottomleft", 
                          colors = color.bins(bins[1:length(bins)-1]), 
                          labels = bins[1:length(bins)-1],
                          opacity = 0.8,
                          title = "Number of cases per 100.000 inhabitants")
        }
        })

})
