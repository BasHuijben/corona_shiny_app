#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    sidebarLayout(
        
        sidebarPanel(
            titlePanel("Visualize Covid-19"),
            checkboxGroupInput('continents', 'Continents', choices = ''),
            
            fluidRow(column(8,
                            selectInput('countries', 'Countries', choices = '', multiple = TRUE)),
                     column(4,
                            actionButton('all_selection', 'Select all', width = '100%'),
                            actionButton('all_remove', 'Remove all', width = '100%'))),
            
            sliderInput('date_slider', '
                        Select start and end period', 
                        min = as.Date('01-01-2020', '%d-%m-%Y'), 
                        max = as.Date('31-12-2020', '%d-%m-%Y'), 
                        value = c(as.Date('01-01-2020', '%d-%m-%Y'), as.Date('31-12-2020', '%d-%m-%Y')),
                        timeFormat = '%F')
            
        ),
        
        mainPanel(
            tabsetPanel(type = 'tabs',
                        
                        tabPanel('Line plot', 
                                 radioButtons('variable_plot', '
                                                Variables', 
                                                choiceNames = c('Reported cases',
                                                                'Reported deaths',
                                                                'Total cases',
                                                                'Total deaths'),
                                                choiceValues = c('cases',
                                                                 'deaths',
                                                                 'total_cases',
                                                                 'total_deaths'),
                                                selected = 'cases',
                                                inline = TRUE), 
                                 radioButtons('relative',
                                              'Relative', 
                                              choiceNames= c('Yes', 'No'),
                                              choiceValues = c('relative_', ''),
                                              selected = 'relative_',
                                              inline = TRUE),
                                 textOutput('warning_tab_line_plot'),
                                 plotOutput("plot_var")),
                        
                        tabPanel('Geo plot',
                             textOutput('warning_tab_geo_plot'),
                             leafletOutput('plot_geo', height = 800))
            )
        )
    )
))
