library(shiny)
library(ggiraph)
library(shinythemes)
library(jsonlite)
library(data.table)
library(DT)


source('helpers.R')

ui <- navbarPage(title = 'The Big Mac Index', theme = shinytheme('flatly'),
                          tabPanel("Plots", 
                                  sidebarPanel(width = 4,
                                    selectInput("select_date", label = h3("Select Date"), 
                                                choices = setNames(big_mac_data$date, big_mac_data$date), 
                                                selected = as.Date("2020-07-01","%Y-%m-%d"),
                                    ),
                                    checkboxGroupInput("check_continent", label = h3("Checkbox country"), 
                                                       choices = c('South America', 'Oceania', 'North', 'America', 'Europe', 'Asia', 'Africa'),
                                                       selected = c('South America', 'Oceania', 'North', 'America', 'Europe', 'Asia', 'Africa')
                                    ),
                                    dateRangeInput("range_dates", label = h3("Date range"), format = 'yyyy', end = '2020-07-01', start = '2000-01-01')
                                  ),
                                  mainPanel(
                                    girafeOutput("joint_graphs", width = "100%", height = "700px")
                                  )
                          ),
                 
                         tabPanel("Data",
                                  sidebarPanel(
                                    width = 4,
                                    uiOutput('my_ticker'),
                                    dateRangeInput('my_date',label = 'Date', format = 'yyyy', end = '2020-07-01', start = '2000-01-01')
                                  ),
                                  mainPanel(
                                    dataTableOutput('my_data',height = "auto")
                                  )
                         )
)
