library(shiny)
library(ggplot2)
library(ggiraph)
library(cowplot)
library(DT)
library(data.table)

source('helpers.R')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$joint_graphs <- renderggiraph({
        GetJointPlot(as.Date(input$select_date,"%Y-%m-%d"), as.Date(input$range_dates[1],"%Y-%m-%d"), 
                     as.Date(input$range_dates[2],"%Y-%m-%d"), input$check_continent)
    })
    
    output$p1_graph <- renderggiraph({
        GetDatePlot(as.Date(input$select_date,"%Y-%m-%d"))
    }) 
    
    output$p2_graph <- renderggiraph({
        p2_g
    })
    
    output$p3_graph <- renderPlot({
        p
    })
    
    
    #DATA
    output$my_ticker <- renderUI({
        selectInput('ticker', label = 'Selected countries', choices = setNames(big_mac_data$name, big_mac_data$name), multiple = TRUE, selected = setNames(big_mac_data$name, big_mac_data$name),)
    })

    my_reactive_df <- reactive({
        df<- get_data_by_ticker_and_date(input$ticker, input$my_date[1], input$my_date[2])
        return(df)
    })
    output$my_data <- DT::renderDataTable({
        my_reactive_df()
    })
    

})
