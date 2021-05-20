library(ggplot2)
library(ggiraph)
library(cowplot)
library(lubridate)
library(data.table)


# GET DATA ----------------------------------------------------------------
# 
# big_mac_data <- readRDS('/app/data/big_mac_data.rds')
# data_balanced <- readRDS('/app/data/data_balanced.rds')

big_mac_data <- readRDS('data/big_mac_data.rds')
data_balanced <- readRDS('data/data_balanced.rds')

print(big_mac_data)

#GetDataByDate <- function(start_date, end_date){
#}

# PLOTS + PLOT GETTERS-----------------------------------------------------

GetDatePlot <- function(date_input){
  p1 <- ggplot(big_mac_data[date == date_input], aes(
    x = reorder(name, usd_raw),
    y = usd_raw, 
    tooltip = name, # tooltip for interactivity
    data_id = name)) + # data_id to match with other plots
    geom_point_interactive(aes(color = positive))+ # an interactive scatter plot working with interactive tooltip
    ggtitle(paste0("Currency value rating in compared to USD at: ", date_input)) + #title
    ylab('') + xlab('Country') + # labels
    scale_y_continuous(labels = scales::percent) + # using percentage labels on y
    geom_hline(yintercept = 0, color = "black", size = 0.5) + # showing the US price benchmark line
    theme_bw() +  # setting theme
    theme(panel.border = element_blank(), # fine tuning theme
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_blank(),
          legend.position = "none",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),# rotate labels with 90deg
          plot.title = element_text(hjust = 0.5)) +
    annotate("text", x = 3, y = -0.12, label = "Undervalued", color = 'red') +
    annotate("text", x = 3, y = 0.12, label = "Overvalued", color = '#5ab4ac')
  
  p1_g <- girafe(ggobj = p1,
                 options = list(
                   opts_hover_inv(css = "opacity:0.1;"), # to make the not selected data transparent
                   opts_hover(css = "stroke-width:5;stroke:#2c7fb8;"), # to make the currently selected data visible
                   opts_tooltip( # setting the tooltip position into a fixed position
                     offx = 120,
                     offy = 400, 
                     use_cursor_pos = FALSE,
                     opacity = 0.9)
                 )
  )
  return(p1_g)
}

GetJointPlot <- function(date_input, date_start, date_end, continents){
  p1 <- ggplot(big_mac_data[date == date_input & Continent_Name %in% continents], aes(
    x = reorder(name, usd_raw),
    y = usd_raw, 
    tooltip = name, # tooltip for interactivity
    data_id = name)) + # data_id to match with other plots
    geom_point_interactive(aes(color = positive))+ # an interactive scatter plot working with interactive tooltip
    ggtitle(paste0("Currency value rating in compared to USD at: ", date_input)) + #title
    ylab('') + xlab('Country') + # labels
    scale_y_continuous(labels = scales::percent) + # using percentage labels on y
    geom_hline(yintercept = 0, color = "black", size = 0.5) + # showing the US price benchmark line
    theme_bw() +  # setting theme
    theme(panel.border = element_blank(), # fine tuning theme
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_blank(),
          legend.position = "none",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),# rotate labels with 90deg
          plot.title = element_text(hjust = 0.5)) +
    annotate("text", x = 3, y = -0.12, label = "Undervalued", color = 'red') +
    annotate("text", x = 3, y = 0.12, label = "Overvalued", color = '#5ab4ac')
  
 p2 <- ggplot(big_mac_data[date >= date_start & date <= date_end & Continent_Name %in% continents], aes(
   x = date,
   y = usd_raw,
   tooltip = name, # tooltip for interactivity
   data_id = name)) +  # data_id to match with other plots
   geom_line_interactive(alpha = 1.0, color = "#f0f0f0") + # an interactive line plot
   geom_point(aes(color = positive), alpha = 0.2) + # adding transparent points for pretty look
   ggtitle("Currency value ratings compared to USD") + #title
   ylab('') + xlab('Year') + # labels
   scale_y_continuous(labels = scales::percent) + # using percentage labels on y
   geom_hline(yintercept = 0, color = "black", size = 0.5) + # showing the US price benchmark line
   theme_bw() + # setting theme
   theme(panel.border = element_blank(), # fine tuning theme
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.line = element_blank(),
         legend.position = "none",
         plot.title = element_text(hjust = 0.5))
 
 joint_plot <- girafe(ggobj =  plot_grid(p1,p2, ncol=1, nrow=2), # using girage to show the two plots above as interactive joint plots
                      options = list(
                        opts_hover_inv(css = "opacity:0.1;"), # to make the not selected data transparent
                        opts_hover(css = "stroke-width:5;stroke:#2c7fb8;"), # to make the currently selected data visible
                        opts_tooltip( # setting the tooltip position into a fixed position
                          opacity = 0.9)
                      )
 )
 return(joint_plot)
}

get_data_by_ticker_and_date  <- function(ticker, start_date, end_date) {

  data <- big_mac_data[date >= start_date & date <= end_date & name %in% ticker]
  return(data)
}

get_best_country <- function(date_input, date_start, date_end, continents){
  data <- big_mac_data[date >= start_date & date <= end_date & name %in% ticker]
  country_name <- data[, .SD[which.max(usd_raw)],]$name
  return(paste0('The best country is: ', country_name))
}

