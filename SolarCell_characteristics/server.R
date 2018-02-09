#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(serial)
library(stringr)
library(tidyverse)
library(DT)



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  values <- reactiveValues()
  values$Isc <- values$Voc <- values$df <- NULL
  
  source("connection.R", local = T)
  source("meassure.R", local = T)

  
  output$param <- renderText({
    if(is.null(values$df))
      return(NULL)
    
    max_p <-
      values$df %>% filter(is_max == T) %>% select(power) %>% unlist
    Isc <- values$Isc
    Voc <- values$Voc
    
    paste(
      " Isc = ",
      Isc,
      "A \n",
      "Voc = ",
      Voc,
      "V \n",
      "Maximum Power = ",
      max_p,
      "W \n",
      "Fill Factor = ",
      max_p / (Isc * abs(Voc)),
      "\n",
      "Efficiency = ",
      max_p / (input$input_power_density * input$area / 1000) * 100,
      "%"
    ) # we use mW in input, so dividing by 1000
  })
  
  output$finalPlot <- renderPlot({
    if(is.null(values$df))
      return(NULL)
    
    I_maxPower <-
      values$df %>% filter(is_max == T) %>% select(curr) %>% unlist %>% as.numeric
    V_maxPower <-
      values$df %>% filter(is_max == T) %>% select(volt) %>% unlist %>% as.numeric
    
    #browser()
    # generate bins based on input$bins from ui.R
    ggplot(values$df, aes(x = curr, y = volt, shape = is_max)) +
      geom_rect(
        mapping = aes(
          xmin = 0,
          xmax = values$Isc,
          ymin = values$Voc,
          ymax = 0,
          fill = "Isc Voc"
        ),
        alpha = 0.05
      ) +
      geom_rect(
        mapping = aes(
          xmin = 0,
          xmax = I_maxPower,
          ymin = V_maxPower,
          ymax = 0,
          fill = "maxPower"
        ),
        alpha = 0.10
      ) +
      geom_point() +
      xlab("Current, A") +
      ylab("Voltage, V")
    
    
    
  })

  
  output$table <- DT::renderDataTable({
    if(is.null(values$df))
      return(NULL)
    
    values$df
  }, 
  server = F, # load full data set in shiny and download full data
  extensions = 'Buttons', 
  options = list(dom = 'Bfrtip',
                                            buttons = c('csv', 'excel')))
  
})
