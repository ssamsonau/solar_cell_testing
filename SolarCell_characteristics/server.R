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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  values <- reactiveValues() 
  
  observeEvent(input$meassure, {
    if(is.null(values$con)){
      showModal(modalDialog(
        title = "Important message",
        "Establish connection first", easyClose = T
      ))
      return(NULL)
    }
    
    if(!isOpen(values$con)){
      showModal(modalDialog(
        title = "Important message",
        "Establish connection first", easyClose = T
      ))
      return(NULL)
    }
    
    con <- values$con
    
    withProgress(message = "Doing measurements", {
      
      #write.serialConnection(con, "*IDN?"); Sys.sleep(1)
      #read.serialConnection(con); Sys.sleep(1)
      
      
      write.serialConnection(con, "*RST ");  
      #write.serialConnection(con, ":TRIG:CLEar ");  
      write.serialConnection(con, ":SENS:FUNC:CONC OFF ");
      #browser()
      
      #find open circuit current
      write.serialConnection(con, ":SOUR:FUNC VOLT "); 
      write.serialConnection(con, ":SOUR:VOLT 0 ");
      write.serialConnection(con, ":SENS:FUNC 'CURR:DC' ");  
      write.serialConnection(con, paste0(":SENS:CURR:PROT ", input$max_curr));  # max current of Keithley 2401 is ~ 1.005 A
      write.serialConnection(con, ":TRIG:COUN 1");
      write.serialConnection(con, ":OUTP ON "); Sys.sleep(2)
      write.serialConnection(con, ":Read?"); Sys.sleep(1)
      Ioc <- read.serialConnection(con); Sys.sleep(1)
      Ioc <- str_split(Ioc, ",", simplify = T)[2] %>% as.numeric()
      
      ## do the sweep
      write.serialConnection(con, "*RST ");  
      write.serialConnection(con, ":SENS:FUNC:CONC OFF ");
      write.serialConnection(con, ":SOUR:FUNC CURR ");  
      write.serialConnection(con, ":SENS:FUNC 'VOLT:DC' ");  
      write.serialConnection(con, paste0(":SENS:VOLT:PROT ", input$max_volt));  
      write.serialConnection(con, paste0(":SOUR:CURR:START ", 0));  
      write.serialConnection(con, paste0(":SOUR:CURR:STOP ", Ioc)); 
      write.serialConnection(con, paste0(":SOUR:CURR:STEP ", Ioc/input$dots)); 
      write.serialConnection(con, ":SOUR:CURR:MODE SWE ");  
      write.serialConnection(con, ":SOUR:SWE:RANG AUTO ");  
      
      write.serialConnection(con, ":SOUR:SWE:SPAC LIN ");  
      write.serialConnection(con, paste0(":TRIG:COUN ",  input$dots));  
      write.serialConnection(con, paste0(":SOUR:DEL ", input$time));  
      write.serialConnection(con, ":OUTP ON "); Sys.sleep(2)
      
      write.serialConnection(con, ":Read?"); 
      
      Sys.sleep(5+input$time*2*input$dots)
    })
    
    a <- read.serialConnection(con); Sys.sleep(1)
    
    write.serialConnection(con, ":OUTP OFF "); 
    
    b <- str_split(a, ",", simplify = T) %>%
      as.numeric()
    
    curr  <- b[seq(2, length(b), by = 5)]
    volt  <- b[seq(1, length(b), by = 5)]
    
    #browser()
    values$df <- tibble(
      curr = curr, 
      volt = volt
    )
    
  })
  
  output$distPlot <- renderPlot({
    
    #browser()
    # generate bins based on input$bins from ui.R
    ggplot(values$df, aes(x = curr, y = volt)) +
      geom_point() +
      xlab("Current, A") +
      ylab("Voltage, V")
    
    
  })
  
  observeEvent(input$connect, {
    values$con <- serialConnection(name = "con1",
                            port = input$com,
                            translation = "cr",
                            mode = "9600,n,8,1",
                            newline = 1)
    
    print("opening connection")
    open(values$con); Sys.sleep(1)
    
    
  })
  
  observeEvent(input$disconnect, {
    print("closing connection")
    close(values$con)
  })
  
})
