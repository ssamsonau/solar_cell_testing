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
      
  
      #find open circuit voltage
      write.serialConnection(con, paste0(":SOUR:DEL ", 0.5)); 
      write.serialConnection(con, ":SOUR:FUNC CURR "); 
      write.serialConnection(con, ":SOUR:CURR 0 ");
      write.serialConnection(con, ":SENS:FUNC 'VOLT:DC' ");  
      write.serialConnection(con, paste0(":SENS:VOLT:PROT ", input$max_volt));  # max current of Keithley 2401 is ~ 1.005 A
      write.serialConnection(con, ":TRIG:COUN 1");
      write.serialConnection(con, ":OUTP ON "); Sys.sleep(1)
      write.serialConnection(con, ":Read?"); Sys.sleep(4)
      Voc <- read.serialConnection(con); Sys.sleep(1)
      Voc <- str_split(Voc, ",", simplify = T)[1] %>% as.numeric()
      
      if(Voc >= 0){
        showModal(modalDialog(
          title = "Important message",
          "Switch wires connecting to positive and negative poles of a cell. 
          Negative values of voltage are expected", easyClose = F
        ))
        Sys.sleep(10)
        return(NULL)
      }
      values$Voc <- Voc
      
      
      #find short circuit current
      write.serialConnection(con, paste0(":SOUR:DEL ", 0.5)); 
      write.serialConnection(con, ":SOUR:FUNC VOLT "); 
      write.serialConnection(con, ":SOUR:VOLT 0 ");
      write.serialConnection(con, ":SENS:FUNC 'CURR:DC' ");  
      write.serialConnection(con, paste0(":SENS:CURR:PROT ", input$max_curr));  # max current of Keithley 2401 is ~ 1.005 A
      write.serialConnection(con, ":OUTP ON "); Sys.sleep(1) 
      write.serialConnection(con, ":Read?"); Sys.sleep(4)
      #browser()
      Isc <- read.serialConnection(con); Sys.sleep(1)
      Isc <- str_split(Isc, ",", simplify = T)[2] %>% as.numeric()
      values$Isc <- Isc
      
      
      
      
      #browser()
      ## do the sweep
      write.serialConnection(con, "*RST ");  
      write.serialConnection(con, ":SENS:FUNC:CONC OFF ");
      write.serialConnection(con, ":SOUR:FUNC CURR ");  
      write.serialConnection(con, ":SENS:FUNC 'VOLT:DC' ");  
      write.serialConnection(con, paste0(":SENS:VOLT:PROT ", input$max_volt));  
      
      write.serialConnection(con, paste0(":SOUR:CURR:START ", 0));  
      write.serialConnection(con, paste0(":SOUR:CURR:STOP ", Isc)); 
      write.serialConnection(con, paste0(":SOUR:CURR:STEP ", Isc/input$dots)); 
      write.serialConnection(con, ":SOUR:CURR:MODE SWEep ");  
      write.serialConnection(con, ":SOUR:SWE:RANG AUTO ");  
      write.serialConnection(con, ":SOUR:SWE:SPAC LIN ");   #Select linear staircase sweep.
      #write.serialConnection(con, ":SOURce:SWEep:SPACing LOG");   #Select linear staircase sweep.
      #write.serialConnection(con, paste0(":SOURce:SWEep:POINts ", input$dots ));   #Select linear staircase sweep.

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
    
    #take only values with negative voltage

    values$df <- values$df %>%
      filter(volt <= 0) %>%
      mutate(power = abs(curr) * abs(volt),
             is_max = power == max(power))
    
    values$df <- bind_rows(values$df, 
                           tibble(
                             curr = c(0, Isc),
                             volt = c(Voc, 0),
                             is_max = c(F, F)
                           ))
    
    #values$Isc <- max(values$df$curr)
    #values$Voc <- max(values$df$volt)
   
      
    
  })
  
  output$param <- renderText({
    
    max_p <- values$df %>% filter(is_max == T) %>% select(power) %>% unlist
    Isc <- values$Isc
    Voc <- values$Voc
    
    paste(" Isc = ", Isc, "A \n",
          "Voc = ", Voc, "V \n",
          "Maximum Power = ", max_p, "W \n", 
          "Fill Factor = ", max_p/(Isc * abs(Voc)), "\n",
          "Efficiency = ", max_p/(input$input_power_density * input$area /100) *100, "%")
  })
  
  output$finalPlot <- renderPlot({
    
    I_maxPower <- values$df %>% filter(is_max == T) %>% select(curr) %>% unlist
    V_maxPower <- values$df %>% filter(is_max == T) %>% select(volt) %>% unlist
    
    #browser()
    # generate bins based on input$bins from ui.R
    ggplot(values$df, aes(x = curr, y = volt, shape = is_max)) +
      geom_rect(mapping=aes(xmin=0, xmax=values$Isc, ymin=0, ymax=values$Voc, fill="Isc Voc"), alpha = 0.05) +
      geom_rect(mapping=aes(xmin=0, xmax=I_maxPower, ymin=0, ymax=V_maxPower, fill="maxPower"), alpha = 0.10) +
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
  
  output$table <- DT::renderDataTable({
    values$df
  }, extensions = 'Buttons', options = list(
    dom = 'Bfrtip',
    buttons = c('csv', 'excel')
  ))
  
})
