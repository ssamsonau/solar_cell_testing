
observeEvent(input$meassure, {
  if (is.null(values$con)) {
    showModal(
      modalDialog(
        title = "Important message",
        "Establish connection first",
        easyClose = T
      )
    )
    return(NULL)
  }
  
  if (!isOpen(values$con)) {
    showModal(
      modalDialog(
        title = "Important message",
        "Establish connection first",
        easyClose = T
      )
    )
    return(NULL)
  }
  
  con <- values$con
  
  withProgress(message = "Doing measurements", {
    #write.serialConnection(con, "*IDN?"); Sys.sleep(1)
    #read.serialConnection(con); Sys.sleep(1)
    
    
    write.serialConnection(con, "*RST ")
    
    #write.serialConnection(con, ":TRIG:CLEar ");
    write.serialConnection(con, ":SENS:FUNC:CONC OFF ")
    
    #browser()
    
    
    #  #find open circuit voltage
    #  write.serialConnection(con, paste0(":SOUR:DEL ", 0.5));
    #  write.serialConnection(con, ":SOUR:FUNC CURR ");
    #  write.serialConnection(con, ":SOUR:CURR 0 ");
    #  write.serialConnection(con, ":SENS:FUNC 'VOLT:DC' ");
    #  write.serialConnection(con, ":SENSe:VOLTage:RANGe DEFault");
    #  write.serialConnection(con, paste0(":SENS:VOLT:PROT ", input$max_volt));   #max current of Keithley 2401 is ~ 1.005 A
    #  write.serialConnection(con, ":TRIG:COUN 1");
    #  write.serialConnection(con, ":OUTP ON "); Sys.sleep(1)
    #  write.serialConnection(con, ":Read?"); Sys.sleep(4)
    #  Voc <- read.serialConnection(con); Sys.sleep(1)
    #  Voc <- str_split(Voc, ",", simplify = T)[1] %>% as.numeric()
    # #
    #  if(Voc >= 0){
    #    showModal(modalDialog(
    #      title = "Important message",
    #      "Switch wires connecting to positive and negative poles of a cell.
    #      Negative values of voltage are expected", easyClose = F
    #    ))
    #    Sys.sleep(10)
    #    return(NULL)
    #  }
    #
    #values$Voc <- Voc
    
    
    #find short circuit current
    write.serialConnection(con, paste0(":SOUR:DEL ", 0.5))
    
    write.serialConnection(con, ":SOUR:FUNC VOLT ")
    
    write.serialConnection(con, ":SOUR:VOLT 0 ")
    
    write.serialConnection(con, ":SENS:FUNC 'CURR:DC' ")
    
    write.serialConnection(con, paste0(":SENS:CURR:PROT ", input$max_curr))
    # max current of Keithley 2401 is ~ 1.005 A
    write.serialConnection(con, ":OUTP ON ")
    Sys.sleep(1)
    write.serialConnection(con, ":Read?")
    Sys.sleep(4)
    #browser()
    Isc <- read.serialConnection(con)
    Sys.sleep(1)
    Isc <- str_split(Isc, ",", simplify = T)[2] %>% as.numeric()
    #values$Isc <- Isc
    
    if (abs(Isc) < 1e-8) {
      showModal(
        modalDialog(
          title = "Important message",
          "Detected value of current of short circuit current is too small(< 1e-8 A) . Exiting",
          easyClose = T
        )
      )
      values$Isc <- values$Voc <- values$df <- NULL
      write.serialConnection(con, ":OUTP OFF ")
      Sys.sleep(10)
      return(NULL)
    }
    
    
    
    if (Isc < 0) {
      showModal(
        modalDialog(
          title = "Important message",
          "Switch wires connecting to positive and negative poles of a cell.
          Positive value of short circuit current are expected",
          easyClose = T
        )
      )
      values$Isc <- values$Voc <- values$df <- NULL
      write.serialConnection(con, ":OUTP OFF ")
      Sys.sleep(10)
      return(NULL)
    }
    
    
    
    
    #browser()
    ## do the sweep
    write.serialConnection(con, "*RST ")
    
    write.serialConnection(con, ":SENS:FUNC:CONC OFF ")
    
    write.serialConnection(con, ":SOUR:FUNC CURR ")
    
    write.serialConnection(con, ":SENS:FUNC 'VOLT:DC' ")
    
    write.serialConnection(con, paste0(":SENS:VOLT:PROT ", input$max_volt))
    
    
    write.serialConnection(con, ":SENSe:VOLTage:RANGe DEFault")
    
    
    write.serialConnection(con, ":SOUR:CURR:MODE SWEep ")
    
    write.serialConnection(con, ":SOURce:SWEep:RANGing AUTO")
    
    write.serialConnection(con, paste0(":SOUR:CURR:START ", 0))
    
    write.serialConnection(con, paste0(":SOUR:CURR:STOP ", Isc))
    
    write.serialConnection(con, paste0(":SOUR:CURR:STEP ", Isc / input$dots))
    
    
    #browser()
    
    write.serialConnection(con, paste0(":TRIG:COUN ",  input$dots))
    
    write.serialConnection(con, ":SOUR:SWE:SPAC LIN ")
    #Select linear staircase sweep.
    
    
    
    #write.serialConnection(con, ":SOURce:SWEep:SPACing LOG");   #Select linear staircase sweep.
    #write.serialConnection(con, paste0(":SOURce:SWEep:POINts ", input$dots ));   #Select linear staircase sweep.
    
    write.serialConnection(con, paste0(":SOUR:DEL ", input$time))
    
    write.serialConnection(con, ":OUTP ON ")
    Sys.sleep(2)
    
    write.serialConnection(con, ":Read?")
    
    
    Sys.sleep(5 + input$time * 2 * input$dots)
    
    
    
    a <- read.serialConnection(con)
    Sys.sleep(1)
    
    write.serialConnection(con, ":OUTP OFF ")
    
    
    b <- str_split(a, ",", simplify = T) %>%
      as.numeric()
    
    curr  <- b[seq(2, length(b), by = 5)]
    volt  <- b[seq(1, length(b), by = 5)]
    
    #browser()
    values$df <- tibble(curr = curr,
                        volt = volt)
    
    
    ################### again for the last 10%
    write.serialConnection(con, paste0(":SOUR:CURR:START ", 0.92 * Isc))
    
    write.serialConnection(con, paste0(":SOUR:CURR:STOP ", 1.02 * Isc))
    
    write.serialConnection(con, paste0(":SOUR:CURR:STEP ", 0.1 * Isc /
                                         input$dots))
    
    write.serialConnection(con, paste0(":TRIG:COUN ",  input$dots))
    
    write.serialConnection(con, paste0(":SOUR:DEL ", input$time))
    
    write.serialConnection(con, ":OUTP ON ")
    Sys.sleep(2)
    write.serialConnection(con, ":Read?")
    
    
    Sys.sleep(5 + input$time * 2 * input$dots)
    
    
    a <- read.serialConnection(con)
    Sys.sleep(1)
    
    write.serialConnection(con, ":OUTP OFF ")
    
    
    b <- str_split(a, ",", simplify = T) %>%
      as.numeric()
    #browser()
    curr  <- b[seq(2, length(b), by = 5)]
    volt  <- b[seq(1, length(b), by = 5)]
    
    values$df <- values$df %>%
      bind_rows(tibble(curr = curr,
                       volt = volt))
    
    ##################################################
    
    
  })
  
  if(is.null(values$df))
    return(NULL)
  
  #take only values with negative voltage
  
  values$df <- values$df %>%
    filter(volt <= 0) %>%
    mutate(power = abs(curr) * abs(volt),
           is_max = power == max(power))
  
  # values$df <- bind_rows(values$df,
  #                        tibble(
  #                          curr = c(0, Isc),
  #                          volt = c(Voc, 0),
  #                          is_max = c(F, F)
  #                        ))
  
  
  
  values$Isc <- max(values$df$curr)
  values$Voc <- min(values$df$volt)
  
  values$df <- bind_rows(values$df,
                         tibble(
                           curr = c(values$Isc),
                           volt = c(0),
                           is_max = c(F)
                         ))
  
  
})