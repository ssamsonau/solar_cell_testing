values$is_open <- NULL

output$is_open_connection <- renderText({
  if(is.null(values$is_open))
    return(NULL)
  
  ifelse(values$is_open, "Port is open", "Port is not open")
})

observeEvent(input$connect, {
  values$con <- serialConnection(
    name = "con1",
    port = input$com,
    translation = "cr",
    mode = "9600,n,8,1",
    newline = 1
  )
  
  print("opening connection")
  
  #browser()
  
  if (isOpen(values$con)) {
    showModal(modalDialog(
      title = "Message",
      "Port is already open.",
      easyClose = T
    ))
    return()
  }
  
  
  
  tryCatch({
    open(values$con)
    values$is_open <- T
    showModal(modalDialog(title = "Message",
                          "Port opened",
                          easyClose = T))
  },
  error = function(e) {
    showModal(modalDialog(
      title = "Message",
      paste0("Somethign went wrong. Port is not open. Error:" , e),
      easyClose = T
    ))
  })
  
  
  Sys.sleep(1)
  
  
})

observeEvent(input$disconnect, {
  print("closing connection")
  close(values$con)
  values$is_open <- F
  
})