#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Solar Cell Efficiency. Using Keithley 2401"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textInput("com", "COM port", value = "COM3"),
      textOutput("is_open_connection"), 
      actionButton("connect", "Open connection"),
      actionButton("disconnect", "Close connection"),
      h4("-----"),
      h3("Note: Program  expects negative voltage values"),
      h4("-----"),
      numericInput("time",
                   "Time for each point measurement (in sec)",
                   min = 0.01,
                   max = 5,
                   value = 0.1),
      numericInput("max_curr",
                   "Maximum allowed current (in A) (Keithley 2401 supports up to 1A)",
                   min = 0,
                   max = 1,
                   value = 1),
      numericInput("max_volt",
                   "Maximum allowed Voltage (in V) (Keithley 2401 supports up to 21V)",
                   min = 0,
                   max = 21,
                   value = 5),
      
      h4("-----------------------"),
      
      numericInput("dots",
                   "Number of Dots:",
                   min = 10,
                   max = 50,
                   value = 50),
      numericInput("input_power_density",
                   "Input Light Power (in mW/cm2). It is ~ 100mW/cm2 for sunlight at the ground level:",
                   value = 100),
      numericInput("area",
                   "Area of a solal cell (cm2):",
                   value = 1),
      
      
      actionButton("meassure", "Do measurements")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
       plotOutput("finalPlot"),
       verbatimTextOutput("param"),
       DT::dataTableOutput("table")
    )
  )
))
