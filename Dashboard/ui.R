#ui_nn

library(shiny)
# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("National Similarity by WBI Metrics"),
  
  sidebarPanel(
    selectInput("Variable", "Variable:",
                choices = clist),
    
    sliderInput("Year",
                "Year:",
                min = 1991,
                max = 2016,
                value= 1991,
                step= 1,
                sep = "")
    
    
  ),
  
  mainPanel(
    h3(textOutput("caption")),
    
    leafletOutput("my_map")
    

  )
  
))
