#server_nn



library(dplyr) 
library(tmap) # for mapping
library(fields) # a faster way to compute dist matrixes, output is matrix, not 'dist'
library(reshape) # necessary to transform distance matrix into long format
library(sf) # needed to code dataframes into 'sf' format (st_as_st)
library(shiny) # 
library(leaflet)

setwd("C:\\Users\\rsolt008\\Documents\\personal git\\WorldBank Shiny App\\Dashboard/") 
load("C:\\Users\\rsolt008\\Documents\\personal git\\WorldBank Shiny App\\NN_dataset.rdata")
#load("C:\\Users\\rsolt008\\Documents\\personal git\\WorldBank Shiny App\\worldBank_raw_wide.rdata")
load("C:\\Users\\rsolt008\\Documents\\personal git\\WorldBank Shiny App\\dat_wide.rdata")


bins <- c(0, 1, 2, 3, 4, 5, Inf)
pal <- colorBin("YlOrRd", domain = nn_dist_viz$Distance, bins = bins)


nn_dist_viz <- dplyr::left_join(nn_dist_viz,dat_wide, 
                                by=c("Country_Comp"="country","Year"="year"))

# nn_dist_viz is 88 mb large

clist<-unique(nn_dist_viz$Country_Focus)

shinyServer(function(input, output, session) {
  
  
  # Reactive expression for the given country in the Title
  formulaText <- reactive({
    paste("Similarity Mapping for", input$Variable)
  })
  
  # Return the formula text for printing as a caption
  output$caption <- renderText({
    formulaText()
  })
  
  #############
  
  # Reactive expression for the given country dataset
  nn_dist_sub <- reactive({
    tmp_dist <- nn_dist_viz[ which ( nn_dist_viz$Country_Focus==input$Variable & nn_dist_viz$Year==input$Year ) , ]
    tmp_dist 
    
    })
  
  
  nn_focus_country <- reactive({
    tmp_foc <- nn_dist_viz[ which (nn_dist_viz$Country_Comp==input$Variable & nn_dist_viz$Year==input$Year), ] 
    tmp_foc
    
  })
  

  # Generate a plot from the subset above for a country distance map
  
  # Generate a plot of the requested variable against mpg and only 
  # include outliers if requested
  #output$dist_plot <- renderPlot({
  #  tm_shape(nn_dist_sub()) + tm_borders(col = "#d9d9d9") +
  #    tm_fill("Distance",alpha = 0.8,style = "cont", palette = c("#253494","#2c7fb8","#41b6c4","#7fcdbb","#c7e9b4")) +
  #    tm_layout(outer.margins = F, frame = F) #+
      #tm_credits(paste("Distance Map for",nn_dist_sub()$Country_Focus[1],"in",nn_dist_sub()$Year[1],sep=" "), 
      #           position=c("left","bottom"))
    
  #})
  
  output$my_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(data=nn_dist_sub(),  
                  fillColor = ~pal(Distance), 
                  stroke = FALSE,
                  popup=paste("<b>","Country","</b>", nn_dist_sub()$Country_Comp, "<br>",
                              "GDP PPP:", nn_dist_sub()$GDP_pC_PPP, "<br>",
                              "Birth Rate:", nn_dist_sub()$birth_rate_per_K, "<br>",
                              "Electricity:", nn_dist_sub()$access_to_electricity_perc, "<br>",
                              "Unemployed:", nn_dist_sub()$Unemployment_perc),
                  popupOptions = popupOptions(maxWidth ="100%", 
                                              closeOnClick = TRUE)) %>%
      addPolygons(data=nn_focus_country(), 
                  fillColor = "#a6bddb",  
                  weight = 1,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = .1) %>%
      addLegend(pal=pal, values=nn_dist_sub()$Distnace, opacity = 0.7, title = "Similarity",
                position = "bottomright") 
  })
  
  
  
  
  
  
})