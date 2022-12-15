server <- function(input, output, session) 
{
  router$server(input, output, session)
  
  font = list(
    color= "white"
  )
  
  observeEvent(input$mmbutton, {
    shinyjs::toggle(id="dropdownmenu")
  })
  
  #================================================================
  # Maps for each of the variables
  #================================================================
  output$poverty = renderLeaflet({
    povertydata = switch(input$age, 
                         "Under 5 years" = all_census$Tot_pov_U_5_18, 
                         "5 to 17 years" = all_census$Tot_pov_U_5_17_18,
                         "Under 18 years" = all_census$Tot_pov_U_18,
                         "Related children of householder under 18 years" = all_census$Tot_pov_RUC_18)
    
    legendTitle = switch(input$age, 
                         "Under 5 years" =  poverty_legendT[1], 
                         "5 to 17 years" = poverty_legendT[2],
                         "Under 18 years" = poverty_legendT[3],
                         "Related children of householder under 18 years" = poverty_legendT[4])
    
    # divides the color palette into 9 colors.
    pal = colorBin(palette="OrRd", bins = 5, domain= povertydata,  pretty = TRUE)
    
    # the labels on top of the map when hovered over
    labels = sprintf("<strong>%s<strong><br/>Total population: %g",
                     all_census$Geographic_Area_Name, povertydata) %>%
      lapply(htmltools::HTML)
    
    all_census %>% 
      st_transform("EPSG:4326") %>%
      leaflet() %>%
      addProviderTiles(provider="CartoDB.Positron") %>%
      addPolygons(label = labels, 
                  stroke = TRUE, 
                  color= "#d6d5de",
                  weight= 1,
                  smoothFactor = .5, 
                  opacity = 0.5,
                  fillOpacity = 0.7,
                  fillColor = ~pal(povertydata),
                  layerId = all_census$GEOID,
                  highlightOptions = highlightOptions(weight=2,
                                                      fillOpacity=1, 
                                                      color="black",
                                                      opacity=0.5,
                                                      bringToFront= TRUE)) %>%
      addLegend("bottomright", 
                pal=pal,
                values = ~povertydata,
                title = legendTitle,
                opacity = 0.7)
  })
  output$healthcoverage = renderLeaflet({
    hcdata = switch(input$hc_age, 
                    "Male under 6 years with health insurance coverage" = all_census$M_U_6__W_HIC, 
                    "Male under 6 years without health insurance coverage"= all_census$M_U_6__N_HIC,
                    "Male under 6 to 18 years with health insurance coverage" = all_census$M_16_18__W_HIC,
                    "Male under 6 to 18 years without health insurance coverage"= all_census$M_16_18__N_HIC)
    
    legendTitle = switch(input$hc_age, 
                         "Male under 6 years with health insurance coverage" = healthC_legendT[1], 
                         "Male under 6 years without health insurance coverage"= healthC_legendT[2],
                         "Male under 6 to 18 years with health insurance coverage"= healthC_legendT[3],
                         "Male under 6 to 18 years without health insurance coverage"= healthC_legendT[4])
    
    pal = colorBin(palette="OrRd", 9, domain= hcdata)
    
    labels = sprintf("<strong>%s<strong><br/>Total population: %g",
                     all_census$Geographic_Area_Name, hcdata) %>%
      lapply(htmltools::HTML)
    
    all_census %>% 
      st_transform("EPSG:4326") %>%
      leaflet() %>%
      addProviderTiles(provider="CartoDB.Positron") %>%
      addPolygons(label = labels, 
                  stroke = TRUE, 
                  color= "#d6d5de",
                  weight= 1,
                  smoothFactor = .5, 
                  opacity = 0.5,
                  fillOpacity = 0.7,
                  fillColor = ~pal(hcdata),
                  layerId = all_census$GEOID,
                  highlightOptions = highlightOptions(weight=2,
                                                      fillOpacity=1, 
                                                      color="black",
                                                      opacity=0.5,
                                                      bringToFront= TRUE)) %>%
      addLegend("bottomright", 
                pal=pal,
                values = ~hcdata,
                title = legendTitle,
                opacity = 0.7)
  })
  output$householdage = renderLeaflet({
    hhdata = switch(input$hh_age, 
                    "Children in households under 3 years old" = all_census$Tot_hld_U_3, 
                    "Children in households that are 3 to 4 years old" = all_census$Tot_hld_3_4,
                    "Children in households that are 5 years old" = all_census$Tot_hld_5.x,
                    "Children in households that are 6 to 8 years old"= all_census$Tot_hld_6_8, 
                    "Children in households that are 9 to 11 years old"= all_census$Tot_hld_9_11,
                    "Children in households that are 12 to 14 years old"= all_census$Tot_hld_12_14,
                    "Children in households that are 15 to 17 years old"= all_census$Tot_hld_15_17)
    
    legendTitle = switch(input$hh_age, 
                         "Children in households under 3 years old" = household_legendT[1], 
                         "Children in households that are 3 to 4 years old"= household_legendT[2],
                         "Children in households that are 5 years old" = household_legendT[3],
                         "Children in households that are 6 to 8 years old"= household_legendT[4], 
                         "Children in households that are 9 to 11 years old" = household_legendT[5],
                         "Children in households that are 12 to 14 years old" = household_legendT[6],
                         "Children in households that are 15 to 17 years old" = household_legendT[7])
    
    pal = colorBin(palette="OrRd", 6, domain= hhdata)
    
    labels = sprintf("<strong>%s<strong><br/>Total population: %g",
                     all_census$Geographic_Area_Name, hhdata) %>%
      lapply(htmltools::HTML)
    
    all_census %>% 
      st_transform("EPSG:4326") %>%
      leaflet() %>%
      addProviderTiles(provider="CartoDB.Positron") %>%
      addPolygons(label = labels, 
                  stroke = TRUE, 
                  color= "#d6d5de",
                  weight= 1,
                  smoothFactor = .5, 
                  opacity = 0.5,
                  fillOpacity = 0.7,
                  fillColor = ~pal(hhdata),
                  layerId = all_census$GEOID,
                  highlightOptions = highlightOptions(weight=2,
                                                      fillOpacity=1, 
                                                      color="black",
                                                      opacity=0.5,
                                                      bringToFront= TRUE)) %>%
      addLegend("bottomright", 
                pal=pal,
                values = ~hhdata,
                title = legendTitle,
                opacity = 0.7)
  })
  output$resources = renderLeaflet({
    rdata = switch(input$resource_type, 
                   "Water"= all_census$ALAND.x,
                   "Land" = all_census$AWATER.x)
    
    legendTitle = switch(input$resource_type, 
                         "Water"= resource_legendT[1],
                         "Land" = resource_legendT[2])
    
    pal = colorBin(palette="OrRd", 6, domain= rdata)
    
    labels = sprintf("<strong>%s<strong><br/>Total: %.13g",
                     all_census$Geographic_Area_Name, rdata) %>%
      lapply(htmltools::HTML)
    
    all_census %>% 
      st_transform("EPSG:4326") %>%
      leaflet() %>%
      addProviderTiles(provider="CartoDB.Positron") %>%
      addPolygons(label = labels, 
                  stroke = TRUE, 
                  color= "#d6d5de",
                  weight= 1,
                  smoothFactor = .5, 
                  opacity = 0.5,
                  fillOpacity = 0.7,
                  fillColor = ~pal(rdata),
                  layerId = all_census$GEOID,
                  highlightOptions = highlightOptions(weight=2,
                                                      fillOpacity=1, 
                                                      color="black",
                                                      opacity=0.5,
                                                      bringToFront= TRUE)) %>%
      addLegend("bottomright", 
                pal=pal,
                values = ~rdata,
                title = legendTitle,
                opacity = 0.7)
  })
  
  
  output$povertyhist = renderPlotly({
    population = switch(input$age, 
                        "Under 5 years" = all_census$Tot_pov_U_5_18, 
                        "5 to 17 years" = all_census$Tot_pov_U_5_17_18,
                        "Under 18 years" = all_census$Tot_pov_U_18,
                        "Related children of householder under 18 years" = all_census$Tot_pov_RUC_18)
    
    histTitle = switch(input$age, 
                       "Under 5 years" =  poverty_histT[1], 
                       "5 to 17 years" = poverty_histT[2],
                       "Under 18 years" = poverty_histT[3],
                       "Related children of householder under 18 years" = poverty_histT[4])
    
    meanTitle = switch(input$age, 
                       "Under 5 years" =  poverty_meanT[1], 
                       "5 to 17 years" = poverty_meanT[2],
                       "Under 18 years" = poverty_meanT[3],
                       "Related children of householder under 18 years" = poverty_meanT[4])
    
    mean = switch(input$age, 
                  "Under 5 years" = mean(all_census$Tot_pov_U_5_18), 
                  "5 to 17 years" = mean(all_census$Tot_pov_U_5_17_18),
                  "Under 18 years" = mean(all_census$Tot_pov_U_18),
                  "Related children of householder under 18 years" = mean(all_census$Tot_pov_RUC_18))
    
    p = all_census %>% 
      ggplot(aes(x=population)) + 
      geom_histogram(binwidth=100, fill='#edce95', color='white') + 
      ggtitle(histTitle) +
      labs(x = "Estimated total population", y = "Count") + 
      geom_vline(aes(xintercept=mean, linetype = meanTitle), color='red') +
      scale_linetype_manual(name = paste("Mean value: ", as.character(round(mean, digits = 0))), values= "dashed", 
                            guide = guide_legend(override.aes= list(color = 'red'))) +
      theme(plot.title = element_text(size=12, face= "bold", hjust=0.5),
            legend.title=element_text(size=9), 
            legend.text=element_text(size=8),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black"))
    
    
    # mode bar in the histogram is not shown
    # note: change displayModeBar to True to make the mode bar visible in the histogram
    gg = ggplotly(p, dynamicTicks= F, height=370) %>%
      config(displayModeBar = F) %>%
      layout(legend = list(x=0.98, xanchor='right', yanchor='top', y = 0.95, 
                           bordercolor=("rgba(0, 0, 0, 0.25)"), borderwidth=1,
                           bgcolor=("rgba(255, 255, 255, 0.75"))) 
  })
  output$healthcovhist = renderPlotly({
    population = switch(input$hc_age, 
                        "Male under 6 years with health insurance coverage" = all_census$M_U_6__W_HIC, 
                        "Male under 6 years without health insurance coverage"= all_census$M_U_6__N_HIC,
                        "Male under 6 to 18 years with health insurance coverage" = all_census$M_16_18__W_HIC,
                        "Male under 6 to 18 years without health insurance coverage"= all_census$M_16_18__N_HIC)
    
    histTitle = switch(input$hc_age, 
                       "Male under 6 years with health insurance coverage" = healthC_histT[1], 
                       "Male under 6 years without health insurance coverage"= healthC_histT[2],
                       "Male under 6 to 18 years with health insurance coverage"= healthC_histT[3],
                       "Male under 6 to 18 years without health insurance coverage"= healthC_histT[4])
    
    meanTitle = switch(input$hc_age, 
                       "Male under 6 years with health insurance coverage" = healthC_meanT[1], 
                       "Male under 6 years without health insurance coverage"= healthC_meanT[2],
                       "Male under 6 to 18 years with health insurance coverage"= healthC_meanT[3],
                       "Male under 6 to 18 years without health insurance coverage"= healthC_meanT[4])
    
    mean = switch(input$hc_age, 
                  "Male under 6 years with health insurance coverage" = mean(all_census$M_U_6__W_HIC), 
                  "Male under 6 years without health insurance coverage"= mean(all_census$M_U_6__N_HIC),
                  "Male under 6 to 18 years with health insurance coverage" = mean(all_census$M_16_18__W_HIC),
                  "Male under 6 to 18 years without health insurance coverage"= mean(all_census$M_16_18__N_HIC))
    
    p = all_census %>% 
      ggplot(aes(x=population)) + 
      geom_histogram(binwidth=100, fill="#edce95", color='white') + 
      ggtitle(histTitle) +
      labs(x = "Estimated total population", y = "Count") + 
      geom_vline(aes(xintercept=mean, linetype = meanTitle), color='red') +
      scale_linetype_manual(name = paste("Mean value: ", as.character(round(mean, digits = 0))), values= "dashed", 
                            guide = guide_legend(override.aes= list(color = 'red'))) +
      theme(plot.title = element_text(size=14, face= "bold", hjust=0.5),
            legend.title=element_text(size=10), 
            legend.text=element_text(size=9),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black"))
    
    
    # mode bar in the histogram is not shown
    # note: change displayModeBar to True to make the mode bar visible in the histogram
    gg = ggplotly(p, dynamicTicks= F, height=370) %>%
      config(displayModeBar = F) %>%
      layout(legend = list(x=0.98, xanchor='right', yanchor='top', y = 0.95, 
                           bordercolor=("rgba(0, 0, 0, 0.25)"), borderwidth=1,
                           bgcolor=("rgba(255, 255, 255, 0.75"))) 
  })
  output$hhist = renderPlotly({
    population = switch(input$hh_age, 
                        "Children in households under 3 years old" = all_census$Tot_hld_U_3, 
                        "Children in households that are 3 to 4 years old" = all_census$Tot_hld_3_4,
                        "Children in households that are 5 years old" = all_census$Tot_hld_5.x,
                        "Children in households that are 6 to 8 years old"= all_census$Tot_hld_6_8, 
                        "Children in households that are 9 to 11 years old"= all_census$Tot_hld_9_11,
                        "Children in households that are 12 to 14 years old"= all_census$Tot_hld_12_14,
                        "Children in households that are 15 to 17 years old"= all_census$Tot_hld_15_17)
    
    histTitle = switch(input$hh_age, 
                       "Children in households under 3 years old" = household_histT[1], 
                       "Children in households that are 3 to 4 years old"= household_histT[2],
                       "Children in households that are 5 years old" = household_histT[3],
                       "Children in households that are 6 to 8 years old"= household_histT[4], 
                       "Children in households that are 9 to 11 years old" = household_histT[5],
                       "Children in households that are 12 to 14 years old" = household_histT[6],
                       "Children in households that are 15 to 17 years old" = household_histT[7])
    
    meanTitle = switch(input$hh_age, 
                       "Children in households under 3 years old" = household_meanT[1], 
                       "Children in households that are 3 to 4 years old"= household_meanT[2],
                       "Children in households that are 5 years old" = household_meanT[3],
                       "Children in households that are 6 to 8 years old"= household_meanT[4], 
                       "Children in households that are 9 to 11 years old" = household_meanT[5],
                       "Children in households that are 12 to 14 years old" = household_meanT[6],
                       "Children in households that are 15 to 17 years old" = household_meanT[7])
    
    mean = switch(input$hh_age, 
                  "Children in households under 3 years old" = mean(all_census$Tot_hld_U_3), 
                  "Children in households that are 3 to 4 years old" = mean(all_census$Tot_hld_3_4),
                  "Children in households that are 5 years old" = mean(all_census$Tot_hld_5.x),
                  "Children in households that are 6 to 8 years old"= mean(all_census$Tot_hld_6_8), 
                  "Children in households that are 9 to 11 years old"= mean(all_census$Tot_hld_9_11),
                  "Children in households that are 12 to 14 years old"= mean(all_census$Tot_hld_12_14),
                  "Children in households that are 15 to 17 years old"= mean(all_census$Tot_hld_15_17))
    
    p = all_census %>% 
      ggplot(aes(x=population)) + 
      geom_histogram(binwidth=100, fill="#edce95", color='white') + 
      ggtitle(histTitle) +
      labs(x = "Estimated total population", y = "Count") + 
      geom_vline(aes(xintercept=mean, linetype = meanTitle), color='red') +
      scale_linetype_manual(name = paste("Mean value: ", as.character(round(mean, digits = 0))), values= "dashed", 
                            guide = guide_legend(override.aes= list(color = 'red'))) +
      theme(plot.title = element_text(size=14, face= "bold", hjust=0.5),
            legend.title=element_text(size=10), 
            legend.text=element_text(size=9),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black"))
    
    
    # mode bar in the histogram is not shown
    # note: change displayModeBar to True to make the mode bar visible in the histogram
    gg = ggplotly(p, dynamicTicks= F, height=370) %>%
      config(displayModeBar = F) %>%
      layout(legend = list(x=0.98, xanchor='right', yanchor='top', y = 0.95, 
                           bordercolor=("rgba(0, 0, 0, 0.25)"), borderwidth=1,
                           bgcolor=("rgba(255, 255, 255, 0.75"))) 
  })
  output$resourcehist = renderPlotly({
    resource = switch(input$resource_type, 
                      "Water"= all_census$ALAND.x,
                      "Land" = all_census$AWATER.x)
    
    histTitle = switch(input$resource_type, 
                       "Water"= resource_legendT[1],
                       "Land" = resource_legendT[2])
    
    meanTitle = switch(input$resource_type, 
                       "Water" = resource_meanT[1],
                       "Land" = resource_meanT[2])
    
    mean = switch(input$resource_type, 
                  "Water"= mean(all_census$ALAND.x),
                  "Land" = mean(all_census$AWATER.x))
    
    p = all_census %>% 
      ggplot(aes(x=resource)) + 
      geom_histogram(bins=5 , fill="#edce95", color='white') + 
      ggtitle(histTitle) +
      labs(x = "Estimated resource", y = "Count") + 
      geom_vline(aes(xintercept=mean, linetype = meanTitle), color='red') +
      scale_linetype_manual(name = paste("Mean value: ", as.character(round(mean, digits = 0))), values= "dashed", 
                            guide = guide_legend(override.aes= list(color = 'red'))) +
      theme(plot.title = element_text(size=14, face= "bold", hjust=0.5),
            legend.title=element_text(size=10), 
            legend.text=element_text(size=9),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black"))
    
    
    # mode bar in the histogram is not shown
    # note: change displayModeBar to True to make the mode bar visible in the histogram
    gg = ggplotly(p, dynamicTicks= F, height=370) %>%
      config(displayModeBar = F) %>%
      layout(legend = list(x=0.98, xanchor='right', yanchor='top', y = 0.95, 
                           bordercolor=("rgba(0, 0, 0, 0.25)"), borderwidth=1,
                           bgcolor=("rgba(255, 255, 255, 0.75"))) 
  })
  
  observeEvent(input$age,{
    click_ages <- reactiveValues( ids = vector() )
    observeEvent({input$poverty_shape_click},{
      click = input$poverty_shape_click
      if(is.null(click))
        return()
      else 
        print(is.null(click))
      print (click$id)
      povertydata = switch(input$age, 
                           "Under 5 years" = povertydata[1], 
                           "5 to 17 years" = povertydata[2],
                           "Under 18 years" = povertydata[3],
                           "Related children of householder under 18 years" = povertydata[4])
      id = click$id
      
      # filtering only the grade and geoid column from the dataframe
      
      print("here1")
      holder = all_census[, c(povertydata, "GEOID")]
      view(holder)
      # filter the data based on the id of the map that was clicked.
      data = holder[holder$GEOID == id, ]
      view(data)
      print("here2")
      
      # Get the value of the selected variable, and find out where it lies in the
      # histogram range and highlight that particular histogram.
      
      # find the min and max of dataset 
      
      variable =  switch(input$age, 
                         "Under 5 years" = holder$Tot_pov_U_5_18, 
                         "5 to 17 years" = holder$Tot_pov_U_5_17_18,
                         "Under 18 years" = holder$Tot_pov_U_18,
                         "Related children of householder under 18 years" = holder$Tot_pov_RUC_18)
      
      overalldata = switch(input$age, 
                           "Under 5 years" = data$Tot_pov_U_5_18, 
                           "5 to 17 years" = data$Tot_pov_U_5_17_18,
                           "Under 18 years" = data$Tot_pov_U_18,
                           "Related children of householder under 18 years" = data$Tot_pov_RUC_18)
      print(holder)
      print(variable)
      min = min(variable)
      max  = max(variable)
      print(min)
      print(max)
      # set the range between them
      range = 100
      
      # the highest bin
      #round up the max value to the nearest 500
      max_val = round_any(max, 100, f = ceiling)
      print(max_val)
      
      max_bin <- findInterval(max, vec = seq(from = min, to = max_val,by = range))
      print(max_bin)
      # have to round up the data 
      overalldata = round_any(overalldata, 100)
      # the bin to highlight in the histogram
      bin <- findInterval(overalldata, vec = seq(from = min, to = max_val,by = range))
      print(bin)
      
      # make a list of colors
      color_list =as.list(rep("#c0c0c0", max_bin + 1))
      
      # DESELECTION FUNCTION
      if(length(click_ages$ids) == 0)
      {
        # If length of click_ages$ids is zero, then we must
        # append the clicked id of the map.
        click_ages$ids = c(click_ages$ids, click$id)
        color_list[bin] = "#edce95"
      }
      else if(length(click_ages$ids)!= 0)
      {
        # if the current id clicked matches the one that has been inserted before, delete it.
        # map the map like it was previously mapped before
        print("CLICKED ID IS: ")
        print(click$id)
        if(click$id %in% click_ages$ids)
        {
          # Set the first element in the click_ages$ids to 0, as we're not 
          # keeping track of the map_id anymore.
          print("oh no :(")
          print(click_ages$ids)
          click_ages$ids = NULL
          print(click_ages$ids)
          
          # As this is the deselect function, we set the color of the bins to yellow.
          color_list =as.list(rep("#edce95", max_bin + 1))
        }
        # if id does not exist in the list, append it.
        else
        {
          print("oldie with the newbie")
          # Set the first element in the click_ages$ids to 0, as we have a new
          # element to keep a track of.
          click_ages$ids = NULL
          click_ages$ids = c(click_ages$ids, click$id)
          print(click_ages$ids)
          
          color_list[bin] = "#edce95"
        }
      }
      print(color_list)
      
      # how to make a new histogram with the right bin highlighted
      plotlyProxy("povertyhist", session) %>%
        plotlyProxyInvoke("restyle", list( marker = list(color = color_list)))
    })
  })
  observeEvent(input$hc_age,{
    click_hcage = reactiveValues(ids = vector())
    observeEvent({input$healthcoverage_shape_click},{
      click = input$healthcoverage_shape_click
      if(is.null(click))
        return()
      else 
        print(is.null(click))
      print (click$id)
      hcdata = switch(input$hc_age, 
                      "Male under 6 years with health insurance coverage" = hcdata[1], 
                      "Male under 6 years without health insurance coverage"= hcdata[2],
                      "Male under 6 to 18 years with health insurance coverage" = hcdata[3],
                      "Male under 6 to 18 years without health insurance coverage"= hcdata[4])
      id = click$id
      
      # filtering only the grade and geoid column from the dataframe
      
      holder = all_census[, c(hcdata, "GEOID")]
      view(holder)
      # filter the data based on the id of the map that was clicked.
      data = holder[holder$GEOID == id, ]
      view(data)
      
      # Get the value of the selected variable, and find out where it lies in the
      # histogram range and highlight that particular histogram.
      
      # find the min and max of dataset 
      
      variable =  switch(input$hc_age, 
                         "Male under 6 years with health insurance coverage" = holder$M_U_6__W_HIC, 
                         "Male under 6 years without health insurance coverage"= holder$M_U_6__N_HIC,
                         "Male under 6 to 18 years with health insurance coverage" = holder$M_16_18__W_HIC,
                         "Male under 6 to 18 years without health insurance coverage"= holder$M_16_18__N_HIC)
      
      overalldata = switch(input$hc_age, 
                           "Male under 6 years with health insurance coverage" = data$M_U_6__W_HIC, 
                           "Male under 6 years without health insurance coverage"= data$M_U_6__N_HIC,
                           "Male under 6 to 18 years with health insurance coverage" = data$M_16_18__W_HIC,
                           "Male under 6 to 18 years without health insurance coverage"= data$M_16_18__N_HIC)
      print(holder)
      print(variable)
      min = min(variable)
      max  = max(variable)
      print(min)
      print(max)
      # set the range between them
      range = 100
      
      # the highest bin
      #round up the max value to the nearest 500
      max_val = round_any(max, range, f = ceiling)
      print(max_val)
      
      max_bin <- findInterval(max, vec = seq(from = min, to = max_val,by = range))
      print(max_bin)
      # have to round up the data 
      overalldata = round_any(overalldata, range)
      # the bin to highlight in the histogram
      bin <- findInterval(overalldata, vec = seq(from = min, to = max_val,by = range))
      print(bin)
      
      # make a list of colors
      color_list =as.list(rep("#c0c0c0", max_bin + 1))
      
      # DESELECTION FUNCTION
      if(length(click_hcage$ids) == 0)
      {
        # If length of click_hcage$ids is zero, then we must
        # append the clicked id of the map.
        click_hcage$ids = c(click_hcage$ids, click$id)
        color_list[bin] = "#edce95"
      }
      else if(length(click_hcage$ids)!= 0)
      {
        # if the current id clicked matches the one that has been inserted before, delete it.
        # map the map like it was previously mapped before
        print("CLICKED ID IS: ")
        print(click$id)
        if(click$id %in% click_hcage$ids)
        {
          # Set the first element in the click_hcage$ids to 0, as we're not 
          # keeping track of the map_id anymore.
          print("oh no :(")
          print(click_hcage$ids)
          click_hcage$ids = NULL
          print(click_hcage$ids)
          
          # As this is the deselect function, we set the color of the bins to yellow.
          color_list =as.list(rep("#edce95", max_bin + 1))
        }
        # if id does not exist in the list, append it.
        else
        {
          print("oldie with the newbie")
          # Set the first element in the click_hcage$ids to 0, as we have a new
          # element to keep a track of.
          click_hcage$ids = NULL
          click_hcage$ids = c(click_hcage$ids, click$id)
          print(click_hcage$ids)
          
          color_list[bin] = "#edce95"
        }
      }
      
      # how to make a new histogram with the right bin highlighted
      plotlyProxy("healthcovhist", session) %>%
        plotlyProxyInvoke("restyle", list( marker = list(color = color_list)))
    })
  })
  observeEvent(input$hh_age, {
    click_hhage <- reactiveValues( ids = vector() )
    observeEvent({input$householdage_shape_click},{
      click = input$householdage_shape_click
      if(is.null(click))
        return()
      else 
        print(is.null(click))
      print (click$id)
      hhdata = switch(input$hh_age, 
                      "Children in households under 3 years old" = hhdata[1], 
                      "Children in households that are 3 to 4 years old" = hhdata[2],
                      "Children in households that are 5 years old" = hhdata[3],
                      "Children in households that are 6 to 8 years old"= hhdata[4], 
                      "Children in households that are 9 to 11 years old"= hhdata[5],
                      "Children in households that are 12 to 14 years old"= hhdata[6],
                      "Children in households that are 15 to 17 years old"= hhdata[7])
      id = click$id
      
      # filtering only the grade and geoid column from the dataframe
      
      holder = all_census[, c(hhdata, "GEOID")]
      view(holder)
      # filter the data based on the id of the map that was clicked.
      data = holder[holder$GEOID == id, ]
      view(data)
      
      # Get the value of the selected variable, and find out where it lies in the
      # histogram range and highlight that particular histogram.
      
      # find the min and max of dataset 
      
      variable =  switch(input$hh_age, 
                         "Children in households under 3 years old" = holder$Tot_hld_U_3, 
                         "Children in households that are 3 to 4 years old" = holder$Tot_hld_3_4,
                         "Children in households that are 5 years old" = holder$Tot_hld_5.x,
                         "Children in households that are 6 to 8 years old"= holder$Tot_hld_6_8, 
                         "Children in households that are 9 to 11 years old"= holder$Tot_hld_9_11,
                         "Children in households that are 12 to 14 years old"= holder$Tot_hld_12_14,
                         "Children in households that are 15 to 17 years old"= holder$Tot_hld_15_17)
      
      overalldata = switch(input$hh_age, 
                           "Children in households under 3 years old" = data$Tot_hld_U_3, 
                           "Children in households that are 3 to 4 years old" = data$Tot_hld_3_4,
                           "Children in households that are 5 years old" = data$Tot_hld_5.x,
                           "Children in households that are 6 to 8 years old"= data$Tot_hld_6_8, 
                           "Children in households that are 9 to 11 years old"= data$Tot_hld_9_11,
                           "Children in households that are 12 to 14 years old"= data$Tot_hld_12_14,
                           "Children in households that are 15 to 17 years old"= data$Tot_hld_15_17)
      print(holder)
      print(variable)
      min = min(variable)
      max  = max(variable)
      print(min)
      print(max)
      # set the range between them
      range = 100
      
      # the highest bin
      #round up the max value to the nearest 500
      max_val = round_any(max, range, f = ceiling)
      print(max_val)
      
      max_bin <- findInterval(max, vec = seq(from = min, to = max_val,by = range))
      print(max_bin)
      # have to round up the data 
      overalldata = round_any(overalldata, range)
      # the bin to highlight in the histogram
      bin <- findInterval(overalldata, vec = seq(from = min, to = max_val,by = range))
      print(bin)
      
      # make a list of colors
      color_list =as.list(rep("#c0c0c0", max_bin + 1))
      
      # DESELECTION FUNCTION
      if(length(click_hhage$ids) == 0)
      {
        # If length of click_hhage$ids is zero, then we must
        # append the clicked id of the map.
        click_hhage$ids = c(click_hhage$ids, click$id)
        color_list[bin] = "#edce95"
      }
      else if(length(click_hhage$ids)!= 0)
      {
        # if the current id clicked matches the one that has been inserted before, delete it.
        # map the map like it was previously mapped before
        print("CLICKED ID IS: ")
        print(click$id)
        if(click$id %in% click_hhage$ids)
        {
          # Set the first element in the click_hhage$ids to 0, as we're not 
          # keeping track of the map_id anymore.
          print("oh no :(")
          print(click_hhage$ids)
          click_hhage$ids = NULL
          print(click_hhage$ids)
          
          # As this is the deselect function, we set the color of the bins to yellow.
          color_list =as.list(rep("#edce95", max_bin + 1))
        }
        # if id does not exist in the list, append it.
        else
        {
          print("oldie with the newbie")
          # Set the first element in the click_hhage$ids to 0, as we have a new
          # element to keep a track of.
          click_hhage$ids = NULL
          click_hhage$ids = c(click_hhage$ids, click$id)
          print(click_hhage$ids)
          
          color_list[bin] = "#edce95"
        }
      }
      
      # how to make a new histogram with the right bin highlighted
      plotlyProxy("hhist", session) %>%
        plotlyProxyInvoke("restyle", list( marker = list(color = color_list)))            
    })
  })
  observeEvent(input$resource_type, {
    click_resource <- reactiveValues( ids = vector() )
    observeEvent({input$resources_shape_click},{
      click = input$resources_shape_click
      if(is.null(click))
        return()
      else 
        print(is.null(click))
      print (click$id)
      resourcesdata =  switch(input$resource_type, 
                              "Water"= resourcedata[1],
                              "Land" = resourcedata[2])
      id = click$id
      
      # filtering only the grade and geoid column from the dataframe
      
      holder = all_census[, c(resourcesdata, "GEOID")]
      #view(holder)
      # filter the data based on the id of the map that was clicked.
      data = holder[holder$GEOID == id, ]
      #view(data)
      
      # Get the value of the selected variable, and find out where it lies in the
      # histogram range and highlight that particular histogram.
      
      # find the min and max of dataset 
      
      variable =  switch(input$resource_type, 
                         "Water"= holder$ALAND.x,
                         "Land" = holder$AWATER.x)
      
      overalldata = switch(input$resource_type, 
                           "Water"= data$ALAND.x,
                           "Land" = data$AWATER.x)
      #print(holder)
      #print(variable)
      min = min(variable)
      max  = max(variable)
      #print(min)
      #print(max)
      # set the range between them
      range = 5000000000
      
      # the highest bin
      #round up the max value to the nearest 500
      max_val = round_any(max, range, f = ceiling)
      #print(max_val)
      
      max_bin <- findInterval(max, vec = seq(from = min, to = max_val,by = range))
      #print("max bin")
      #print(max_bin)
      # have to round up the data 
      # the bin to highlight in the histogram
      bin <- findInterval(overalldata, vec = seq(from = min, to = max_val,by = range))
      # make a list of colors
      color_list =as.list(rep("#c0c0c0", max_bin + 1))
      
      # DESELECTION FUNCTION
      if(length(click_resource$ids) == 0)
      {
        # If length of click_resource$ids is zero, then we must
        # append the clicked id of the map.
        click_resource$ids = c(click_resource$ids, click$id)
        color_list[bin] = "#edce95"
      }
      else if(length(click_resource$ids)!= 0)
      {
        # if the current id clicked matches the one that has been inserted before, delete it.
        # map the map like it was previously mapped before
        print("CLICKED ID IS: ")
        print(click$id)
        if(click$id %in% click_resource$ids)
        {
          # Set the first element in the click_resource$ids to 0, as we're not 
          # keeping track of the map_id anymore.
          #print("oh no :(")
          #print(click_resource$ids)
          click_resource$ids = NULL
          #print(click_resource$ids)
          
          # As this is the deselect function, we set the color of the bins to yellow.
          color_list =as.list(rep("#edce95", max_bin + 1))
        }
        # if id does not exist in the list, append it.
        else
        {
          #print("oldie with the newbie")
          # Set the first element in the click_resource$ids to 0, as we have a new
          # element to keep a track of.
          click_resource$ids = NULL
          click_resource$ids = c(click_resource$ids, click$id)
          #print(click_resource$ids)
          
          color_list[bin] = "#edce95"
        }
      }
      
      # how to make a new histogram with the right bin highlighted
      plotlyProxy("resourcehist", session) %>%
        plotlyProxyInvoke("restyle", list( marker = list(color = color_list)))            
    })
  })
  
  #================================
  # if statement for the textOuput!
  #================================
  
  #------------ poverty data --------------
  output$poverty_variableTitle = renderText({
    out = ""
    if(input$age == "Under 5 years")
    {
      out = paste("About under 5 years")
    }
    if(input$age == "5 to 17 years")
    {
      out = paste("About 5 to 17 years")
    }
    if(input$age == "Under 18 years")
    {
      out = paste("About under 18 years")
    }
    if(input$age == "Related children of householder under 18 years")
    {
      out = paste("About related children of householder under 18 years")
    }
    out
  })
  output$poverty_variable = renderText({
    out = ""
    if(input$age == "Under 5 years")
    {
      out = paste("Explanation 1")
    }
    if(input$age == "5 to 17 years")
    {
      out = paste("Explanation 2")
    }
    if(input$age == "Under 18 years")
    {
      out = paste("Explanation 3")
    }
    if(input$age == "Related children of householder under 18 years")
    {
      out = paste("Explanation 4")
    }
    out
  })
  
  #------------ health cov hist --------------
  output$hc_title = renderText({
    out = ""
    if(input$hc_age == "Male under 6 years with health insurance coverage")
    {
      out = paste("About Variable 1")
    }
    if(input$hc_age == "Male under 6 years without health insurance coverage")
    {
      out = paste("About Variable 2")
    }
    if(input$hc_age == "Male under 6 to 18 years with health insurance coverage")
    {
      out = paste("About Variable 3")
    }
    if(input$hc_age == "Male under 6 to 18 years without health insurance coverage")
    {
      out = paste("About Variable 4")
    }
    out
  })
  output$hc_variable = renderText({
    out = ""
    if(input$hc_age == "Male under 6 years with health insurance coverage")
    {
      out = paste("Explanation 1 - Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore 
                                     et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut 
                                     aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum 
                                     dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia 
                                     deserunt mollit anim id est laborum.")
    }
    if(input$age == "Male under 6 years without health insurance coverage")
    {
      out = paste("Explanation 2 - Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore 
                                     et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut 
                                     aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum 
                                     dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia 
                                     deserunt mollit anim id est laborum.")
    }
    if(input$age == "Male under 6 to 18 years with health insurance coverage")
    {
      out = paste("Explanation 3 - Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore 
                                     et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut 
                                     aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum 
                                     dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia 
                                     deserunt mollit anim id est laborum.")
    }
    if(input$age == "Male under 6 to 18 years without health insurance coverage")
    {
      out = paste("Explanation 4 - Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore 
                                     et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut 
                                     aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum 
                                     dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia 
                                     deserunt mollit anim id est laborum.")
    }
    out
  })
  
  #------------ population in household by age --------------
  output$hhage_title = renderText({
    out = ""
    if(input$hh_age == "Children in households under 3 years old")
    {
      out = paste("About under 3 years")
    }
    if(input$hh_age == "Children in households that are 3 to 4 years old")
    {
      out = paste("About 3 to 4 years")
    }
    if(input$hh_age == "Children in households that are 5 years old")
    {
      out = paste("About 5 year olds")
    }
    if(input$hh_age =="Children in households that are 6 to 8 years old")
    {
      out = paste("About 6 to 8 years")
    }
    if(input$hh_age == "Children in households that are 12 to 14 years old")
    {
      out = paste("About under 14 years")
    }
    if(input$hh_age == "Children in households that are 15 to 17 years old")
    {
      out = paste("About 15 to 17 years")
    }
    out
  })
  output$hhage_variable = renderText({
    out = ""
    if(input$hh_age == "Under 5 years")
    {
      out = paste("Explanation 1 - Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore 
                                     et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut 
                                     aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum 
                                     dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia 
                                     deserunt mollit anim id est laborum.")
    }
    if(input$hh_age == "5 to 17 years")
    {
      out = paste("Explanation 2 - Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore 
                                     et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut 
                                     aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum 
                                     dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia 
                                     deserunt mollit anim id est laborum.")
    }
    if(input$hh_age == "Under 18 years")
    {
      out = paste("Explanation 3 - Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore 
                                     et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut 
                                     aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum 
                                     dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia 
                                     deserunt mollit anim id est laborum.")
    }
    if(input$hh_age == "Related children of householder under 18 years")
    {
      out = paste("Explanation 4 - Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore 
                                     et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut 
                                     aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum 
                                     dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia 
                                     deserunt mollit anim id est laborum.")
    }
    out
  })
  
  #------------ resources --------------
  output$resources_title = renderText({
    out = ""
    if(input$resource_type == "Water")
    {
      out = paste("About Water")
    }
    if(input$resource_type == "Land")
    {
      out = paste("About Land")
    }
    out
  })
  output$resources_variable = renderText({
    out = ""
    if(input$resource_type == "Water")
    {
      out = paste("About Water - Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore 
                                     et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut 
                                     aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum 
                                     dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia 
                                     deserunt mollit anim id est laborum.")
    }
    if(input$resource_type == "Land")
    {
      out = paste("About Land - Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore 
                                     et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut 
                                     aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum 
                                     dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia 
                                     deserunt mollit anim id est laborum.")
    }
    out
  })
}
