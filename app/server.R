options(shiny.autoreload = TRUE)

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
  # Functions for the school districts (UI outputs)
  #------------------------------------
  # Description:
  #   Fetches text from the file "SchoolDistricts_ASPIRE_Collapsed.csv" to put on the
  #   homepage, and then creates an html element for that text with
  #.  the second call.
  # Mock-up variable screenshot name:
  #   On the mock-up, it's listed underneath the Format and Screenshots of Variables
  #.  section.
  #================================================================
  
  # for Grades
  output$sd_topic_desc = renderUI({
    text = fetch_text_sd("Topic_Desc", input$grades)
    generateSchoolUI("Topic_Desc", text)
  })
  
  output$sd_indicator_desc = renderUI({
    text = fetch_text_sd("Indicator_Desc", input$grades)
    generateSchoolUI("Indicator_Desc", text)
  })
  
  # This is the only exception here with the output being Text not UI
  output$sd_indicator_title = renderText({
    text = fetch_text_sd("Indicator_Title", input$grades)
  })
  
  output$sd_image = renderUI({
    img_url = fetch_text_sd("Image_url", input$grades)
    generatePic(img_url)
  })
  
  # for Race
  output$sd_topic_desc_r = renderUI({
    text = fetch_text_sd("Topic_Desc", input$race_opt)
    generateSchoolUI("Topic_Desc", text)
  })
  
  output$sd_indicator_desc_r = renderUI({
    text = fetch_text_sd("Indicator_Desc", input$race_opt)
    generateSchoolUI("Indicator_Desc", text)
  })
  
  # This is the only exception here with the output being Text not UI
  output$sd_indicator_title_r = renderText({
    text = fetch_text_sd("Indicator_Title", input$race_opt)
  })
  
  output$sd_image_r = renderUI({
    img_url = fetch_text_sd("Image_url", input$race_opt)
    generatePic(img_url)
  })
  
  #================================================================
  # Functions for the homepage (Ui outputs)
  #------------------------------------
  # > Mission_Text
  # > Project_1_Title
  # > Project_1_Desc
  # > Project_2_Title
  # > Project_2_Desc
  # > Project_3_Title
  # > Project_3_Desc
  # > Project_4_Title
  # > Project_4_Desc
  # ------------------------------------
  # Description:
  #   Fetches text from the file "Website_ASPIRE.csv" to put on the
  #   homepage. 
  # Mock-up variable screenshot name:
  #   On the mock-up, it's listed underneath the Format and Screenshots of Variables
  #.  section.
  #================================================================
  
  output$mission_text = renderUI({
    text = fetch_text_webpage("Mission_Text")
    generateWebText("Mission_Text", text)
  })
  
  output$project_1_title = renderUI({
    text = fetch_text_webpage("Project_1_Title")
    generateWebText("Project_1_Title", text)
  })
  
  output$project_1_desc = renderUI({
    text = fetch_text_webpage("Project_1_Desc")
    generateWebText("Project_1_Desc", text)
  })
  
  output$project_2_title = renderUI({
    text = fetch_text_webpage("Project_2_Title")
    generateWebText("Project_3_Title", text)
  })
  
  output$project_2_desc = renderUI({
    text = fetch_text_webpage("Project_2_Desc")
    generateWebText("Project_2_Desc", text)
  })
  
  output$project_3_title = renderUI({
    text = fetch_text_webpage("Project_3_Title")
    generateWebText("Project_3_Title", text)
  })
  
  output$project_3_desc = renderUI({
    text = fetch_text_webpage("Project_3_Desc")
    generateWebText("Project_3_Desc", text)
  })
  
  output$project_4_title = renderUI({
    text = fetch_text_webpage("Project_4_Title")
    generateWebText("Project_4_Title", text)
  })
  
  output$project_4_desc = renderUI({
    text = fetch_text_webpage("Project_4_Desc")
    generateWebText("Project_4_Desc", text)
  })
  
  
  #================================================================
  # Maps for each of the variables
  #================================================================
  
  #--------- Leaflet Map for Census Data
  output$poverty = renderLeaflet({
    povertydata = fetch_pd(input$age, "data")
    legendTitle = fetch_pd(input$age, "legend")
    
    map = leafletMap(povertydata, legendTitle)
  })
  
  
  #================================================================
  # Functions for the census page (Ui outputs, Text output)
  #------------------------------------
  # Description:
  #   Fetches text from the file "Census_ASPIRE_Collapsed.csv" to put on the
  #   census page
  # Mock-up variable screenshot name:
  #   On the mock-up, it's listed underneath the Format and Screenshots of Variables
  #.  section.
  #================================================================
  
  #------------------------------------------------ POVERTY STATUS FETCH FUNCTIONS
  output$pov_topic_desc = renderUI({
    text = fetch_text_census("Topic_Desc", "0", input$age)
    generateCensusUI("Topic_Desc", text)
  })
  
  output$pov_indicator_title = renderText({
    text = fetch_text_census("Indicator_Title", "0", input$age)
  })
  
  output$picture_holder_pov = renderUI({
    img_url = fetch_text_census("url", "", input$age)
    generatePic(img_url)
  })
  
  output$census_summary_pov = renderUI({
    text = fetch_text_census("summary", "0", input$age)
    generateCensusUI("summary", text)
  })
  
  
  #------------------------------------------------ HEALTH COVERAGE FETCH FUNCTIONS
  output$hc_topic_desc = renderUI({
    text = fetch_text_census("Topic_Desc", "0", input$hc_age)
    generateCensusUI("Topic_Desc", text)
  })
  
  output$hc_indicator_title = renderText({
    text = fetch_text_census("Indicator_Title", "0", input$hc_age)
  })
  
  output$picture_holder_hc = renderUI({
    img_url = fetch_text_census("url", "", input$hc_age)
    generatePic(img_url)
  })
  
  output$census_summary_hc = renderUI({
    text = fetch_text_census("summary", "0", input$hc_age)
    generateCensusUI("summary", text)
  })
  
  #--------------------------------- POPULATION IN HOUSEHOLD BY AGE FETCH FUNCTIONS
  
  output$census_summary_hha = renderUI({
    text = fetch_text_census("summary", "0", input$hh_age)
    generateCensusUI("summary", text)
  })
  
  output$picture_holder_hha = renderUI({
    img_url = fetch_text_census("url", "", input$hh_age)
    generatePic(img_url)
  })
  
  output$hha_topic_desc = renderUI({
    text = fetch_text_census("Topic_Desc", "0", input$hh_age)
    generateCensusUI("Topic_Desc", text)
  })
  
  output$hha_indicator_title = renderText({
    text = fetch_text_census("Indicator_Title", "0", input$hh_age)
  })
  
  #--------------------------------- RESOURCES FETCH FUNCTIONS
  
  output$census_summary = renderUI({
    text = fetch_text_census("summary", "0", input$resource_type)
    generateCensusUI("summary", text)
  })
  
  output$picture_holder_resource = renderUI({
    img_url =fetch_text_census("url", "", input$resource_type)
    generatePic(img_url)
  })
  
  output$r_topic_desc = renderUI({
    text = fetch_text_census("Topic_Desc", "0", input$resource_type)
    generateCensusUI("Topic_Desc", text)
  })
  
  output$r_indicator_title = renderText({
    text = fetch_text_census("Indicator_Title", "0", input$resource_type)
  })
  
  #------------------------------------------------- fetch functions for environmental variable topic in census
  
  output$envvar_summary = renderText({
    fetch_text_wildfire("summary", "0", input$env_var)
  })
  
  
  output$fire_resources = renderUI({
    resource_name = as.array(fetch_text_wildfire("resources", "names", input$env_var))
    resource_links = as.array(fetch_text_wildfire("resources", "links", input$env_var))
    
    generateListItems(resource_name, resource_links)
  })
  
  output$picture_holder = renderUI({
    img_url = fetch_text_wildfire("url", "", input$env_var)
    generatePic(img_url)
  })
  
  output$fire_policies = renderUI({
    array_name = as.array(fetch_text_wildfire("policies", "names", input$env_var))
    array_links = as.array(fetch_text_wildfire("policies", "links", input$env_var))
    array_desc = as.array(fetch_text_wildfire("policies", "desc", input$env_var))
  
    generatePoliciesItems(array_name, array_links, array_desc)
  })
  
  output$fire_lr = renderUI({
    array_name = as.array(fetch_text_wildfire("localResources", "names", input$env_var))
    array_links = as.array(fetch_text_wildfire("localResources", "links", input$env_var))
    array_desc = as.array(fetch_text_wildfire("localResources", "desc", input$env_var))
    
    generatePoliciesItems(array_name, array_links, array_desc)
  })
  
  output$env_var_title = renderText({
    out = ""
    if(input$env_var == "Widlfire Smoke")
    {
      out = paste("Wildfire Smoke, 2022-2023")
    }
    if(input$env_var == "Radon")
    {
      out = paste("Radon, 2022-2023")
    }
    out
  })
  
  output$envvar_title = renderText({
    fetch_text_wildfire("title", "0", input$env_var)
  })
  
#------------------------------------------------------ leaflet functions for census data page
  
  output$healthcoverage = renderLeaflet({
    hcdata = fetch_hc(input$hc_age, "data")
    legendTitle = fetch_hc(input$hc_age, "legend")
    
    map = leafletMap(hcdata, legendTitle)
  })
  output$householdage = renderLeaflet({
    hhdata = fetch_hha(input$hh_age, "data")
    legendTitle = fetch_hha(input$hh_age, "legend")
    
    map = leafletMap(hhdata, legendTitle)
  })
  output$resources = renderLeaflet({
    rdata = fetch_resources(input$resource_type, "data")
    legendTitle = fetch_resources(input$resource_type, "legend")
    
    map = leafletMap(rdata, legendTitle)
  })
  
  #--------------------------------------------------------------- Histogram for Census data
  
  # Histogram for poverty status
  output$povertyhist = renderPlotly({
    population = fetch_hist(input$age, "poverty", "population")
    histTitle = fetch_hist(input$age, "poverty", "histTitle")
    meanTitle = fetch_hist(input$age, "poverty", "meanTitle")
    meanVal = fetch_hist(input$age, "poverty", "mean")
    
    print(meanVal)
    
    p = fetch_histogram(population, histTitle, meanTitle, meanVal,"normal")
    
    
    # mode bar in the histogram is not shown
    # note: change displayModeBar to True to make the mode bar visible in the histogram
    gg = ggplotly(p, dynamicTicks= F, height=370) %>%
      config(displayModeBar = F) %>%
      layout(legend = list(x=0.98, xanchor='right', yanchor='top', y = 0.95, 
                           bordercolor=("rgba(0, 0, 0, 0.25)"), borderwidth=1,
                           bgcolor=("rgba(255, 255, 255, 0.75"))) 
  })
  
  # Histogram for health insurance coverage 
  output$healthcovhist = renderPlotly({
    population = fetch_hist(input$hc_age, "hc", "population")
    
    histTitle = fetch_hist(input$hc_age, "hc", "histTitle")
    
    meanTitle = fetch_hist(input$hc_age, "hc", "meanTitle")
    
    mean = fetch_hist(input$hc_age, "hc", "mean")
    
    p = fetch_histogram(population, histTitle, meanTitle, mean,"normal")
    
    # mode bar in the histogram is not shown
    # note: change displayModeBar to True to make the mode bar visible in the histogram
    gg = ggplotly(p, dynamicTicks= F, height=370) %>%
      config(displayModeBar = F) %>%
      layout(legend = list(x=0.98, xanchor='right', yanchor='top', y = 0.95, 
                           bordercolor=("rgba(0, 0, 0, 0.25)"), borderwidth=1,
                           bgcolor=("rgba(255, 255, 255, 0.75"))) 
  })
  
  # Histogram for population in household by age
  output$hhist = renderPlotly({
    population = fetch_hist(input$hh_age, "hha", "population")
    
    histTitle = fetch_hist(input$hh_age, "hha", "histTitle")
    
    meanTitle = fetch_hist(input$hh_age, "hha", "meanTitle")
    
    mean = fetch_hist(input$hh_age, "hha", "mean")
    
    p = fetch_histogram(population, histTitle, meanTitle, mean, "normal")
    
    # mode bar in the histogram is not shown
    # note: change displayModeBar to True to make the mode bar visible in the histogram
    gg = ggplotly(p, dynamicTicks= F, height=370) %>%
      config(displayModeBar = F) %>%
      layout(legend = list(x=0.98, xanchor='right', yanchor='top', y = 0.95, 
                           bordercolor=("rgba(0, 0, 0, 0.25)"), borderwidth=1,
                           bgcolor=("rgba(255, 255, 255, 0.75"))) 
  })
  
  # Histogram for Resources
  output$resourcehist = renderPlotly({
    resource = fetch_hist(input$resource_type, "resources", "population")
    
    histTitle = fetch_hist(input$resource_type, "resources", "histTitle")
    
    meanTitle = fetch_hist(input$resource_type, "resources", "meanTitle")
    
    meanVal = fetch_hist(input$resource_type, "resources", "mean")
    
    p = fetch_histogram(population, histTitle, meanTitle, meanVal,"smallBin")
    
    # mode bar in the histogram is not shown
    # note: change displayModeBar to True to make the mode bar visible in the histogram
    gg = ggplotly(p, dynamicTicks= F, height=370) %>%
      config(displayModeBar = F) %>%
      layout(legend = list(x=0.98, xanchor='right', yanchor='top', y = 0.95, 
                           bordercolor=("rgba(0, 0, 0, 0.25)"), borderwidth=1,
                           bgcolor=("rgba(255, 255, 255, 0.75"))) 
  })
  
  output$env_variable = renderUI({
    turn_to_list("resources")
  })
  
  #----------------------------------------------------- Observe Event for Census Data
  
  # When an area in the leaflet map is clicked, highlight over it on the histogram.
  
  # Observe the interactions on the poverty status leaflet map.
  observeEvent(input$age,{
    click_ages <- reactiveValues( ids = vector() )
    observeEvent({input$poverty_shape_click},{
      click = input$poverty_shape_click
      if(is.null(click))
        return()
      else 
        print(is.null(click))
      print (click$id)
      povertydata = fetch_pd(input$age, "povertydata")
      id = click$id
      
      # filtering only the grade and geoid column from the dataframe
      
      #print("here1")
      #holder = all_census[, c(povertydata, "GEOID")]
      #view(holder)
      # filter the data based on the id of the map that was clicked.
      #data = holder[holder$GEOID == id, ]
      #view(data)
      #print("here2")
      
      # Get the value of the selected variable, and find out where it lies in the
      # histogram range and highlight that particular histogram.
      
      # find the min and max of dataset 
      
      variable =  fetch_observe_vars(input$age, "poverty", "variable", id, povertydata)
      overalldata = fetch_observe_vars(input$age, "poverty", "overalldata", id, povertydata)
      
      #print(holder)
      #print(variable)
      #min = min(variable)
      #max  = max(variable)
      #print(min)
      #print(max)
      # set the range between them
      range = 100
      
      # the highest bin
      #round up the max value to the nearest 500
      max_val = round_any(max, 100, f = ceiling)
      #print(max_val)
      
      max_bin <- findInterval(max, vec = seq(from = min, to = max_val,by = range))
      #print(max_bin)
      # have to round up the data 
      overalldata = round_any(overalldata, 100)
      # the bin to highlight in the histogram
      bin <- findInterval(overalldata, vec = seq(from = min, to = max_val,by = range))
      #print(bin)
      
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
        #print("CLICKED ID IS: ")
        #print(click$id)
        if(click$id %in% click_ages$ids)
        {
          # Set the first element in the click_ages$ids to 0, as we're not 
          # keeping track of the map_id anymore.
          #print("oh no :(")
          #print(click_ages$ids)
          click_ages$ids = NULL
          print(click_ages$ids)
          
          # As this is the deselect function, we set the color of the bins to yellow.
          color_list =as.list(rep("#edce95", max_bin + 1))
        }
        # if id does not exist in the list, append it.
        else
        {
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
  
  # Observe the interactions on the health insurance coverage leaflet map.
  observeEvent(input$hc_age,{
    click_hcage = reactiveValues(ids = vector())
    observeEvent({input$healthcoverage_shape_click},{
      click = input$healthcoverage_shape_click
      if(is.null(click))
        return()
      else 
        print(is.null(click))
      print (click$id)
      hcdata = fetch_hc(input$hc_age, hcdata)
      id = click$id
      
      # filtering only the grade and geoid column from the dataframe
      
      #holder = all_census[, c(hcdata, "GEOID")]
      #view(holder)
      # filter the data based on the id of the map that was clicked.
      #data = holder[holder$GEOID == id, ]
      #view(data)
      
      # Get the value of the selected variable, and find out where it lies in the
      # histogram range and highlight that particular histogram.
      
      # find the min and max of dataset 
      
      variable = fetch_observe_vars(input$hc_age, "hc", "variable", id, hcdata)
      
      overalldata = fetch_observe_vars(input$hc_age, "hc", "overalldata", id, hcdata)
      
      #print(holder)
      #print(variable)
      min = min(variable)
      max  = max(variable)
      #print(min)
      #print(max)
      # set the range between them
      range = 100
      
      # the highest bin
      #round up the max value to the nearest 500
      max_val = round_any(max, range, f = ceiling)
      #print(max_val)
      
      max_bin <- findInterval(max, vec = seq(from = min, to = max_val,by = range))
      #print(max_bin)
      # have to round up the data 
      overalldata = round_any(overalldata, range)
      # the bin to highlight in the histogram
      bin <- findInterval(overalldata, vec = seq(from = min, to = max_val,by = range))
      #print(bin)
      
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
        #print("CLICKED ID IS: ")
        #print(click$id)
        if(click$id %in% click_hcage$ids)
        {
          # Set the first element in the click_hcage$ids to 0, as we're not 
          # keeping track of the map_id anymore.
          #print(click_hcage$ids)
          click_hcage$ids = NULL
          #print(click_hcage$ids)
          
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
  
  # Observe the interactions on population in household by age leaflet map.
  observeEvent(input$hh_age, {
    click_hhage <- reactiveValues( ids = vector() )
    observeEvent({input$householdage_shape_click},{
      click = input$householdage_shape_click
      if(is.null(click))
        return()
      else 
        print(is.null(click))
      #print (click$id)
      hhdata = fetch_hha(input$hh_age, "hhdata")
      id = click$id
      
      # filtering only the grade and geoid column from the dataframe
      
      #holder = all_census[, c(hhdata, "GEOID")]
      #view(holder)
      # filter the data based on the id of the map that was clicked.
      #data = holder[holder$GEOID == id, ]
      #view(data)
      
      # Get the value of the selected variable, and find out where it lies in the
      # histogram range and highlight that particular histogram.
      
      # find the min and max of dataset 
      
      variable =  fetch_observe_vars(input$hh_age, "hha", "variable", id, hhdata)
      overalldata = fetch_observe_vars(input$hh_age, "hha", "overalldata", id, hhdata)
        
      #print(holder)
      #print(variable)
      min = min(variable)
      max  = max(variable)
      #print(min)
      #print(max)
      # set the range between them
      range = 100
      
      # the highest bin
      #round up the max value to the nearest 500
      max_val = round_any(max, range, f = ceiling)
      #print(max_val)
      
      max_bin <- findInterval(max, vec = seq(from = min, to = max_val,by = range))
      #print(max_bin)
      # have to round up the data 
      overalldata = round_any(overalldata, range)
      # the bin to highlight in the histogram
      bin <- findInterval(overalldata, vec = seq(from = min, to = max_val,by = range))
      #print(bin)
      
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
        #print("CLICKED ID IS: ")
        #print(click$id)
        if(click$id %in% click_hhage$ids)
        {
          # Set the first element in the click_hhage$ids to 0, as we're not 
          # keeping track of the map_id anymore.
          #print(click_hhage$ids)
          click_hhage$ids = NULL
          #print(click_hhage$ids)
          
          # As this is the deselect function, we set the color of the bins to yellow.
          color_list =as.list(rep("#edce95", max_bin + 1))
        }
        # if id does not exist in the list, append it.
        else
        {
          # Set the first element in the click_hhage$ids to 0, as we have a new
          # element to keep a track of.
          click_hhage$ids = NULL
          click_hhage$ids = c(click_hhage$ids, click$id)
          #print(click_hhage$ids)
          
          color_list[bin] = "#edce95"
        }
      }
      
      # how to make a new histogram with the right bin highlighted
      plotlyProxy("hhist", session) %>%
        plotlyProxyInvoke("restyle", list( marker = list(color = color_list)))            
    })
  })
  
  # Observe the interactions on the Resources leaflet map.
  observeEvent(input$resource_type, {
    click_resource <- reactiveValues( ids = vector() )
    observeEvent({input$resources_shape_click},{
      click = input$resources_shape_click
      if(is.null(click))
        return()
      else 
        print(is.null(click))
      #print (click$id)
      resourcesdata =  fetch_resources(input$resource_type, "resourcesdata")
      id = click$id
      
      # filtering only the grade and geoid column from the dataframe
      
      #holder = all_census[, c(resourcesdata, "GEOID")]
      #view(holder)
      # filter the data based on the id of the map that was clicked.
      #data = holder[holder$GEOID == id, ]
      #view(data)
      
      # Get the value of the selected variable, and find out where it lies in the
      # histogram range and highlight that particular histogram.
      
      # find the min and max of dataset 
      
      variable =  fetch_observe_vars(input$resource_type, "resources", "variable", id, resourcesdata)
    
      overalldata = fetch_observe_vars(input$resource_type, "resources", "overalldata", id, resourcesdata)
        
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
        #print("CLICKED ID IS: ")
        #print(click$id)
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

  
  #------------------------------------------------- SCHOOL DISTRICTS
  
  #--------- Leaflet Map for School Districts.
  output$grades = renderLeaflet({
    gradedata = fetch_sd_leaflet_var(input$grades, "grades", "gradedata")
    legendTitle = fetch_sd_leaflet_var(input$grades, "grades", "legendTitle")


    # Calling a function to render Leaflet.
    fetch_sd_leaflet_map(gradedata, legendTitle)
  })
  output$race = renderLeaflet({
    racedata = fetch_sd_leaflet_var(input$race_opt, "race", "racedata")
    legendTitle = fetch_sd_leaflet_var(input$race_opt, "race", "legendTitle")

    fetch_sd_leaflet_map(racedata, legendTitle)
  })
  
  #--------- Histogram for School Districts data
  output$gradehist = renderPlotly({
    population = switch(input$grades,
                        "Kindergarten" = all_schoold$F2021_22_Kindergarten,
                        "Grade one"= all_schoold$F2021_22_Grade_One,
                        "Grade two"= all_schoold$F2021_22_Grade_Two,
                        "Grade three"= all_schoold$F2021_22_Grade_Three,
                        "Grade four"= all_schoold$F2021_22_Grade_Four,
                        "Grade five"= all_schoold$F2021_22_Grade_Five,
                        "Grade six"= all_schoold$F2021_22_Grade_Six,
                        "Grade seven"= all_schoold$F2021_22_Grade_Seven,
                        "Grade eight"= all_schoold$F2021_22_Grade_Eight,
                        "Grade nine"= all_schoold$F2021_22_Grade_Nine,
                        "Grade ten"= all_schoold$F2021_22_Grade_Ten,
                        "Grade eleven"= all_schoold$F2021_22_Grade_Eleven,
                        "Grade twelve"= all_schoold$F2021_22_Grade_Twelve)

    histTitle = switch(input$grades,
                       "Kindergarten"= grades_histTitle[1],
                       "Grade one"= grades_histTitle[2],
                       "Grade two"= grades_histTitle[3],
                       "Grade three"= grades_histTitle[4],
                       "Grade four"= grades_histTitle[5],
                       "Grade five"= grades_histTitle[6],
                       "Grade six"= grades_histTitle[7],
                       "Grade seven"= grades_histTitle[8],
                       "Grade eight"= grades_histTitle[9],
                       "Grade nine"= grades_histTitle[10],
                       "Grade ten"= grades_histTitle[11],
                       "Grade eleven"= grades_histTitle[12],
                       "Grade twelve"= grades_histTitle[13])

    meanTitle = switch(input$grades,
                       "Kindergarten"= grades_meanTitle[1],
                       "Grade one"= grades_meanTitle[2],
                       "Grade two"= grades_meanTitle[3],
                       "Grade three"= grades_meanTitle[4],
                       "Grade four"= grades_meanTitle[5],
                       "Grade five"= grades_meanTitle[6],
                       "Grade six"= grades_meanTitle[7],
                       "Grade seven"= grades_meanTitle[8],
                       "Grade eight"= grades_meanTitle[9],
                       "Grade nine"= grades_meanTitle[10],
                       "Grade ten"= grades_meanTitle[11],
                       "Grade eleven"= grades_meanTitle[12],
                       "Grade twelve"= grades_meanTitle[13])


    mean = switch(input$grades,
                  "Kindergarten" = mean(all_schoolna$F2021_22_Kindergarten),
                  "Grade one"= mean(all_schoolna$F2021_22_Grade_One),
                  "Grade two"= mean(all_schoolna$F2021_22_Grade_Two),
                  "Grade three"= mean(all_schoolna$F2021_22_Grade_Three),
                  "Grade four"= mean(all_schoolna$F2021_22_Grade_Four),
                  "Grade five"= mean(all_schoolna$F2021_22_Grade_Five),
                  "Grade six"= mean(all_schoolna$F2021_22_Grade_Six),
                  "Grade seven"= mean(all_schoolna$F2021_22_Grade_Seven),
                  "Grade eight"= mean(all_schoolna$F2021_22_Grade_Eight),
                  "Grade nine"= mean(all_schoolna$F2021_22_Grade_Nine),
                  "Grade ten"= mean(all_schoolna$F2021_22_Grade_Ten),
                  "Grade eleven"= mean(all_schoolna$F2021_22_Grade_Eleven),
                  "Grade twelve"= mean(all_schoolna$F2021_22_Grade_Twelve))

    p = all_schoold %>%
      ggplot(aes(x=population)) +
      geom_histogram(binwidth=500, fill="#edce95", color='white') +
      ggtitle(histTitle) +
      labs(x = "Estimated population", y = "Count") +
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
  
  # Histogram for the race topic in school districts page
  output$racehist = renderPlotly({
    population = switch(input$race_opt,
                        "American Indian Alaska" = all_schoold$F2021_22_American_Indian_Alaska,
                        "Asian" =  all_schoold$F2021_22_Asian,
                        "Native Hawaiian" = all_schoold$F2021_22_Native_Hawaiian__Pacif,
                        "Black African American"=  all_schoold$F2021_22_Black_African_American,
                        "Hispanic"=  all_schoold$F2021_22_Hispanic__Latino,
                        "White"=  all_schoold$F2021_22_White)

    histTitle = switch(input$race_opt,
                       "American Indian Alaska" = race_histTitle[1],
                       "Asian"= race_histTitle[2],
                       "Native Hawaiian"= race_histTitle[3],
                       "Black African American"= race_histTitle[4],
                       "Hispanic"= race_histTitle[5],
                       "White"= race_histTitle[6])

    meanTitle = switch(input$race_opt,
                       "American Indian Alaska" = race_meanTitle[1],
                       "Asian"= race_meanTitle[2],
                       "Native Hawaiian"= race_meanTitle[3],
                       "Black African American"= race_meanTitle[4],
                       "Hispanic"= race_meanTitle[5],
                       "White"= race_meanTitle[6])


    mean = switch(input$race_opt,
                  "American Indian Alaska" = mean(all_schoolna$F2021_22_American_Indian_Alaska),
                  "Asian" =  mean(all_schoolna$F2021_22_Asian),
                  "Native Hawaiian" = mean(all_schoolna$F2021_22_Native_Hawaiian__Pacif),
                  "Black African American"=  mean(all_schoolna$F2021_22_Black_African_American),
                  "Hispanic"=  mean(all_schoolna$F2021_22_Hispanic__Latino),
                  "White"=  mean(all_schoolna$F2021_22_White))

    p = all_schoold %>%
      ggplot(aes(x=population)) +
      geom_histogram(binwidth=500, fill="#edce95", color='white') +
      ggtitle(histTitle) +
      labs(x = "Estimated population", y = "Count") +
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
  
  

}