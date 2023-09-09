library(shiny)
library(shiny.router)
#install.packages("tidyverse")
library(tidyverse)
library(shinythemes)
library(shinyjs)
library(leaflet) #interactive map
library(htmlwidgets) #labels on interactive maps
library(htmltools)
library(sf)        
library(tigris)     
library(raster)
library(plyr)
library(dplyr)
library(plotly)
library(fresh) # fresh is used to create a custom bootstrap application with customized font
library(readr)

source('webpages.r')

# Generates linebreaks when it is called. 
# N here refers to the amount of linebreaks
linebreaks <- function(n){HTML(strrep(br(), n))}

router <- make_router(
  route("/", home_page),
  route("censusdata", censusdata),
  route("schooldistricts", schooldistricts)
)

#all_census = readRDS("all_census.RDS")
#all_schoold = readRDS("all_schooldistricts.RDS")
#all_schoolna = readRDS("all_schoolna.RDS")

# POVERTY DATA 
povertydata = c("Tot_pov_U_5_18", 
                "Tot_pov_U_5_17_18",
                "Tot_pov_U_18",
                "Tot_pov_RUC_18s")

poverty_legendT = c("Children in poverty </br> under 5 years",
                    "Children in poverty </br> from 5 to 17 years",
                    "Children in poverty </br> under 18 years",
                    "Related children of householder </br> in poverty under 18 years")

poverty_histT = c("Children in poverty under 5 years", 
                  "Children in poverty from 5 to 17 years",
                  "Children in poverty under 18 years",
                  "Related children of householder in poverty under 18 years")

poverty_meanT = c("Average total of children \n in poverty under 5 years", 
                  "Average total of children \n in poverty from 5 to 17 years",
                  "Average total of children \n in poverty under 18 years",
                  "Average total of related children \n of householder in poverty under 18 years")

grades_histTitle = c("Enrollment in 2021-2022 for Kindergarten",
                     "Enrollment in 2021-2022 for Grade One", 
                     "Enrollment in 2021-2022 for Grade Two",
                     "Enrollment in 2021-2022 for Grade Three", 
                     "Enrollment in 2021-2022 for Grade Four",
                     "Enrollment in 2021-2022 for Grade Five", 
                     "Enrollment in 2021-2022 for Grade Six",
                     "Enrollment in 2021-2022 for Grade Seven", 
                     "Enrollment in 2021-2022 for Grade Eight",
                     "Enrollment in 2021-2022 for Grade Nine", 
                     "Enrollment in 2021-2022 for Grade Ten",
                     "Enrollment in 2021-2022 for Grade Eleven", 
                     "Enrollment in 2021-2022 for Grade Twelve")

grades_meanTitle = c("Average enrollment in \n2021-2022 for Kindergarten",
                     "Average enrollment in \n2021-2022 for Grade One", 
                     "Average enrollment in \n2021-2022 for Grade Two",
                     "Average enrollment in \n 2021-2022 for Grade Three", 
                     "Average enrollment in \n2021-2022 for Grade Four",
                     "Average enrollment in \n2021-2022 for Grade Five", 
                     "Average enrollment in \n2021-2022 for Grade Six",
                     "Average enrollment in \n2021-2022 for Grade Seven", 
                     "Average enrollment in \n2021-2022 for Grade Eight",
                     "Average enrollment in \n2021-2022 for Grade Nine", 
                     "Average enrollment in \n2021-2022 for Grade Ten",
                     "Average enrollment in \n2021-2022 for Grade Eleven", 
                     "Average enrollment in \n2021-2022 for Grade Twelve")

grade_data = c("F2021_22_Kindergarten",
               "F2021_22_Grade_One", 
               "F2021_22_Grade_Two",
               "F2021_22_Grade_Three", 
               "F2021_22_Grade_Four",
               "F2021_22_Grade_Five", 
               "F2021_22_Grade_Six",
               "F2021_22_Grade_Seven", 
               "F2021_22_Grade_Eight",
               "F2021_22_Grade_Nine", 
               "F2021_22_Grade_Ten",
               "F2021_22_Grade_Eleven", 
               "F2021_22_Grade_Twelve")

race_data = c("F2021_22_American_Indian_Alaska", 
              "F2021_22_Asian",
              "F2021_22_Native_Hawaiian__Pacif",
              "F2021_22_Black_African_American",
              "F2021_22_Hispanic__Latino",
              "F2021_22_White")

race_histTitle = c("Enrollment for American Indian Alaska population", 
                   "Enrollment for Asian population",
                   "Enrollment for Native Hawaiian population",
                   "Enrollment for Black African American population",
                   "Enrollment for Hispanic population",
                   "Enrollment for White population")

race_meanTitle = c("Average enrollment for \nAmerican Indian Alaska population", 
                   "Average enrollment for \nAsian population",
                   "Native Hawaiian"= "Average enrollment for \nNative Hawaiian population",
                   "Black African American"= "Average enrollment for \nBlack African American population",
                   "Hispanic"= "Average enrollment for \nHispanic population",
                   "White"= "Average enrollment for \nWhite population")

grades_values = c("all_schoold$F2021_22_Kindergarten",
                  "all_schoold$F2021_22_Grade_One", 
                  "all_schoold$F2021_22_Grade_Two",
                  "all_schoold$F2021_22_Grade_Three", 
                  "all_schoold$F2021_22_Grade_Four",
                  "all_schoold$F2021_22_Grade_Five", 
                  "all_schoold$F2021_22_Grade_Six",
                  "all_schoold$F2021_22_Grade_Seven", 
                  "all_schoold$F2021_22_Grade_Eight",
                  "all_schoold$F2021_22_Grade_Nine", 
                  "all_schoold$F2021_22_Grade_Ten",
                  "all_schoold$F2021_22_Grade_Eleven", 
                  "all_schoold$F2021_22_Grade_Twelve")

#HEALTH COVERAGE DATA

hcdata = c("M_U_6__W_HIC", 
           "M_U_6__N_HIC",
           "M_16_18__W_HIC",
           "M_16_18__N_HIC")

healthC_legendT = c("Male under 6 years </br> with health insurance coverage", 
                    "Male under 6 years </br> without health insurance coverage",
                    "Male under 6 to 18 years </br> with health insurance coverage",
                    "Male under 6 to 18 years </br> without health insurance coverage")

healthC_histT = c("Male under 6 years with health insurance coverage", 
                  "Male under 6 years without health insurance coverage",
                  "Male under 6 to 18 years with health insurance coverage",
                  "Male under 6 to 18 years without health insurance coverage")

healthC_meanT = c("Average total of male under 6 years \n with health insurance coverage", 
                  "Average total of male under 6 years \n without health insurance coverage",
                  "Average total of male under 6 to 18 \n years with health insurance coverage",
                  "Average total of male under 6 to 18 years \n without health insurance coverage")

#HOUSEHOLD DATA

hhdata = c("Tot_hld_U_3", 
           "Tot_hld_3_4",
           "Tot_hld_5.x",
           "Tot_hld_6_8", 
           "Tot_hld_9_11",
           "Tot_hld_12_14",
           "Tot_hld_15_17")

household_legendT =c("Children in households \n under 3 years old", 
                     "Children in households \n that are 3 to 4 years old",
                     "Children in households \n that are 5 years old",
                     "Children in households \n that are 6 to 8 years old", 
                     "Children in households \n that are 9 to 11 years old",
                     "Children in households \n that are 12 to 14 years old",
                     "Children in households \n that are 15 to 17 years old")

household_histT = c("Children in households under 3 years old", 
                    "Children in households that are 3 to 4 years old",
                    "Children in households that are 5 years old",
                    "Children in households that are 6 to 8 years old", 
                    "Children in households that are 9 to 11 years old",
                    "Children in households that are 12 to 14 years old",
                    "Children in households that are 15 to 17 years old")

household_meanT = c("Average number of children \nin households under 3 years old", 
                    "Average number of children in  \nhouseholds that are 3 to 4 years old",
                    "Average number of children in  \nhouseholds that are 5 years old",
                    "Average number of children in  \nhouseholds that are 6 to 8 years old", 
                    "Average number of children in  \nhouseholds that are 9 to 11 years old",
                    "Average number of children in  \nhouseholds that are 12 to 14 years old",
                    "Average number of children in  \nhouseholds that are 15 to 17 years old")

#RESOURCE DATA

resourcedata = c("ALAND.x",
                 "AWATER.x")

resource_legendT = c( "Water resource in certain areas",
                      "Land resource in certain areas")

resource_meanT = c("Average water resource",
                   "Average land resource")

# FETCHING DATA

fetch_pd = function(args, type)
{
  all_census = readRDS("all_census.RDS")
  
  value = ""
  
  if(type=="data")
  {
    value = switch(args, 
                   "Under 5 years" = all_census$Tot_pov_U_5_18, 
                   "5 to 17 years" = all_census$Tot_pov_U_5_17_18,
                   "Under 18 years" = all_census$Tot_pov_U_18,
                   "Related children of householder under 18 years" = all_census$Tot_pov_RUC_18)
  }
  else if(type == "legend")
  {
    value = switch(args, 
                   "Under 5 years" =  poverty_legendT[1], 
                   "5 to 17 years" = poverty_legendT[2],
                   "Under 18 years" = poverty_legendT[3],
                   "Related children of householder under 18 years" = poverty_legendT[4])
  }
  else if(type == "povertydata")
  {
    value = switch(args, 
                   "Under 5 years" = povertydata[1], 
                   "5 to 17 years" = povertydata[2],
                   "Under 18 years" = povertydata[3],
                   "Related children of householder under 18 years" = povertydata[4])
  }
  
  return(value)
}

fetch_hc = function(args, type)
{
  all_census = readRDS("all_census.RDS")
  
  hc_data = ""
  
  if(type == "data")
  {
    hc_data = switch(args, 
                    "Male under 6 years with health insurance coverage" = all_census$M_U_6__W_HIC, 
                    "Male under 6 years without health insurance coverage"= all_census$M_U_6__N_HIC,
                    "Male under 6 to 18 years with health insurance coverage" = all_census$M_16_18__W_HIC,
                    "Male under 6 to 18 years without health insurance coverage"= all_census$M_16_18__N_HIC)
  }
  else if(type == "legend")
  {
    hc_data = switch(args, 
                    "Male under 6 years with health insurance coverage" = healthC_legendT[1], 
                    "Male under 6 years without health insurance coverage"= healthC_legendT[2],
                    "Male under 6 to 18 years with health insurance coverage"= healthC_legendT[3],
                    "Male under 6 to 18 years without health insurance coverage"= healthC_legendT[4])
  }
  else if(type == "hcdata")
  {
    hc_data = switch(args, 
                     "Male under 6 years with health insurance coverage" = hcdata[1], 
                     "Male under 6 years without health insurance coverage"= hcdata[2],
                     "Male under 6 to 18 years with health insurance coverage" = hcdata[3],
                     "Male under 6 to 18 years without health insurance coverage"= hcdata[4])
  }
  
  return(hc_data)
}

fetch_hha = function(args, type)
{
  all_census = readRDS("all_census.RDS")
  
  data = ""
  
  if(type == "data")
  {
    data = switch(args, 
           "Children in households under 3 years old" = all_census$Tot_hld_U_3, 
           "Children in households that are 3 to 4 years old" = all_census$Tot_hld_3_4,
           "Children in households that are 5 years old" = all_census$Tot_hld_5.x,
           "Children in households that are 6 to 8 years old"= all_census$Tot_hld_6_8, 
           "Children in households that are 9 to 11 years old"= all_census$Tot_hld_9_11,
           "Children in households that are 12 to 14 years old"= all_census$Tot_hld_12_14,
           "Children in households that are 15 to 17 years old"= all_census$Tot_hld_15_17)
  }
  else if(type == "legend")
  {
    data = switch(args, 
                  "Children in households under 3 years old" = household_legendT[1], 
                  "Children in households that are 3 to 4 years old"= household_legendT[2],
                  "Children in households that are 5 years old" = household_legendT[3],
                  "Children in households that are 6 to 8 years old"= household_legendT[4], 
                  "Children in households that are 9 to 11 years old" = household_legendT[5],
                  "Children in households that are 12 to 14 years old" = household_legendT[6],
                  "Children in households that are 15 to 17 years old" = household_legendT[7])
  }
  else if(type == "hhdata")
  {
    data = switch(args, 
                  "Children in households under 3 years old" = hhdata[1], 
                  "Children in households that are 3 to 4 years old" = hhdata[2],
                  "Children in households that are 5 years old" = hhdata[3],
                  "Children in households that are 6 to 8 years old"= hhdata[4], 
                  "Children in households that are 9 to 11 years old"= hhdata[5],
                  "Children in households that are 12 to 14 years old"= hhdata[6],
                  "Children in households that are 15 to 17 years old"= hhdata[7])
  }
  
  return(data)
}

fetch_hist = function(args, data, type)
{
  all_census = readRDS("all_census.RDS")
  #view(all_census)
  
  returnVal = ""
  
  #print("fetch_hist() function")
    
  if(data == "poverty")
  {
    if(type == "population")
    {
      returnVal =  switch(args, 
                  "Under 5 years" = all_census$Tot_pov_U_5_18, 
                  "5 to 17 years" = all_census$Tot_pov_U_5_17_18,
                  "Under 18 years" = all_census$Tot_pov_U_18,
                  "Related children of householder under 18 years" = all_census$Tot_pov_RUC_18)
    }
    else if(type == "histTitle")
    {
      returnVal = switch(args, 
                         "Under 5 years" =  poverty_histT[1], 
                         "5 to 17 years" = poverty_histT[2],
                         "Under 18 years" = poverty_histT[3],
                         "Related children of householder under 18 years" = poverty_histT[4])
    }
    else if(type == "meanTitle")
    {
      returnVal = switch(args, 
                       "Under 5 years" =  poverty_meanT[1], 
                       "5 to 17 years" = poverty_meanT[2],
                       "Under 18 years" = poverty_meanT[3],
                       "Related children of householder under 18 years" = poverty_meanT[4])
    }
    else if(type == "mean")
    {
      print(args)
      
      returnVal = switch(args, 
                "Under 5 years" = mean(all_census$Tot_pov_U_5_18), 
                "5 to 17 years" = mean(all_census$Tot_pov_U_5_17_18),
                "Under 18 years" = mean(all_census$Tot_pov_U_18),
                "Related children of householder under 18 years" = mean(all_census$Tot_pov_RUC_18))
      
      #print("=====RETURNVAL, mean")
      #print(returnVal)
      #view(all_census)
    }
  }
  else if(data == "hc")
  {
    if(type == "population")
    {
      returnVal =  switch(args, 
                          "Male under 6 years with health insurance coverage" = all_census$M_U_6__W_HIC, 
                          "Male under 6 years without health insurance coverage"= all_census$M_U_6__N_HIC,
                          "Male under 6 to 18 years with health insurance coverage" = all_census$M_16_18__W_HIC,
                          "Male under 6 to 18 years without health insurance coverage"= all_census$M_16_18__N_HIC)
    }
    else if(type == "histTitle")
    {
      returnVal = switch(args, 
                         "Male under 6 years with health insurance coverage" = healthC_histT[1], 
                         "Male under 6 years without health insurance coverage"= healthC_histT[2],
                         "Male under 6 to 18 years with health insurance coverage"= healthC_histT[3],
                         "Male under 6 to 18 years without health insurance coverage"= healthC_histT[4])
    }
    else if(type == "meanTitle")
    {
      returnVal = switch(args, 
                         "Male under 6 years with health insurance coverage" = healthC_meanT[1], 
                         "Male under 6 years without health insurance coverage"= healthC_meanT[2],
                         "Male under 6 to 18 years with health insurance coverage"= healthC_meanT[3],
                         "Male under 6 to 18 years without health insurance coverage"= healthC_meanT[4])
    }
    else if(type == "mean")
    {
      returnVal = switch(args, 
                         "Male under 6 years with health insurance coverage" = mean(all_census$M_U_6__W_HIC), 
                         "Male under 6 years without health insurance coverage"= mean(all_census$M_U_6__N_HIC),
                         "Male under 6 to 18 years with health insurance coverage" = mean(all_census$M_16_18__W_HIC),
                         "Male under 6 to 18 years without health insurance coverage"= mean(all_census$M_16_18__N_HIC))
    }
  }
  else if(data == "hha")
  {
    if(type == "population")
    {
      returnVal =  switch(args, 
                          "Children in households under 3 years old" = all_census$Tot_hld_U_3, 
                          "Children in households that are 3 to 4 years old" = all_census$Tot_hld_3_4,
                          "Children in households that are 5 years old" = all_census$Tot_hld_5.x,
                          "Children in households that are 6 to 8 years old"= all_census$Tot_hld_6_8, 
                          "Children in households that are 9 to 11 years old"= all_census$Tot_hld_9_11,
                          "Children in households that are 12 to 14 years old"= all_census$Tot_hld_12_14,
                          "Children in households that are 15 to 17 years old"= all_census$Tot_hld_15_17)
    }
    else if(type == "histTitle")
    {
      returnVal = switch(args, 
                         "Children in households under 3 years old" = household_histT[1], 
                         "Children in households that are 3 to 4 years old"= household_histT[2],
                         "Children in households that are 5 years old" = household_histT[3],
                         "Children in households that are 6 to 8 years old"= household_histT[4], 
                         "Children in households that are 9 to 11 years old" = household_histT[5],
                         "Children in households that are 12 to 14 years old" = household_histT[6],
                         "Children in households that are 15 to 17 years old" = household_histT[7])
    }
    else if(type == "meanTitle")
    {
      returnVal = switch(args, 
                         "Children in households under 3 years old" = household_meanT[1], 
                         "Children in households that are 3 to 4 years old"= household_meanT[2],
                         "Children in households that are 5 years old" = household_meanT[3],
                         "Children in households that are 6 to 8 years old"= household_meanT[4], 
                         "Children in households that are 9 to 11 years old" = household_meanT[5],
                         "Children in households that are 12 to 14 years old" = household_meanT[6],
                         "Children in households that are 15 to 17 years old" = household_meanT[7])
    }
    else if(type == "mean")
    {
      returnVal = switch(args, 
                         "Children in households under 3 years old" = mean(all_census$Tot_hld_U_3), 
                         "Children in households that are 3 to 4 years old" = mean(all_census$Tot_hld_3_4),
                         "Children in households that are 5 years old" = mean(all_census$Tot_hld_5.x),
                         "Children in households that are 6 to 8 years old"= mean(all_census$Tot_hld_6_8), 
                         "Children in households that are 9 to 11 years old"= mean(all_census$Tot_hld_9_11),
                         "Children in households that are 12 to 14 years old"= mean(all_census$Tot_hld_12_14),
                         "Children in households that are 15 to 17 years old"= mean(all_census$Tot_hld_15_17))
    }
  }
  else if(data == "resources")
  {
    if(type == "population")
    {
      returnVal =  switch(args, 
                          "Water"= all_census$ALAND.x,
                          "Land" = all_census$AWATER.x)
    }
    else if(type == "histTitle")
    {
      returnVal = switch(args, 
                         "Water"= resource_legendT[1],
                         "Land" = resource_legendT[2])
    }
    else if(type == "meanTitle")
    {
      returnVal = switch(args, 
                         "Water" = resource_meanT[1],
                         "Land" = resource_meanT[2])
    }
    else if(type == "mean")
    {
      returnVal = switch(args, 
                         "Water"= mean(all_census$ALAND.x),
                         "Land" = mean(all_census$AWATER.x))
    }
  }
  
  return(returnVal)
}

fetch_resources = function(args, type)
{
  all_census = readRDS("all_census.RDS")
  
  rdata = ""
  
  if(type == "data")
  {
    rdata = switch(args, 
                   "Water"= all_census$ALAND.x,
                   "Land" = all_census$AWATER.x)
  }
  else if(type == "legend")
  {
    rdata = switch(args, 
                   "Water"= resource_legendT[1],
                   "Land" = resource_legendT[2])
  }
  else if(type == "resourcesdata")
  {
    rdata = switch(args, 
                   "Water"= resourcedata[1],
                   "Land" = resourcedata[2])
  }
  
  return(rdata)
}

#-------------------------------------------------------------------------------
# generateListItems
#-------------------------------------------------------------------------------
# >Purpose: 
#     This function is designed to create an HTML unordered list (<ul>)
#     containing a list of items with clickable links. It takes two arrays ('array' and 'linksArr') as inputs
#     and generates a structured list with hyperlinked items.
#
# Arguments:
# - array: An array containing the names or labels of the items to be displayed.
# - linksArr: An array containing the URLs or links corresponding to each item in the list.
#
# * This was used to create the Resources on the environmental variable topic of
#   the census data page.
#-------------------------------------------------------------------------------
generateListItems <- function(array, linksArr) {
  tags$ul(class="bullet-points",
    lapply(seq_along(array), function(i) {
      tags$li(class="bullet-li",
        tags$a(class="blue-link",href = linksArr[i], array[i])
      )
    })
  )
}

#-------------------------------------------------------------------------------
# generateWebText
#-------------------------------------------------------------------------------
# > Purpose: 
#    Generates an HTML element based on the type that is given. 
#
# > Arguments:
# - type : This type are the variables listed in the Webpage_ASPIRE.csv file (which are:
#          Mission_Text, Project_1_Title, Project_2_Title, Project_3_Title, Project_4_Title, 
#.         Project_1_Desc, Project_2_Desc, Project_3_Desc, Project_4_Desc)
# - text : The text that is supposed to be put in the element
#-------------------------------------------------------------------------------
generateWebText <- function(type, text) {
  if (type == "Mission_Text") {
    return(
      tags$p(class = "normal-p", text)
    )
  }
  if (type == "Project_1_Title" || type == "Project_2_Title" || type == "Project_3_Title" || type == "Project_4_Title") 
  {
    # We're routing the titles to each perspective webpages. route_link will automatically link the webpages with 
    # the webpages assigned from webpages.R.
    if(type == "Project_1_Title")
    {
      return(
        tags$h3(class = "flexbox-title",
                tags$a(class = "flexbox-title-link", href = route_link("censusdata"), text))
      )
    }
    if(type == "Project_2_Title")
    {
      return(
        tags$h3(class = "flexbox-title",
                tags$a(class = "flexbox-title-link", href = route_link("schooldistricts"), text))
      )
    }
    if(type == "Project_3_Title" || type == "Project_4_Title" )
    {
      return(
        tags$h3(class = "flexbox-title", text)
      )
    }
  }
  
  if(type == "Project_1_Desc" || type == "Project_2_Desc" || type == "Project_3_Desc" || type == "Project_4_Desc")
  {
    return(
      tags$p(class = "flexbox-p", text)
    )
  }
}
#========================================================================================================
# fetch_text_sd
#------------------------------------
# Purpose:
# fetch_text_sd function retrieves specific information from the "SchoolDistricts_ASPIRE_Collapsed.csv" dataset
# based on the specified type and input type (indicator).
# Parameters:
#   - type: A character string indicating the type of information to fetch (e.g., "Topic_Desc", "Indicator_Title").
#       (type can only be the indicators that are already stated in SchoolDistricts_ASPIRE_Collapsed.csv file).
#   - inp_type: A character string specifying the input type (indicator) to filter the dataset.
#       (inpt_type is based on the selectInput function in webpages.R, if it's poverty status, then the 
#         inp_type would be input$age, if it's health insurance coverage, it'd be input$hc_age, etc).
# Returns:
#   - A character string containing the fetched information based on the specified type.
#========================================================================================================
fetch_text_sd = function(type, inp_type)
{
  oregon_sd = read_csv("SchoolDistricts_ASPIRE_Collapsed.csv")
  indicator = inp_type
  sd = oregon_sd[oregon_sd$Indicator == indicator, ]
  
  value = ""
  if(type == "Topic_Desc")
  {
    value = sd$Topic_Desc
  }
  if(type == "Indicator_Title")
  {
    value = sd$Indicator_Title
  }
  if(type == "Indicator_Desc")
  {
    value = sd$Indicator_Desc
  }
  if(type == "Image_url")
  {
    value = sd$Image_url
  }
  
  return(value)
}

#-------------------------------------------------------------------------------
# generateSchoolUI
#-------------------------------------------------------------------------------
# > Purpose: 
#    Generates an HTML element based on the type that is given. 
#
# > Arguments:
# - type : Type could be either Indicator_Desc or Topic_Desc (look at the documentation)
#           for which variable would be located in the website.
# - text : The text that is supposed to be put in the element
#
# > Return Value:
#   Based on the type, the css that is applied to the response would be different.
#-------------------------------------------------------------------------------
generateSchoolUI <- function(type, text) {
  if (type == "Topic_Desc") {
    return(
      tags$p(class = "topic-p topic-p-1",
           text  
      )
    )
  }
  if(type == "Indicator_Desc")
  {
    return(
      tags$p(class = "topic-p",
             text  
      )
    )
  }
}

#-------------------------------------------------------------------------------
# generatePic
#-------------------------------------------------------------------------------
# > Purpose: 
#    Generates an HTML image element (<img>).
#    It takes a 'photo_url' as input and sets the 'src' attribute of the image to the provided URL.
#    Additionally, it specifies the 'height' and 'class' attributes for styling purposes.
#
# > Arguments:
# - photo_url: A URL string specifying the source of the image to be displayed.
#-------------------------------------------------------------------------------
generatePic <- function(photo_url)
{
  tags$img(src = photo_url, height = 170, class = "img-topic")
}

#-------------------------------------------------------------------------------
# generatePoliciesItems
#-------------------------------------------------------------------------------
# > Purpose: 
#    Generates policy items that is located in the environmental variable on
#    the census data page. It takes three arrays ('array,' 'linksArr,' and 'descArr') as inputs
#    and constructs a structured list with clickable links and item descriptions.
#
# > Arguments:
# - array: An array containing the names or labels of policy items.
# - linksArr: An array containing the URLs for the policy item links.
# - descArr: An array containing descriptions or details for each policy item.
#-------------------------------------------------------------------------------
generatePoliciesItems <- function(array, linksArr, descArr) {
  tags$ul(class = "bullet-points",
          lapply(seq_along(array), function(i) {
            tags$li(class = "bullet-li",
                    tags$a(class = "blue-link", href = linksArr[i], array[i]),
                    tags$ul(
                      tags$li(class="topic-li", descArr[i])
                    )
            )
          })
  )
}

#-------------------------------------------------------------------------------
# leafletMap
#-------------------------------------------------------------------------------
# Creates leaflet map for these topics: 
# health insurance coverage, poverty status, and resources (Census Data)
#-------------------------------------------------------------------------------
leafletMap = function(data, legendTitle)
{
  all_census = readRDS("all_census.RDS")
  # divides the color palette into 9 colors.
  pal = colorBin(palette="OrRd", bins = 5, domain= data,  pretty = TRUE)
  
  # the labels on top of the map when hovered over
  labels = sprintf("<strong>%s<strong><br/>Total population: %g",
                   all_census$Geographic_Area_Name, data) %>%
    lapply(htmltools::HTML)
  
  l_map = all_census %>% 
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
                fillColor = ~pal(data),
                layerId = all_census$GEOID,
                highlightOptions = highlightOptions(weight=2,
                                                    fillOpacity=1, 
                                                    color="black",
                                                    opacity=0.5,
                                                    bringToFront= TRUE)) %>%
    addLegend("bottomright", 
              pal=pal,
              values = ~data,
              title = legendTitle,
              opacity = 0.7)
  
  
  return(l_map)
}

#-------------------------------------------------------------------------------
# fethc_histogram
#-------------------------------------------------------------------------------
# Fetches the histogram for the topics listed below: 
# health insurance coverage, poverty status, and resources (Census Data) 
#-------------------------------------------------------------------------------
fetch_histogram = function(population, histTitle, meanTitle, mean, arg="normal")
{
  bin = 0
  all_census = readRDS("all_census.RDS")
  
  if(arg == "smallBin")
  {
    bin = 5
  }
  else if(arg == "normal")
  {
    bin = 100
  }
  
  h = all_census %>% 
      ggplot(aes(x=population)) + 
      geom_histogram(binwidth=bin, fill='#edce95', color='white') + 
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
  
  return(h)
}

fetch_observe_vars= function(args, type, data_title, id, dataset)
{
  all_census = readRDS("all_census.RDS")
  value = ""
  
  if(type == "poverty")
  {
    holder = all_census[, c(dataset, "GEOID")]
    
    # filter the data based on the id of the map that was clicked.
    data = holder[holder$GEOID == id, ]
    
    if(data_title == "variable")
    {
      value = switch(args, 
                     "Children in households under 3 years old" = holder$Tot_hld_U_3, 
                     "Children in households that are 3 to 4 years old" = holder$Tot_hld_3_4,
                     "Children in households that are 5 years old" = holder$Tot_hld_5.x,
                     "Children in households that are 6 to 8 years old"= holder$Tot_hld_6_8, 
                     "Children in households that are 9 to 11 years old"= holder$Tot_hld_9_11,
                     "Children in households that are 12 to 14 years old"= holder$Tot_hld_12_14,
                     "Children in households that are 15 to 17 years old"= holder$Tot_hld_15_17)
    }
    else if(data_title == "overalldata")
    {
      value = switch(args, 
                     "Children in households under 3 years old" = data$Tot_hld_U_3, 
                     "Children in households that are 3 to 4 years old" = data$Tot_hld_3_4,
                     "Children in households that are 5 years old" = data$Tot_hld_5.x,
                     "Children in households that are 6 to 8 years old"= data$Tot_hld_6_8, 
                     "Children in households that are 9 to 11 years old"= data$Tot_hld_9_11,
                     "Children in households that are 12 to 14 years old"= data$Tot_hld_12_14,
                     "Children in households that are 15 to 17 years old"= data$Tot_hld_15_17)
    }
  }
  else if(type == "hc")
  {
    holder = all_census[, c(dataset, "GEOID")]
    
    # filter the data based on the id of the map that was clicked.
    data = holder[holder$GEOID == id, ]
    
    if(data_title == "variable")
    {
      value = switch(args, 
                     "Male under 6 years with health insurance coverage" = holder$M_U_6__W_HIC, 
                     "Male under 6 years without health insurance coverage"= holder$M_U_6__N_HIC,
                     "Male under 6 to 18 years with health insurance coverage" = holder$M_16_18__W_HIC,
                     "Male under 6 to 18 years without health insurance coverage"= holder$M_16_18__N_HIC)
    }
    else if(data_title == "overalldata")
    {
      value = switch(args, 
                     "Male under 6 years with health insurance coverage" = data$M_U_6__W_HIC, 
                     "Male under 6 years without health insurance coverage"= data$M_U_6__N_HIC,
                     "Male under 6 to 18 years with health insurance coverage" = data$M_16_18__W_HIC,
                     "Male under 6 to 18 years without health insurance coverage"= data$M_16_18__N_HIC)
    }
  }
  else if(type == "hha")
  {
    holder = all_census[, c(dataset, "GEOID")]
    
    # filter the data based on the id of the map that was clicked.
    data = holder[holder$GEOID == id, ]
    
    if(data_title == "variable")
    {
      value = switch(args, 
                     "Male under 6 years with health insurance coverage" = holder$M_U_6__W_HIC, 
                     "Male under 6 years without health insurance coverage"= holder$M_U_6__N_HIC,
                     "Male under 6 to 18 years with health insurance coverage" = holder$M_16_18__W_HIC,
                     "Male under 6 to 18 years without health insurance coverage"= holder$M_16_18__N_HIC)
    }
    else if(data_title == "overalldata")
    {
      value = switch(args, 
                     "Male under 6 years with health insurance coverage" = data$M_U_6__W_HIC, 
                     "Male under 6 years without health insurance coverage"= data$M_U_6__N_HIC,
                     "Male under 6 to 18 years with health insurance coverage" = data$M_16_18__W_HIC,
                     "Male under 6 to 18 years without health insurance coverage"= data$M_16_18__N_HIC)
    }
  }
  else if(type == "resources")
  {
    holder = all_census[, c(dataset, "GEOID")]
    
    # filter the data based on the id of the map that was clicked.
    data = holder[holder$GEOID == id, ]
    
    if(data_title == "variable")
    {
      value = switch(args, 
                     "Water"= holder$ALAND.x,
                     "Land" = holder$AWATER.x)
    }
    else if(data_title == "overalldata")
    {
      value = switch(args, 
                     "Water"= data$ALAND.x,
                     "Land" = data$AWATER.x)
    }
  }
  
  return(value)
}

#------------------------------------------------------------------------------------------------------------------
# fetch_text_wildfire
#------------------------------------------------------------------------------------------------------------------
# This was created for the environment variables section of Census Data. 
#  - Any variables associated here are already documented in the documentation
#   underneath the third section which is Format and Screenshots of Variables. 
# > Purpose: 
#    This fetches texts from the file "Resources_ASPIRE_Collapsed.csv" 
#    and uses the texts to dynamically populate the website.
# > Parameters:
#   - type: Character string indicating the type of data to retrieve ("summary", "title", "url", "resources", "policies", "localResources").
#   - data: Character string specifying additional data retrieval ("num", "names", "links", "desc") 
#     based on the selected type (see note below)
#   - inp_type: Character string specifying the indicator to filter the data.
# > Returns:
#   - A character string containing the requested data based on the specified type and data.
# > Example:
#   If you're trying to fetch Resource_Names for Wildfire Smoke, this is the call you'd make:
#       array_name = as.array(fetch_text_wildfire("localResources", "names", input$env_var))
#   If you're trying to fetch Policies Links for Well Water Contamination, this is the call:
#       array_name = as.array(fetch_text_wildfire("policies", "names", input$env_var))
#   If you're trying to get the img_url for an indicator, do this:
#       img_url = fetch_text_wildfire("url", "", input$env_var)
# *note:
#   > Since some of the variables (Resource_Names, Resource_Links, Oregon_Program_Name has a list of 
#     sentences, this function would return as an array which is why the examples above require as.array()
#     wrapping the response. Otherwise, the function could be called by doing simply fetch_text_wildfire() 
#     without as.array() wrapping it.
#   > Depending on the call you're trying to make, the parameter "data" can either be "names", "links", "desc" or "num".
#     at times when you don't need it (the example of the photo_url or summary), you could put "" or "0" on the 
#     data parameter.
#------------------------------------------------------------------------------------------------------------------
fetch_text_wildfire = function(type, data, inp_type)
{
  oregonr_csv <- read_csv("Resources_ASPIRE_Collapsed.csv")
  indicator = inp_type
  oregon_resources = oregonr_csv[oregonr_csv$Indicator == indicator, ]
  
  value = ""
  if(type == "summary")
  {
    value = oregon_resources$Summary
  }
  if(type == "title")
  {
    value = indicator
  }
  if(type == "url")
  {
    value = oregon_resources$Image_url
  }
  if(type == "resources")
  {
    if(data == "num")
    {
      value = oregon_resources$Resources_Num
    }
    if(data == "names")
    {
      value = readLines(textConnection(oregon_resources$Resource_Names))
    }
    if(data == "links")
    {
      value = readLines(textConnection(oregon_resources$Resource_Links))
    }
  }
  if(type == "policies")
  {
    if(data == "num")
    {
      value = oregon_resources$Policies_Num
    }
    if(data == "names")
    {
      value = readLines(textConnection(oregon_resources$Policies_Titles))
    }
    if(data == "links")
    {
      value = readLines(textConnection(oregon_resources$Policies_Links))
    }
    if(data == "desc")
    {
      value = readLines(textConnection(oregon_resources$Policies_Desc))
    }
  }
  if(type == "localResources")
  {
    if(data == "num")
    {
      value = oregon_resources$LR_Num
    }
    if(data == "names")
    {
      value =  readLines(textConnection(oregon_resources$LR_Name))
    }
    if(data == "links")
    {
      value = readLines(textConnection(oregon_resources$LR_Links))
    }
    if(data == "desc")
    {
      value = readLines(textConnection(oregon_resources$LR_Desc))
    }
  }
  return(value)
}

#-------------------------------------------------------------------------------
# fetch_text_webpage
#-------------------------------------------------------------------------------
# This was created for the homepage of ASPIRE website.
# - Any variables associated here are already documented in the documentation
#   underneath the third section which is Format and Screenshots of Variables. 
# > Purpose:
#   This function fetches texts from the file "Webpage_ASPIRE.csv" 
#   and uses the texts to dynamically populate the website.
# > Arguments:
#   - type: type here should be any variables that is listed under "Variable"
#     in the Webpage_ASPIRE.csv. It is case-sensitive and has to be exactly the same.
#   - if you're wanting to fetch the text variable for Mission_Text, 
#     call this function with fetch_text_webpage("Mission_Text"). 
#-------------------------------------------------------------------------------
fetch_text_webpage <- function(type) {
  # Load the CSV file
  webpage_text <- read.csv("Webpage_ASPIRE.csv")
  
  oregon_wp = webpage_text[webpage_text$Variable == type, ]
  
  return(oregon_wp$Text_Variable)
}

#-------------------------------------------------------------------------------
# fetch_text_census
#-------------------------------------------------------------------------------
# This was created for the Census Data page of the ASPIRE website. 
# - Any variables associated here are already documented in the documentation
#   underneath the third section which is Format and Screenshots of Variables. 
# > Purpose:
#   This function fetches texts from the file "Census_ASPIRE_Collapsed.csv" 
#   and uses the texts to dynamically populate the website.
# > Some topics associated here:
#   > Resources
#   > Population in Household by Age
#   > Poverty Status
#   > Health Insurance Coverage
# Parameters:
#   - type: Character string indicating the type of data to retrieve 
#     ("url", "summary", "Topic_Desc", "Indicator_Title").
#   - data: Data frame containing the census data (just use "0" for this, it was
#     used for some errors in the file)
#   - inp_type: Character string specifying the indicator to filter the data
#     (inp_type is based on the name you put on selectInput in webpages.R, 
#     if you're trying to fetch data based on poverty status, use input$age, and so on).
# Returns:
#   - A character string containing the requested data based on the specified type.
#-------------------------------------------------------------------------------
fetch_text_census = function(type, data, inp_type)
{
  oregonr_csv <- read_csv("Census_ASPIRE_Collapsed.csv")
  indicator = inp_type
  oregon_resources = oregonr_csv[oregonr_csv$Indicator == indicator, ]
  
  value = ""
  if(type == "url")
  {
    value = oregon_resources$Image_url
  }
  if(type == "summary")
  {
    value = oregon_resources$Summary
  }
  if(type == "Topic_Desc")
  {
    value = oregon_resources$Topic_Desc
  }
  if(type == "Indicator_Title")
  {
    value = oregon_resources$Indicator_Title
  }
  return(value)
}

#-------------------------------------------------------------------------------
# generateCensusUI
#-------------------------------------------------------------------------------
# > Purpose: 
#    Generates an HTML element based on the type that is given. 
#
# > Arguments:
# - type : Type could be either Indicator_Desc or Topic_Desc
# - text : The text that is supposed to be put in the element
#-------------------------------------------------------------------------------
generateCensusUI <- function(type, text) {
  if (type == "Topic_Desc") {
    return(
      tags$p(class = "topic-p topic-p-1",
             text  
      )
    )
  }
  if(type == "summary")
  {
    return(
      tags$p(class = "topic-p", text
      )
    )
  }
}

#-------------------------------------------------------------------------------
# turn_to_list
#-------------------------------------------------------------------------------
# Purpose: 
#   This custom R function, 'turn_to_list,' is designed to convert data into an unordered list
#   (<ul>) format based on the provided 'args.'
#-------------------------------------------------------------------------------
turn_to_list = function(args)
{
  ul= tags$ul()
  
  if(args == "resources")
  {
    oregon_r_names = fetch_text_wildfire("resources", "names")
    oregon_r_links = fetch_text_wildfire("resources", "links")
    
    ul$children = lapply(oregon_r_names, function(x) {
      tags$li(oregon_r_names[index])
    })
  }
  
  return(ul)
}

fetch_sd_leaflet_var = function(args, type, dataTitle)
{
  all_schoold = readRDS("all_schooldistricts.RDS")
  value = ""
  
  if(type == "grades")
  {
    if(dataTitle == "gradedata")
    {
      value = switch(args, 
                     "Kindergarten"= all_schoold$F2021_22_Kindergarten,
                     "Grade one"=  all_schoold$F2021_22_Grade_One, 
                     "Grade two"= all_schoold$F2021_22_Grade_Two,
                     "Grade three"= all_schoold$F2021_22_Grade_Three, 
                     "Grade four"=  all_schoold$F2021_22_Grade_Four,
                     "Grade five"= all_schoold$F2021_22_Grade_Five, 
                     "Grade six"= all_schoold$F2021_22_Grade_Five,
                     "Grade seven"= all_schoold$F2021_22_Grade_Seven, 
                     "Grade eight"= all_schoold$F2021_22_Grade_Eight,
                     "Grade nine"= all_schoold$F2021_22_Grade_Nine, 
                     "Grade ten"= all_schoold$F2021_22_Grade_Ten,
                     "Grade eleven"= all_schoold$F2021_22_Grade_Eleven, 
                     "Grade twelve"= all_schoold$F2021_22_Grade_Twelve)
    }
    else if(type == "legendTitle")
    {
      value = switch(args, 
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
    }
  }
  else if(type == "race")
  {
    if(dataTitle == "racedata")
    {
      value = switch(args, 
                     "American Indian Alaska" = all_schoold$F2021_22_American_Indian_Alaska, 
                     "Asian" =  all_schoold$F2021_22_Asian,
                     "Native Hawaiian" = all_schoold$F2021_22_Native_Hawaiian__Pacif,
                     "Black African American"=  all_schoold$F2021_22_Black_African_American,
                     "Hispanic"=  all_schoold$F2021_22_Hispanic__Latino,
                     "White"=  all_schoold$F2021_22_White)
    }
    else if(dataTitle == "legendTitle")
    {
      value = switch(args, 
                     "American Indian Alaska" = race_histTitle[1], 
                     "Asian"= race_histTitle[2],
                     "Native Hawaiian"= race_histTitle[3],
                     "Black African American"= race_histTitle[4],
                     "Hispanic"= race_histTitle[5],
                     "White"= race_histTitle[6])
    }
  }
  
  return(value)
}

#-------------------------------------------------------------------------------
# fetch_sd_leaflet_map
#-------------------------------------------------------------------------------
# > Purpose:
#   This function creates an interactive leaflet map visualization
#   displaying data for each school district, with population-based color coding.
#   This function reads school district geometries from the "all_schooldistricts.RDS" file and
#   requires the Leaflet and sf libraries.
# > Parameters:
#   - data: Numeric vector containing the data values to be displayed on the map.
#   - legendTitle: Character string specifying the title for the map legend.
# > Returns:
#   - An interactive leaflet map displaying school district data, color-coded by population.
#-------------------------------------------------------------------------------

fetch_sd_leaflet_map = function(data, legendTitle)
{
  # fetch all_schoold data:
  all_schoold = readRDS("all_schooldistricts.RDS")
  #view(all_schoold)

  # 1. making the label for the map:
  labels = sprintf("<strong>%s<strong><br/>Total population: %g",
                   all_schoold$NAME.x, data) %>%
    lapply(htmltools::HTML)

  # 2. Making the color palette, dividing it into 9 categories.
  pal = colorBin(palette="OrRd", 5, domain = data, pretty = TRUE)

  # Making the map for the particular data given:
  map_interactive = all_schoold %>%
    st_transform(crs = "EPSG:4326") %>%
    leaflet() %>%
    addProviderTiles(provider="CartoDB.Positron") %>%
    addPolygons(label = labels,
                stroke = TRUE,
                color= "#d6d5de",
                weight= 1,
                smoothFactor = .5,
                opacity = 0.5,
                fillOpacity = 0.7,
                fillColor = ~pal(data),
                layerId = all_schoold$GEOID,
                highlightOptions = highlightOptions(weight=2,
                                                    fillOpacity=1,
                                                    color="black",
                                                    opacity=0.5,
                                                    bringToFront= TRUE)) %>%
    addLegend("bottomright",
              pal=pal,
              values = data,
              title = legendTitle,
              opacity = 0.7)
}
#shinyApp(ui = ui, server = server)