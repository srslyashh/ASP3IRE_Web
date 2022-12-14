library(shiny)
library(shiny.router)
#install.packages("tidyverse")
library(tidyverse)
library(shinythemes)
library(shinyjs)
library(leaflet) #interactive map
library(htmlwidgets) #labels on interactive maps
library(sf)         # spatial data - need
library(tigris)     # geojoin - need
library(raster)
library(plyr)
library(dplyr)
library(plotly)
library(fresh) # fresh is used to create a custom bootstrap application with customized font


source('webpages.r')

router <- make_router(
  route("/", home_page),
  route("censusdata", censusdata),
  route("schooldistricts", schooldistricts)
)
all_census = readRDS("all_census.RDS")

# Generates linebreaks when it is called. 
# N here refers to the amount of linebreaks
linebreaks <- function(n){HTML(strrep(br(), n))}

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


#shinyApp(ui = ui, server = server)