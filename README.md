# ASP3IRE Website
Web data visualizations for ASP3IRE

## Information
**Author**: Kathleen Ashley
**Affiliation**: Oregon State University, ASP3IRE Children's Environmental Health, College of Public Health and Human Sciences
**Principal Investigator**: Perry Hystad
**Date last modified**: January 16 2023

## **Summary**
This github repository contains the code to create an R shiny website with interactive visualizations (Map data visualization and interactive histogram) that were done through R packages. 

## **Features**
  - Data automation: loads data, creates maps and visualizations based on the data given, calculates the summary statistics for ease.
      ![alt text](app/www/documentation/automation_data.png "Raw data of Oregon Census Tracts")
      ![alt text](app/www/documentation/automation_visual.png "Interactive Visualization of census tract data using Leaflet and R shiny libraries")
  - Organize datasets: datasets are divided and organized by topics, enabling users to switch between one topic and another within a click.
      ![alt text](app/www/documentation/category_pic.png "A preview of the Oregon Census screen with the drop down menu containing organized datasets based on topics")
  - Hover-over feature: A pop-up box will dynamically appear when a user hovers over an area in the map. The information in the box will be based on the geographical area chosen and selected category. 
      ![alt text](app/www/documentation/ho_map.png "Exposure details for Oregon School tracts when hover over feature is used.")
      ![alt text](app/www/documentation/ho_closeup.png "Exposure details for Oregon School districts when hover over feature is used.")
  - Highlight-on-click feature: The highlight on click feature informs users the trend in a particular area when compared to other geographical areas in the map via a histogram. This feature also indicates where health exposures or health risks are higher in Oregon.
      ![alt text](app/www/documentation/hoc_map.png "Dynamically updated Leaflet map after highlight-on-click is performed.")
      ![alt text](app/www/documentation/hoc_histogram.png "Dynamically updated histogram after highlight-on-click is performed.")
  - Map color gradients: The colors on the map and the legend that is located on the bottom right of the map indicate the spatial distribution and environmental exposures for each location. 
    ![alt text](app/www/documentation/colorgradient.png "A preview of the map color gradients on the map and on the legend highlighted in red")
  
  ## **External Links**
  - Oregon Census Data: [insert link here]
