
library(shiny)
library(tidyverse)
library(readr)
library(gifski)
library(gganimate)
library(mapview)
library(leaflet)
library(geojsonio)
library(lubridate)
library(leaflet.extras)
library(htmltools)

library(png)
library(dygraphs)
library(xts)         
library(stringr)
library(DT)
library(here)
library(rgdal)
library(tmaptools)


ui <- navbarPage("Crime in Boston",
                 
                 tabPanel("Map",
                          sidebarLayout(
                            sidebarPanel(
                              checkboxGroupInput("UCR_type", label = "Type of Crime", 
                                                 choices = list("Serious Crime" = "Part One", 
                                                                "Less Serious" = "Part Two", 
                                                                "Least Serious" = "Part Three"),
                                                 selected = c("Part One")),
                              
                              checkboxGroupInput("crime_type", label = "Or, select specific crime types: ", 
                                                 choices = list("Homicide" = "Homicide",
                                                                "Robbery" = "Robbery", 
                                                                "Burglary" = "Burglary",
                                                                "Larceny" = "Larceny",
                                                                "Aggrevated Assault" = "Aggrevated Assault",
                                                                "Drug Violation" = "Drug Violation",
                                                                "Other" = "Other"),
                                                 selected = c()),
                              
                              checkboxGroupInput("popup_info", label = "Information Shown When Clicked", 
                                                 choices = list("Type of Crime" = 1, 
                                                                "Time" = 2, 
                                                                "Street" = 3,
                                                                "Police District" = 4),
                                                 selected = c(1)),
                              
                              dateInput("date", label = "Select Date", value = ymd("2018-01-01"), 
                                        min = ymd("2012-07-08"), max = ymd("2018-11-17"))
                              
                            ),
                            mainPanel(
                              leafletOutput("map_coordinates"),
                              h3("Ever wonder how close you were to a crime?"),
                              p(em("Maybe a murder or theft happened very close to you, but you never knew.")),
                              p(strong("Find out if any crime happened near you on a selected day"), " (New Year? Valentine's?) using the map above, which displays ", strong("real-life crimes in Boston")),
                              p("See more information of an individual crime when you click on a marker.
                                Choose the date, information to display, and crimes to include on the sidebar.")
                              )
                            
                            )
                 ),
                 
                 tabPanel("Heatmap",
                          sidebarLayout(
                            sidebarPanel(
                              checkboxGroupInput("heat_UCR_type", label = "Type of Crime", 
                                                 choices = list("Serious Crime" = "Part One", 
                                                                "Less Serious" = "Part Two", 
                                                                "Least Serious" = "Part Three"),
                                                 selected = c("Part One")),
                              
                              dateRangeInput("heat_dates", label = "Select Date Range", 
                                             start = ymd("2015-06-15"), end = ymd("2018-11-17"),
                                             min = ymd("2015-06-15"), max = ymd("2018-11-17")),
                              
                              sliderInput("size", label = "Size", min = 20, max = 500, value = 100)
                              
                            ),
                            mainPanel(
                              leafletOutput("heatmap"),
                              h3("Do you live in a dangerous neighborhood?"),
                              h3("What areas are the most dangerous in Boston?"),
                              p("Find out with this ", strong("heatmap,"), " which shows areas with highest concentrations of crime"),
                              p("Select type of crime, date range, and adjust circle sizes of the map using the sidebar")
                            )        
                            
                          )
                 ),
                 
                 tabPanel("Time",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("time_crime_type_1", "Choose a Crime Type:",
                                          choices = list("Vehicle Accident" = "Motor Vehicle Accident",
                                                         "Larceny" = "Larceny", 
                                                         "Medical Assistance" = "Medical Assistance",  
                                                         "Investigate Person" = "Investigate Person",   
                                                         "Drug Violation" = "Drug Violation",  
                                                         "Simple Assault" = "Simple Assault",   
                                                         "Vandalism" = "Vandalism",   
                                                         "Verbal Disputes" = "Verbal Disputes",
                                                         "Fraud"="Fraud",
                                                         "Investigate Property"= "Investigate Property"
                                          ), selected = c("Motor Vehicle Accident")),
                              selectInput("time_crime_type_2", "Choose a Crime Type:",
                                          choices = list("Vehicle Accident" = "Motor Vehicle Accident",
                                                         "Larceny" = "Larceny", 
                                                         "Medical Assistance" = "Medical Assistance",  
                                                         "Investigate Person" = "Investigate Person",   
                                                         "Drug Violation" = "Drug Violation",  
                                                         "Simple Assault" = "Simple Assault",   
                                                         "Vandalism" = "Vandalism",   
                                                         "Verbal Disputes" = "Verbal Disputes",
                                                         "Fraud"="Fraud",
                                                         "Investigate Property"= "Investigate Property"
                                          ), selected = c("Larceny")),
                              selectInput("time_crime_type_3", "Choose a Crime Type:",
                                          choices = list("Vehicle Accident" = "Motor Vehicle Accident",
                                                         "Larceny" = "Larceny", 
                                                         "Medical Assistance" = "Medical Assistance",  
                                                         "Investigate Person" = "Investigate Person",   
                                                         "Drug Violation" = "Drug Violation",  
                                                         "Simple Assault" = "Simple Assault",   
                                                         "Vandalism" = "Vandalism",   
                                                         "Verbal Disputes" = "Verbal Disputes",
                                                         "Fraud"="Fraud",
                                                         "Investigate Property"= "Investigate Property"
                                          ), selected = c("Drug Violation"))
                              
                            ),
                            
                            mainPanel(
                              dygraphOutput("time_series1"),
                              h3("Crime Count Comparison"),
                              p("Choose up to 3 crime categories you are interested in."),
                              p("The above graph shows the trend of different crime types from 2012-2018. The x axis shows the time span of this dataset. 
                                Slide the range selector to zoom in on to specific dates. The y axis shows the total count of each crime type per day. 
                                See the specific count value by putting the mouse on a line. 
                                ")
                              )
                              )
                          
                              ),
                 
                 tabPanel("Schools",
                          sidebarLayout(
                            sidebarPanel(
                              checkboxGroupInput("school_type", "Select School Type:",
                                                 choices = list("Public (K-12)" = "Public",
                                                                "Non-Public (K-12)" = "Non-Public",
                                                                "College" = "College"
                                                 ), selected = c("Public", "Non-Public", "College")),
                              
                              checkboxGroupInput("school_UCR", label = "Type of Crime", 
                                                 choices = list("Serious Crime" = "part1_count", 
                                                                "Less Crime" = "part2_count", 
                                                                "Least Serious" = "part3_count"),
                                                 selected = c("part1_count")),
                              
                              checkboxGroupInput("school_info", label = "Information Shown in Label", 
                                                 choices = list("Name of School" = 1, 
                                                                "Town" = 2, 
                                                                "Address" = 3),
                                                 selected = c(1))
                            ),
                            
                            
                            
                            mainPanel(
                              leafletOutput("schools"),
                              h3("Is you or your child in school? Or thinking about one?"),
                              h3("Check out which schools are in safe neighborhoods!"),
                              p("Or if you are just curious about the relationship between school locations and crime..."),
                              p("Explore schools and crime levels in this ", strong("map,"), " which shows public, non-public, and college locations on top of crime level by neighborhood"),
                              p("Select type of school, crime type, and information shown in label using the sidebar")
                            )
                          )
                          
                 ),
                 
                 tabPanel("About", includeMarkdown("About_new.Rmd"))
                 
                 
                 
                          )

server <- function(input, output){
  
  # Data -----------
  crime_original <- read_csv("~/DS_CLASS_2018-19_Christina/final_project/2012-2018dataset.csv")
  
  crime <- crime_original %>%
    mutate(date = as.Date(occurred)) %>%
    mutate(OFFENSE_CODE_GROUP = INCIDENT_TYPE_DESCRIPTION,
           OCCURRED_ON_DATE = occurred,
           STREET = STREETNAME,
           DISTRICT = REPTDISTRICT,
           UCR_PART = UCRPART) %>%
    select(-INCIDENT_TYPE_DESCRIPTION, -occurred, -STREETNAME, -REPTDISTRICT, -UCRPART) %>%
    separate(Location, into = c("Lat", "Long"), sep = ", ") %>%
    mutate(Lat = as.numeric(str_sub(Lat, 2)), Long = as.numeric(str_sub(Long, 1, -2)))
  
  
  reference <- tibble(
    original = c("Homicide", "Larceny", "Larceny From Motor Vehicle", 
                 "Residential Burglary", "Commercial Burglary", "Other Burglary",
                 "Robbery", "Aggravated Assault", "Drug Violation"),
    new = c("Homicide", "Larceny", "Larceny", 
            "Burglary", "Burglary", "Burglary",
            "Robbery", "Aggravated Assault", "Drug Violation")
  )
  
  crime <- crime %>%
    left_join(reference, by = c("OFFENSE_CODE_GROUP" = "original")) %>%
    mutate(label = ifelse(is.na(new), "Other", new))
  
  # Map 1 -----------
  
  centers <- tibble(
    district = c("C11", "B3", "E5", "E18", "E13", "B2", "D4", "D14", "A7", "A15", "A1", "C6"),
    lng = c(-71.06, -71.085, -71.15, -71.125, -71.116, -71.08, -71.08, -71.148, -71.026, -71.063, -71.063, -71.047),
    lat = c(42.3, 42.283, 42.28, 42.26, 42.31, 42.32, 42.345, 42.35, 42.37, 42.377, 42.357, 42.337)
  ) %>%
    arrange(district)
  
  districts <- geojson_read("~/DS_CLASS_2018-19_Christina/final_project/Police_Districts.geojson",
                            what = "sp")
  
  
  # outputs the regular map (with markers that indicate individual crimes)
  output$map_coordinates <- renderLeaflet({
    coordinates <- crime %>%
      filter(date == input$date) %>%
      filter(Lat > 40, Long < -70) %>%
      filter(UCR_PART %in% input$UCR_type | label %in% input$crime_type) %>%
      mutate(time = format(OCCURRED_ON_DATE, format = "%H:%M"))
    
    
    temp <- coordinates %>%
      select(OFFENSE_CODE_GROUP, time, STREET, DISTRICT) %>%
      .[as.numeric(input$popup_info)] 
    
    popup_message <- do.call(what = "paste", args = c(temp, sep = ", "))
    
    
    if(nrow(coordinates) > 0){
      leaflet(districts) %>%
        addTiles() %>%
        addPolygons(weight = 1, smoothFactor = 0.5, fillOpacity = 0.1, color = "black",
                    fillColor = "blue") %>%
        addMarkers(coordinates$Long, coordinates$Lat, 
                   popup = htmlEscape(popup_message), label = popup_message)
    }
    
    else{
      leaflet(districts) %>%
        addTiles() %>%
        addPolygons(weight = 1, smoothFactor = 0.5, fillOpacity = 0.1, color = "black",
                    fillColor = "blue")
    }
    
    
  })
  
  
  
  # Heatmap --------------
  
  output$heatmap <- renderLeaflet({
    df <- crime %>%
      filter(Lat > 40, Long < -70) %>%
      filter(OCCURRED_ON_DATE >= min(input$heat_dates), OCCURRED_ON_DATE <= max(input$heat_dates)) %>%
      filter(UCR_PART %in% input$heat_UCR_type) 
    
    # leaflet(districts) %>%
    #   addTiles() %>%
    #   addProviderTiles("CartoDB.Positron") %>%
    #   addPolygons(weight = 1, smoothFactor = 0.5, fillOpacity = 0.5, color = "black", fillColor = "black") %>%
    #   addWebGLHeatmap(lng=~df$Long, lat=~df$Lat, size = input$size) 
    
    crime %>%
      filter(Lat > 40, Long < -70) %>%
      filter(OCCURRED_ON_DATE >= min(input$heat_dates), OCCURRED_ON_DATE <= max(input$heat_dates)) %>%
      filter(UCR_PART %in% input$heat_UCR_type) %>%
      leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = districts, weight = 1, smoothFactor = 0.5, fillOpacity = 0.1, color = "black",
                  fillColor = "blue") %>%
      addWebGLHeatmap(lng=~Long, lat=~Lat, size = input$size)
    
  })
  
  # Time Series----------------
  
  output$time_series1 <- renderDygraph({
    
    data <- crime_original %>%
      mutate(date = as.Date(occurred)) %>%
      separate(date,c("year","month","day"),"-")
    
    
    data_1 <-data %>%
      filter(INCIDENT_TYPE_DESCRIPTION %in% input$time_crime_type_1) 
    data_2 <-data %>%
      filter(INCIDENT_TYPE_DESCRIPTION %in% input$time_crime_type_2) 
    data_3<-data%>%
      filter(INCIDENT_TYPE_DESCRIPTION %in% input$time_crime_type_3) 
    
    
    if(nrow(data_1) > 0 ) { 
      data_1 <- data_1 %>%
        group_by(year,month,day)%>%
        mutate(count=n())
      data_1$occurred  = ymd_hms(data_1$occurred)
      
      data_2 <- data_2 %>%
        group_by(year,month,day)%>%
        mutate(count=n())
      data_2$occurred  = ymd_hms(data_2$occurred)
      
      data_3 <- data_3 %>%
        group_by(year,month,day)%>%
        mutate(count=n())
      data_3$occurred  = ymd_hms(data_3$occurred)
      
      # Then you can create the xts format, and thus use dygraph
      a=xts(x = data_1$count, order.by = data_1$occurred)
      b=xts(x = data_2$count, order.by = data_2$occurred)
      c=xts(x = data_3$count, order.by = data_3$occurred)
      
      don<-cbind(a,b,c)
      
      dygraph(don,main="Crime Comparison", ylab = "Count") %>%
        dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2"),connectSeparatedPoints=TRUE) %>%
        dyRangeSelector() %>%
        dyCrosshair(direction = "vertical") %>%
        dyRoller(rollPeriod = 1)%>%
        dyLegend(show="always",labelsSeparateLines = TRUE)%>%
        dySeries("..1", label = input$time_crime_type_1)%>%
        dySeries("..2", label = input$time_crime_type_2)%>%
        dySeries("..3", label = input$time_crime_type_3)
    }
    
  })
  
  # Education----------
  
  #public schools
  schools <- geojson_read("~/DS_CLASS_2018-19_Christina/final_project/Public_Schools.geojson", what = "sp")
  # nonpublic schools
  schools_np <- geojson_read("~/DS_CLASS_2018-19_Christina/final_project/Non_Public_Schools.geojson", what = "sp")
  #colleges
  schools_c <- geojson_read("~/DS_CLASS_2018-19_Christina/final_project/Colleges_and_Universities.geojson", what = "sp")
  school_coord <- read_csv("~/DS_CLASS_2018-19_Christina/final_project/Public_Schools.csv")
  school_coord_np <- read_csv("~/DS_CLASS_2018-19_Christina/final_project/Non_Public_Schools.csv")
  school_coord_c <- read_csv("~/DS_CLASS_2018-19_Christina/final_project/Colleges_and_Universities.csv")
  
  output$schools <- renderLeaflet({
    

    temp <- MAcount %>%
      select(one_of(input$school_UCR)) %>%
      apply(MARGIN = 1, FUN = sum)
    
    crime_count <- tibble(
      GEOID = MAcount$GEOID,
      count = temp
    )
    
    shape_crime <- append_data(data = crime_count, shp = MA, key.shp = "GEOID", key.data = "GEOID")
    
    testShape <- shape_crime[!is.na(shape_crime@data$count), ]
    
    pal <- colorNumeric(
      palette = "Reds",
      domain = testShape@data$count,
      alpha = TRUE)
    
    map <- leaflet(testShape) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(fillOpacity = 1, fillColor = ~pal(count), stroke = FALSE, weight = 1, color = "black") %>%
      leaflet::addLegend("bottomright", pal = pal, values = ~count,
                         title = "Crime Count",
                         opacity = 1
      )
    
    
    temp <- schools@data %>%
      select(SCH_NAME, CITY, ADDRESS) %>%
      .[as.numeric(input$school_info)] 
    label_message <- do.call(what = "paste", args = c(temp, sep = ", "))
    
    temp <- schools_np@data %>%
      select(NAME, TOWN_MAIL, ADDRESS) %>%
      .[as.numeric(input$school_info)] 
    label_message_np <- do.call(what = "paste", args = c(temp, sep = ", "))
    
    temp <- schools_c@data %>%
      select(Name, City, Address) %>%
      .[as.numeric(input$school_info)] 
    label_message_c <- do.call(what = "paste", args = c(temp, sep = ", "))
    
    if("Public" %in% input$school_type){
      map <- map %>%
        addCircleMarkers(data = schools, radius = 3, color = "blue", label = label_message)
    }
    
    if("Non-Public" %in% input$school_type){
      map <- map %>%
        addCircleMarkers(data = schools_np, radius = 3, color = "#3c3", label = label_message_np)
    }
    
    if("College" %in% input$school_type){
      map <- map %>%
        addCircleMarkers(data = schools_c, radius = 3, color = "yellow", label = label_message_np)
    }
    
   map
  })
  
  
  
}

shinyApp(ui = ui, server = server)

