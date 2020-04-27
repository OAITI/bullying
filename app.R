library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(maps)
library(sp)
library(rgeos)
library(rgdal)
library(maptools)
library(leaflet)
library(scales)
library(shinythemes)
library(shinycssloaders)
library(bsplus)

## Set images resource path
addResourcePath("images", "images")

## Load the bullying data
bullying <- read_csv("data/bullying/bullying_clean.csv")

## Load the iowa schoool district shapefiles
load("data/cb_2016_iowa_school_districts/iowa_districts_simple.RData")

## Load initial versions of the maps for faster initial runtime
load("data/bullying/initial_explore_map.RData")
load("data/bullying/initial_model_map.RData")

## Code to produce simplified shape files
# iowa_districts <- readOGR(dsn = "data/cb_2016_iowa_school_districts", layer = "IdoeSchoolDistrictsFY2016", stringsAsFactors = FALSE)
# iowa_districts_simp_temp <- gSimplify(iowa_districts, tol = 0.02)
# iowa_districts_simple <- SpatialPolygonsDataFrame(iowa_districts_simp_temp, data = iowa_districts@data)
# save(iowa_districts_simple, file = "data/cb_2016_iowa_school_districts/iowa_districts_simple.RData")

## A coef function which will return NA if the model could not be fit
mycoef <- function(formula, data) {
    result <- try(coef(lm(formula, data = data)))
    
    if (inherits(result, "try-error")) return(NA)
    
    return(result)
}

ui <- fluidPage(theme = shinytheme("cerulean"),
                
   includeCSS("css/styles.css"),
   
   titlePanel("Reported Bullying Incidents in Iowa School Districts"),
   
   sidebarLayout(
      sidebarPanel(
          a(href = "https://oaiti.org", target = "_blank", img(src = "images/oaiti_transparent.png", width = "135")),
          h4("About"),
          HTML("This application uses data from the <a href='https://www.educateiowa.gov/education-statistics' target='_blank'>Iowa Department of Education</a> on bullying incidents and enrollment in Iowa school districts. We use the district shapefiles to allow you to explore this data by clicking on the maps.<br><br>The Explore tab displays the number of incidents of a particular type in a particular year, with the option to scale by the enrollment of the district. The Model tab uses a simple linear regression to model the trend in the selected incident type over time for each district. Finally, the Data Sources tab gives information on the data collection process and displays a table of the raw results.<br><br>For more information, check out <a href='https://oaiti.org/2017/11/29/bullying-incidents-in-iowa-school-districts/' target='_blank'>our blog</a>.<br><br>"),
          actionButton("view_disclaimer", "View Disclaimer", icon = icon("info-circle")) %>%
            bs_attach_modal(id_modal = "startupModal"),
          hr(),
          
          h4("Configuration"),
          selectInput("variable", "Incidents", choices = c("All" = "Founded Incidents", names(bullying)[4:21])),
          
          conditionalPanel(condition = "input.tabs1 == 'Explore'",
                           selectInput("year", "Year", choices = c("All", 2013, 2014, 2015, 2016)),
                           checkboxInput("scale", "Scale by District Enrollment")     
          )
      ),
      
      mainPanel(
          bs_modal(id = 'startupModal', title = 'Reported Bullying Incidents Disclaimer',
                   size = "medium", HTML("We have created this app to make the publicly available bullying data for Iowa easier to visualize and interpret. However, some important disclaimers should be noted:<br><br>1. This data consists of <b>reported</b> bullying incidents. Many bullying incidents are left unreported, and some districts may be better about logging these reports than others.<br><br>2. Districts in which between 1 and 9 bullying incidents were reported in a particular year had the <b>data redacted</b> to protect student privacy. These districts will display as white in both the Explore and the Model tabs. See the <a href='https://www.educateiowa.gov/data-reporting/data-reporting/data-access-sharing-and-privacy'>Iowa Department of Education Data Privacy</a> for more information.<br><br>3. The model is a simple trend in the incidents over a four year period. It is meant to be a prototype and <b>should not be used to make decisions</b> based on the bullying trend. Please use the model as an example of a possible analysis and not as a decision making tool.<br><br> By clicking \"Close\", you confirm that you have read these disclaimers.")),
          
          tabsetPanel(id = "tabs1",
              tabPanel("Explore",
                       h4("Map of Reported Bullying Incidents"),
                       HTML("The map below displays the total number of bullying incidents by district over the specified time frame. Use the inputs on the left to filter to a specific type of incident and/or a specific year. Districts colored in white had missing or redacted data during the specified time frame."),
                       
                       withSpinner(leafletOutput("mymap"))
              ),
              
              tabPanel("Model",
                       h4("Modeling the Trend"),
                       HTML("Here we use a Simple Linear Regression to model the trend in bullying incidents between 2013 and 2016. A negative trend (colored in green) indicates that bullying incidents appear to be on the decrease, while a positive trend (colored in red) indicates an increase. Districts are colored white where data was redacted or too limited to compute the model."),
                       
                       withSpinner(leafletOutput("modelmap"))
              ),
              
              tabPanel("Data Source",
                       h4("Data Source"),
                       HTML("This data was collected by the <a href='http://educateiowa.gov' target='_blank'>Iowa Department of Education</a>. The data has been collectd from the Excel spreadsheets on the site. Because of a change in data format in 2013, data from prior to 2013 was not used. Also note that in districts where bullying incidents were between 1 and 9 in a particular year, the exact value was redacted to preserve student privacy. We have set these values to be missing, and thus districts with these redacted values will appear as white on the maps."),
                       
                       hr(),
                       
                       h4("Raw Data"),
                       withSpinner(dataTableOutput("data"))
              )
          )
      )
   )
)

server <- function(input, output, session) {
    
    values <- reactiveValues(firstrun = FALSE)
    
    observeEvent(input$year, {
        values$firstrun <- FALSE
    }, ignoreInit = TRUE)
    
    observeEvent(input$scale, {
        values$firstrun <- FALSE
    }, ignoreInit = TRUE)
    
    observeEvent(input$variable, {
        values$firstrun <- FALSE
    }, ignoreInit = TRUE)
    
    output$modelmap <- renderLeaflet({
        if (values$firstrun) {
            return(initial_model_map)
        } else {
            withProgress(message = "Rendering model map", detail = "Please wait...", {
                bullying$MyVar <- bullying[[input$variable]]
                bullying_fip <- bullying %>%
                    group_by(District, Year) %>%
                    summarise(`Incidents` = sum(MyVar),
                              `District Name` = tools::toTitleCase(`District Name`)[1]) %>%
                    group_by(`District`) %>%
                    do(Trend = mycoef(Incidents ~ Year, data = .)[2],
                       Name = .$`District Name`[1])
                bullying_fip2 <- data.frame(District = bullying_fip$District,
                                            Name = unlist(bullying_fip$Name),
                                            Trend = unlist(bullying_fip$Trend))
                
                leafmap <- iowa_districts_simple %>%
                    merge(bullying_fip2, by.x = "IDOE_ID", by.y = "District")
                
                # Format popup data for leaflet map.
                popup_dat <- paste0("<strong>District: </strong>", 
                                    leafmap$Name, 
                                    "<br><strong>Incident Trend: </strong>", 
                                    ifelse(is.na(leafmap$Trend), "Not Available", paste(round(leafmap$Trend, digits = 3), "per year")))
                
                absmax <- ceiling(max(abs(leafmap$Trend), na.rm = TRUE))
                pal <- colorNumeric("RdYlGn", -absmax:absmax, reverse = TRUE, na.color = "#FFFFFF")
                
                # Render final map in leaflet.
                leaflet(data = leafmap) %>% addTiles() %>%
                    addPolygons(fillColor = ~pal(Trend), 
                                fillOpacity = 0.8, 
                                color = "#BDBDC3", 
                                weight = 1,
                                popup = popup_dat) %>%
                    addLegend(pal = pal, values = ~Trend, opacity = 0.6, title = "Trend in Incidents", na.label = "")
            })
        }
    })
    
    output$mymap <- renderLeaflet({
        if (values$firstrun) {
            return(initial_explore_map)
        } else {
            withProgress(message = "Rendering incidents map", detail = "Please wait...", {
                myyear <- input$year
                if (myyear == "All") myyear <- 2013:2016
                
                bullying$MyVar <- bullying[[input$variable]]
                bullying_fip <- bullying %>%
                    filter(Year %in% myyear) %>%
                    group_by(District) %>%
                    summarise(`Incidents` = ifelse(all(is.na(MyVar)), NA, sum(MyVar, na.rm = TRUE)),
                              `District Name` = `District Name`[1],
                              `District Enrollment` = ifelse(all(is.na(Enrollment)), NA, sum(Enrollment, na.rm = TRUE)))
                if (input$scale) bullying_fip$Incidents = bullying_fip$Incidents * 1000 / bullying_fip$`District Enrollment`
                
                leafmap <- iowa_districts_simple %>%
                    merge(bullying_fip, by.x = "IDOE_ID", by.y = "District")
                
                # Format popup data for leaflet map.
                popup_dat <- paste0("<strong>District: </strong>", 
                                    leafmap$`District Name`, 
                                    "<br><strong>Incidents: </strong>", 
                                    ifelse(is.na(leafmap$Incidents), "Not Available", round(leafmap$Incidents, digits = 2)),
                                    "<br><strong>Enrollment: </strong>",
                                    leafmap$`District Enrollment`)
                
                mytitle <- ifelse(input$scale, paste(input$variable, "(per 1000)"), input$variable)
                pal <- colorNumeric("YlOrRd", NULL, na.color = "#FFFFFF")
                
                # Render final map in leaflet.
                leaflet(data = leafmap) %>% addTiles() %>%
                    addPolygons(fillColor = ~pal(Incidents), 
                                fillOpacity = 0.8, 
                                color = "#BDBDC3", 
                                weight = 1,
                                popup = popup_dat) %>%
                    addLegend(pal = pal, values = ~Incidents, opacity = 0.6, title = mytitle, na.label = "")
            })
        }
    })
    
    output$data <- renderDataTable({
        myyear <- input$year
        if (myyear == "All") myyear <- 2013:2016
        
        return(bullying %>%
                   filter(Year %in% myyear) %>%
                   select(District, `District Name`, Year, Enrollment, .data[[input$variable]]) %>%
                   arrange(desc(.data[[input$variable]])))
    })
    
}

shinyApp(ui = ui, server = server)
