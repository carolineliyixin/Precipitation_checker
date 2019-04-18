# what is updated in version 2?
# V2 is able to manipulate coordinates inputs, specifically:
  # 3 points, 
  # return a table (column combine), include id, x, y, extracted_value...
  # user can choose CRS coresponding to their inputs
# optimize the performance of the interface

# what need to do more:
# cont. optimize the performance of the interface
# different kind of column can be returned (2)
# user upload a csv file, read csv and then 


library(shiny)
library(dismo)
library(raster)
library(rgdal)
library(gtools)
library(sp)

# read in a world map (shapefile)
county = shapefile("data/country/country.shp")
#View(county@data)
# read in all the temperature infors (raster).
list.ras <- mixedsort(list.files("data/wc10", full.names = T, 
                                 pattern = ".bil$"))
tmin.all <- stack(list.ras)

a = list(county$CNTRY_NAME)

acountry <- subset(county, county$CNTRY_NAME == "Afghanistan")
extracted = extract(x = tmin.all[[1]], y = acountry)
mean(extracted[[1]])



## Only run examples in interactive R sessions
if (interactive()) {
  
  ui <- navbarPage(
    "Database input options",
    titlePanel("Global Precipitation explorer"),
    
    tabPanel("coordinates",
      sidebarPanel(
       selectInput("select_CRS","Select the CRS that applies on your coordinates.",
                   choices = list("WGS84 (EPSG: 4326)" = 1, 
                                  "NAD83 (EPSG:4269)" = 2,
                                  "NAD27 (EPSG: 4267)" = 3,
                                  "Mercator (EPSG: 3857)" = 4), selected = 1),
       numericInput("longitude1","Longtitude 1", 30, min = -180, max = 180),
       numericInput("latitude1", "latitude 1", 40, min = -180, max = 180),
       numericInput("longitude2", "Longtitude 2", -1, min = -180, max = 180),
       numericInput("latitude2", "latitude 2", -1, min = -180, max = 180),
       numericInput("longitude3", "Longtitude 3", -1, min = -180, max = 180),
       numericInput("latitude3","latitude 3", -1, min = -180, max = 180),
       
       actionButton("do_coordinates","get the mean value of precipitation!",class = "btn-primary"),
       actionButton("do_coordinatesTable","get the table!",class = "btn-primary")
      ),
      mainPanel(
        verbatimTextOutput("valueForCoords"),
        tableOutput("table")
      )
    ),
     
    tabPanel("country name",
      sidebarPanel(
       textInput("input_country", "Please enter a country's name:", "Aruba"),
       selectInput("select_mission", "What do you want me to do?", 
                   choices = list("Calculate the minimum temperature" = 1, 
                                  "Choice 2" = 2,
                                  "Choice 3" = 3), selected = 1),
       #textInput("input_month", "Which month are you looking for?", "1"),
       numericInput("input_month", 
                    "Which month are you looking for?", 
                    value = 1, min = 1, max = 12),
       
       
       actionButton("do_countryname","Click!")
      ),
      mainPanel(
       verbatimTextOutput("valueForNames")
      )
    ),
    tabPanel("country name",
     fileInput("input_file", 
               "If your data is stored in csv, upload it here", 
               multiple=TRUE)
    )
    
    
  )
  server <- function(input, output) {
    
    # read in all the temperature infors (raster).
    list.ras <- mixedsort(list.files("data/wc10", full.names = T, 
                                     pattern = ".bil$"))
    tmin.all <- stack(list.ras)
    
    ############################################################################
    #################### if user provided the country name ####################
    
    df_name <- eventReactive(input$do_countryname, {
      
      # extract all of the minimum temperature for a specific month for every country
      month_to_calculate <- tmin.all[[paste("tmin",as.character(input$input_month), sep = "")]]

      # Get the extract value for a specfic month and a specific country
      new_country <- subset(county, county$CNTRY_NAME == input$input_country)
      extracted_value <- extract(x = month_to_calculate, y = new_country)
      mean(extracted_value[[1]])

    })
    output$valueForNames <- renderText({ df_name() })
    
    ############################################################################
    #################### if user provided the coordinates  #####################
    
    df_coord <- eventReactive(input$do_coordinates, {
      month_to_calculate <- tmin.all[[paste("tmin",as.character(input$input_month), sep = "")]]
      
      input_longtitudes <- c(input$longitude1,
                             input$longitude2,
                             input$longitude3)
      input_latitudes <- c(input$latitude1,
                           input$latitude2,
                           input$latitude3)
      input_coordinates <- as.data.frame( cbind(input_longtitudes,input_latitudes) )
      selected_input_coordinates <- input_coordinates[which(input_coordinates$input_longtitudes!="-1"
                                                            & input_coordinates$input_latitudes!="-1"),]

      coordinates(selected_input_coordinates) = c("input_longtitudes","input_latitudes")
      if (input$select_CRS == "WGS84 (EPSG: 4326)"){
        proj4string(selected_input_coordinates) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
      }
      else if (input$select_CRS == "NAD83 (EPSG:4269)"){
        proj4string(selected_input_coordinates) = CRS("+init=epsg:4269")
      }
      else if (input$select_CRS == "NAD27 (EPSG: 4267)"){
        proj4string(selected_input_coordinates) = CRS("+init=epsg:4267")
      }
      else if (input$select_CRS == "Mercator (EPSG: 3857)"){
        proj4string(selected_input_coordinates) = CRS("+init=epsg:3857")
      }
      extracted_value <- extract(x = month_to_calculate, y = selected_input_coordinates)
      mean(extracted_value)
    })
    output$valueForCoords <- renderText({ df_coord() })
    
    
    
    df_tableGenerator <- eventReactive(input$do_coordinatesTable, {
      
      month_to_calculate <- tmin.all[[paste("tmin",as.character(input$input_month), sep = "")]]
      Longtitudes <- c(input$longitude1,
                             input$longitude2,
                             input$longitude3)
      Latitudes <- c(input$latitude1,
                           input$latitude2,
                           input$latitude3)
      Coordinates <- as.data.frame( cbind(Longtitudes,Latitudes) )
      selected_input_coordinates <- Coordinates[which(Coordinates$Longtitudes!="-1"
                                                            & Coordinates$Latitudes!="-1"),]
      
      a = selected_input_coordinates
      coordinates(a) = c("Longtitudes","Latitudes")
      if (input$select_CRS == "WGS84 (EPSG: 4326)"){
        proj4string(a) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
      }
      else if (input$select_CRS == "NAD83 (EPSG:4269)"){
        proj4string(a) = CRS("+init=epsg:4269")
      }
      else if (input$select_CRS == "NAD27 (EPSG: 4267)"){
        proj4string(a) = CRS("+init=epsg:4267")
      }
      else if (input$select_CRS == "Mercator (EPSG: 3857)"){
        proj4string(a) = CRS("+init=epsg:3857")
      }
      precipitation <- extract(x = month_to_calculate, y = a)
      as.data.frame(precipitation)
      
      informationOutputsTable <- cbind(selected_input_coordinates, precipitation)
      informationOutputsTable
      })
    output$table <- renderTable({ df_tableGenerator() })
    
    ############################################################################
    ####################### if user provided a csv file  #######################
    
    ## but we don't know what the csv file would look like??
    #new_file = read.csv(input$input_file,header = T, strip.white = T, sep = ",")
    

    
    
  }
  shinyApp(ui, server)
}