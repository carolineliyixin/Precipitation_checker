# what need to do more:
# optimize the performance of the interface
# coordinates (can choose different crs())
    # input 3 points, return a table (column combine), include id, x, y, extracted_value...
# decide which kind of input user provide
# different kind of column can be returned (2)
# user input 2 points, make them into spacial point, and extract value based on the point.
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


acountry <- subset(county, county$CNTRY_NAME == "Afghanistan")
extracted = extract(x = tmin.all[[1]], y = acountry)
mean(extracted[[1]])



#plot(aruba)

## Only run examples in interactive R sessions
if (interactive()) {
  
  ui <- fluidPage(
    titlePanel("Global Precipitation explorer"),
    column(6,
           numericInput("longitude1","Longtitude 1", 0, min = -180, max = 180)
    ),
    column(6,
           numericInput("latitude1", "latitude 1", -6.64722, min = -180, max = 180)
    ),
    column(6,
           numericInput("longitude2", "Longtitude 2", -1, min = -180, max = 180)
    ),
    column(6,
           numericInput("latitude2", "latitude 2", -1, min = -180, max = 180)
    ),
    column(6,
           numericInput("longitude3", "Longtitude 3", -1, min = -180, max = 180)
    ),
    column(6,
           numericInput("latitude3","latitude 3", -1, min = -180, max = 180)
    ),
    
    textInput("input_country", "Please enter a country's name:", "Aruba"),
    selectInput("select_mission", "What do you want me to do?", 
                choices = list("Calculate the minimum temperature" = 1, 
                               "Choice 2" = 2,
                               "Choice 3" = 3), selected = 1),
    #textInput("input_month", "Which month are you looking for?", "1"),
    numericInput("input_month", 
                 "Which month are you looking for?", 
                 value = 1, min = 1, max = 12),
    fileInput("input_file", 
              "If your data is stored in csv, upload it here", 
              multiple=TRUE),

    verbatimTextOutput("valueForNames"),
    verbatimTextOutput("valueForCoords"),
    tableOutput("table"),
    actionButton("do_countryname","Click!"),
    actionButton("do_coordinates","Click!")
    
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
      month_to_calculate

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
      proj4string(selected_input_coordinates) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
      extracted_value <- extract(x = month_to_calculate, y = selected_input_coordinates)
      mean(extracted_value[[1]])
    })
    output$valueForCoords <- renderText({ df_coord() })
    
    #output$table <- DT::renderDataTable({ df_coord() })
    
    ############################################################################
    ####################### if user provided a csv file  #######################
    
    ## but we don't know what the csv file would look like??
    #new_file = read.csv(input$input_file,header = T, strip.white = T, sep = ",")
    

    
    
  }
  shinyApp(ui, server)
}