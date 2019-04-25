# what is updated in version 3?

# what need to do more:
# save the data into the disk once the user click searching.
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
# read in all the temperature infors (raster).
list.ras <- mixedsort(list.files("data/wc10", full.names = T, 
                                 pattern = ".bil$"))
tmin.all <- stack(list.ras)

checkData <- function(list1,list2){
  for (i in range(length(list1))){
    #(list1[i] != -999 | list2[i] != -999) |
    if ( (list1[i] > 180 | list1[i] < -180 | list2[i] > 90 | list2[i] < -90)){
      return ("Invalid coordinates!")
      break
    }
    else{
      return (NULL)
    }
  }
}

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
               numericInput("latitude1", "latitude 1", 40, min = -90, max = 90),
               numericInput("longitude2", "Longtitude 2", -999, min = -180, max = 180),
               numericInput("latitude2", "latitude 2", -999, min = -90, max = 90),
               numericInput("longitude3", "Longtitude 3", -999, min = -180, max = 180),
               numericInput("latitude3","latitude 3", -999, min = -90, max = 90),
               
               h2(textOutput("error")),
                
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
               textInput("input_country", "Please enter a country's name:", "--Not selected--"),
               selectInput("select_mission", "What do you want me to do?", 
                           choices = list("Calculate the minimum temperature" = 1, 
                                          "Choice 2" = 2,
                                          "Choice 3" = 3), selected = 1),
               #textInput("input_month", "Which month are you looking for?", "1"),
               numericInput("input_month", 
                            "Which month are you looking for?", 
                            value = 1, min = 1, max = 12),
               
               
               actionButton("do_countryname","Click!"),
               downloadLink('downloadData', 'Download')
             ),
             mainPanel(
               verbatimTextOutput("valueForNames"),
               plotOutput(outputId = "distPlot"),
               textOutput("serverTime")
             )
    ),
    tabPanel("Load File",
             fileInput("input_file", 
                       "If your data is stored in csv, upload it here", 
                       multiple=TRUE),
             
             tableOutput("table2")
             
    )
    
    
  )
  server <- function(input, output) {
    
    # read in all the temperature infors (raster).
    list.ras <- mixedsort(list.files("data/wc10", full.names = T, 
                                     pattern = ".bil$"))
    tmin.all <- stack(list.ras)
    
    
    crs_chosen <- function(crsName){
      if (crsName == "WGS84 (EPSG: 4326)"){
        proj4string(a) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
      }
      else if (crsName == "NAD83 (EPSG:4269)"){
        proj4string(a) = CRS("+init=epsg:4269")
      }
      else if (crsName == "NAD27 (EPSG: 4267)"){
        proj4string(a) = CRS("+init=epsg:4267")
      }
      else if (crsName == "Mercator (EPSG: 3857)"){
        proj4string(a) = CRS("+init=epsg:3857")
      }
     
    }
    
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
    
    output$distPlot <- renderPlot({
      month_to_calculate <- tmin.all[[paste("tmin",as.character(input$input_month), sep = "")]]
      plot(month_to_calculate)
    })
    

    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("Percipitation in ", ".csv", sep = "")
      },
      content = function(file) {
        write.table(as.data.frame(df_name()), file, row.names = FALSE)
      }
    )
    
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
      selected_input_coordinates <- input_coordinates[which(input_coordinates$input_longtitudes!=-999
                                                            & input_coordinates$input_latitudes!=-999),]
      
      ####################       check if the user input is valid.     #######################
      recheck_click <- eventReactive(input$do_coordinates,{
        validate(checkData(selected_input_coordinates$input_longtitudes,selected_input_coordinates$input_latitudes))
      }
      )
      output$error <- renderText({ recheck_click() })
      
      coordinates(selected_input_coordinates) = c("input_longtitudes","input_latitudes")
      
      crs_chosen(input$select_CRS)
      
      extracted_value <- extract(x = month_to_calculate, y = selected_input_coordinates)
      mean(extracted_value, na.rm = TRUE)
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
      selected_input_coordinates <- Coordinates[which(Coordinates$Longtitudes!= -999
                                                      & Coordinates$Latitudes!=-999),]
      
      recheck_click <- eventReactive(input$do_coordinatesTable,{
        validate(checkData(selected_input_coordinates$Longtitudes,selected_input_coordinates$Latitudes))
      })
      output$error <- renderText({ recheck_click() })
      
      a = selected_input_coordinates
      coordinates(a) = c("Longtitudes","Latitudes")
      
      crs_chosen(input$select_CRS)
      
      precipitation <- extract(x = month_to_calculate, y = a)
      as.data.frame(precipitation)
      
      resolutions <- res(tmin.all)
      user_input <- selected_input_coordinates
      servertime = paste("Server time:", as.character(Sys.time()), as.character(Sys.timezone()),sep = "")
      outputs <- as.data.frame(c("variable one", servertime, resolutions[1], resolutions[2], "WGS84 (EPSG: 4326)", input$select_CRS, user_input, "points", "queried_condition"))
      
      Temp_path <- "temp/"
      if(! file.exists(Temp_path)  ){
        dir.create(Temp_path)
      }
      num = sample(1:10000,1)
      My_filename <-  paste(Temp_path,as.character(Sys.time()),as.character(num),".csv")
      write.csv(outputs,file = My_filename)
      
      
      informationOutputsTable <- cbind(selected_input_coordinates, precipitation)
      informationOutputsTable
      
      
    })
    output$table <- renderTable({ df_tableGenerator() })
    
    
    
    
    ############################################################################
    ####################### if user provided a csv file  #######################

    df_tableGenerator2 <- eventReactive(input$input_file, {
      #new_file = read.csv(input$input_file,header = F, sep = ",")
      new_file = read.csv(input$input_file$datapath,header = F, sep = ",")
      
      long = new_file[1]
      lat = new_file[2]
      #month = list(new_file[3])
      #a = as.character(month==1)

      thedata <- as.data.frame(cbind(long,lat))
      coordinates(thedata) = c("V1","V2")
      proj4string(thedata) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

      month_to_calculate <- tmin.all[[paste("tmin","2", sep = "")]]
      precipitation <- extract(x = month_to_calculate, y = thedata)
      as.data.frame(precipitation)
       
    })
    output$table2 <- renderTable({ df_tableGenerator2() })

    output$serverTime <- renderText({ 
      paste("Server time:", as.character(Sys.time()), as.character(Sys.timezone()),sep = "") 
    })
    
  }
  shinyApp(ui, server)
}
