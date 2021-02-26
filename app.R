library(shiny)
library(DT)
library(tidyverse)
library(readxl)
library(leaflet)
library(htmltools)
library(rgdal)
library(openxlsx)
library(sf)
options(encoding = 'UTF-8')
source("functions.R")

SWEREF99 <- 3006
#SWEREF99 <- sp::CRS("+init=epsg:3006")
RT90 <- 3021
WGS84 <- 4326
UTM32N <- 32632

REFS <- c(SWEREF99, RT90, WGS84, UTM32N)
names(REFS) <- c("SWEREF99", "RT90", "WGS84", "UTM32N")

# UI
ui <- fluidPage(
    titlePanel(title = "CGI GPS Converter"),

    sidebarLayout(
        sidebarPanel(
            fileInput("uploaded_file", "Choose file that holds GPS data",
                      multiple = FALSE,
                      accept = c(".xls", ".xlsx", ".ods", ".csv",
                                  ".tdf", ".txt", ".tsv")),
            tags$hr(),
            checkboxInput("header", "Header", TRUE),

            selectInput("gpsfrom", "Input GPS reference system",
                        c("SWEREF99", "RT90", "WGS84", "UTM32N")),
            selectInput("gpsto", "Output GPS reference system",
                        c("SWEREF99", "RT90", "WGS84", "UTM32N")),
            tags$hr(),

            radioButtons("disp", "Display",
                         choices = c(All = "all", Head = "head"),
                         selected = "all"),

            uiOutput("select_N"),

            uiOutput("select_Y"),
            
            uiOutput("select_X")
        ),

        mainPanel(
            tabsetPanel(id = "dataset",
                        tabPanel("Input", DT::dataTableOutput("rendered_file")),
                        tabPanel("Converted GPS coordinates",
                                 DT::dataTableOutput("df_conv"),
                                 downloadButton("downloadData",
                                                "Download as .csv"),
                                 downloadButton("downloadData2",
                                                "Download as .xlsx")),
                        tabPanel("Map", leafletOutput("map")))

        )
    )
)

# Server logic
server <- function(input, output, session) {
    df <- reactive({
    req(input$uploaded_file)
    source("functions.R")
    load_data(input$uploaded_file$datapath)
    })

  # Dynamically generate UI input appears after file is loaded

    output$select_N <- renderUI({
      selectInput(inputId = "select_N",
                  label = "Select Column with Sample name",
                  choices = names(df()))
    })
    
    output$select_Y <- renderUI({
    selectInput(inputId = "select_Y",
                       label = "Select Column with Latitude data",
                       choices = names(df()))
    })

    output$select_X <- renderUI({
    selectInput(inputId = "select_X",
                       label = "Select Column with Longitude data",
                choices = names(df()))
    })

    
    df_sel <- reactive({
      req(input$select_X)
      req(input$select_Y)
      req(input$select_N)
      df_sel <- df() %>%
          select(input$select_N, input$select_Y, input$select_X) %>%
          rename(Name = input$select_N , Latitude = input$select_Y, Longitude = input$select_X)
      
    })

    output$rendered_file <- DT::renderDataTable({
        if(input$disp == "head"){
            head(df_sel())
        } else {
            df_sel()
        }
    })

  # Convert GPS coordinates
    df_conv <- reactive({
        inPutGPS <- REFS[[input$gpsfrom]]
        outPutGPS <- REFS[[input$gpsto]]
        df <- df_sel()
        #df <- df[,-1]
        p1 <- st_as_sf(df, 
                       coords = c("Longitude", "Latitude"), 
                       crs = inPutGPS)
        p2 <- st_transform(p1, outPutGPS)
        p2
    })

  # Generate WGS84 values for map even if other formats are asked for  
    df_convMap <- reactive({
        inPutGPS <- REFS[[input$gpsfrom]]
        outPutGPS <- REFS[["WGS84"]]
        df <- df_sel()
        #df <- df[,-1]
        p1 <- st_as_sf(df, 
                       coords = c("Longitude", "Latitude"), 
                       crs = inPutGPS)
        p2 <- st_transform(p1, outPutGPS)
        p2
    })
    
  # Generate table of converted values
    output$df_conv <- DT::renderDataTable({
        dt <- df_conv()
        xy <- st_coordinates(dt)
        yx <- xy[,2:1]
        dt <- st_set_geometry(dt, NULL)
        yx <- cbind(dt[,1], yx)
        colnames(yx) <- c("Name", "Latitude", "Longitude")
        if(input$disp == "head") {
            head(yx)
        } else {
            yx
        }
    })


  # Generate map from converted values
    output$map <- renderLeaflet({
      leaflet(df_convMap()) %>%
          addProviderTiles(providers$OpenStreetMap,
                           options = providerTileOptions(noWrap = TRUE)) %>%
          addMarkers(label = ~htmlEscape(Name))
    })

  # Generate downloadable file
    output$downloadData <- downloadHandler(
      filename = function() {
          paste("GPS_data_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        dt <- df_conv()
        xy <- st_coordinates(dt)
        yx <- xy[,2:1]
        dt <- st_set_geometry(dt, NULL)
        yx <- cbind(dt[,1], yx)
        colnames(yx) <- c("Name", "Latitude", "Longitude")
        if(input$disp == "head") {
          write.table(head(yx), file, row.names = FALSE, fileEncoding
                      = "utf8", sep = ",")
          } else {
          write.table(yx, file, row.names = FALSE, fileEncoding
                      = "utf8", sep = ",")
        }
      })


  # Additional download options
    output$downloadData2 <- downloadHandler(
      filename = function() {
          paste("GPS_data_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        dt <- df_conv()
        xy <- st_coordinates(dt)
        yx <- xy[,2:1]
        dt <- st_set_geometry(dt, NULL)
        yx <- cbind(dt[,1], yx)
        colnames(yx) <- c("Name", "Latitude", "Longitude")
        if(input$disp == "head") {
          openxlsx::write.xlsx(head(yx), file)
        } else {
          openxlsx::write.xlsx(yx, file,  sheetName = "ConvertedGPS")
        }
      })
}

# Start and run the shiny app
shinyApp(ui, server)
