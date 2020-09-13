library(shiny)
library(DT)
library(tidyverse)
library(readxl)
library(leaflet)
library(htmltools)
library(rgdal)
options(encoding = 'UTF-8')
source("functions.R")
SWEREF99 <- CRS("+init=epsg:3006")
RT90 <- CRS("+init=epsg:3021")
WGS84 <- CRS("+init=epsg:4326")
UTM32N <- CRS("+init=epsg:32632")

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
        ".tdf", ".txt")),
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

            uiOutput("select_X"),

            uiOutput("select_Y"),
            
            uiOutput("select_N")
        ),

        mainPanel(
            tabsetPanel(id = "dataset",
                         tabPanel("FILE", DT::dataTableOutput("rendered_file"),
                                 downloadButton("downloadData", "Download"),
                                 downloadButton("downloadData2", "Download2")),
                        tabPanel("Converted GPS coordinates",
                                 DT::dataTableOutput("df_conv")),
                        tabPanel("Map", leafletOutput("map")))        )
    )
)

# Server logic
server <- function(input, output, session) {
    df <- reactive({
    req(input$uploaded_file)
    source("functions.R")
    load_data(input$uploaded_file$datapath)
    ## if(grepl(pattern = "xls", input$uploaded_file$datapath)) {
      ##   read_excel(input$uploaded_file$datapath,
      ##          col_names = input$header,
      ##          sheet = 1)
      ## } else {
      ##     read.table(input$uploaded_file$datapath,
      ##                header = input$header,
      ##                sep = " ")
      ##   }
    })

  # Dynamically generate UI input appears after file is loaded
    output$select_X <- renderUI({
    selectInput(inputId = "select_X",
                       label = "Select Column with Longitude data",
                choices = names(df()))
    })

    output$select_Y <- renderUI({
    selectInput(inputId = "select_Y",
                       label = "Select Column with Latitude data",
                       choices = names(df()))
    })
    
    output$select_N <- renderUI({
      selectInput(inputId = "select_N",
                  label = "Select Column with Sample name",
                  choices = names(df()))
    })

    df_sel <- reactive({
      req(input$select_X)
      req(input$select_Y)
      req(input$select_N)
      df_sel <- df() %>%
          select(input$select_N, input$select_X, input$select_Y) %>%
          rename(Name = input$select_N , Longitude = input$select_X,
                 Latitude = input$select_Y)
      
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
        # p <- df_sel() %>% select(2, 1)
        inPutGPS <- REFS[[input$gpsfrom]]
        outPutGPS <- REFS[[input$gpsto]]
        df <- df_sel()
        df <- df[,-1]
        p1 <- SpatialPointsDataFrame(df, data = df_sel(), proj4string = inPutGPS)
        p2 <- spTransform(p1, outPutGPS)
        #colnames(p2@coords) <- c(paste("Longitude", input$gpsto, sep = " "), paste("Latitude", input$gpsto, sep = " "))
        #p2@coords
        p2
    })

  # Generate table of converted values
    output$df_conv <- DT::renderDataTable({
        if(input$disp == "head") {
            head(as.data.frame(df_conv()))
        } else {
            as.data.frame(df_conv())
        }
    })


  # Generate map from converted values
    output$map <- renderLeaflet({
      leaflet(df_conv()) %>%
          addProviderTiles(providers$OpenStreetMap,
                           options = providerTileOptions(noWrap = TRUE)) %>%
          addMarkers(label = ~htmlEscape(Name))
    })

  # Generate downloadable file
    output$downloadData <- downloadHandler(
      filename = function() {
          paste(input$uploaded_file, ".txt", sep = "")
      },
      content = function(file) {
          write.table(df_sel(), file, row.names = FALSE, fileEncoding = "utf8", sep = "\t")
      })


  # Additional download options
    output$downloadData2 <- downloadHandler(
      filename = function() {
          paste(input$uploaded_file, ".txt", sep = "")
      },
      content = function(file) {
          write.table(cbind(as.data.frame(df()), as.data.frame(df_conv())), file, row.names = FALSE, fileEncoding = "UTF-8", sep = "\t")

      },
      contentType = "text/csv")
}

# Start and run the shiny app
shinyApp(ui, server)
