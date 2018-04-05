library(shiny)
library(DT)
library(tidyverse)
library(readxl)
library(leaflet)
library(rgdal)


SWEREF99 <- CRS("+init=epsg:3006")
RT90 <- CRS("+init=epsg:4124")
WGS84 <- CRS("+init=epsg:4326")

REFS <- c(SWEREF99, RT90, WGS84)
names(REFS) <- c("SWEREF99", "RT90", "WGS84")

# UI
ui <- fluidPage(
    titlePanel(title = "Convert GPS data"),

    sidebarLayout(
        sidebarPanel(
            fileInput("uploaded_file", "Choose excel File that holds GPS data",
                      multiple = TRUE,
                      accept = c(".xls", ".xlsx")),
            tags$hr(),
            checkboxInput("header", "Header", TRUE),

            selectInput("gpsfrom", "Input GPS reference system",
                        c("RT90", "SWEREF99", "WGS84")),
            selectInput("gpsto", "Output GPS reference system",
                        c("RT90", "SWEREF99", "WGS84")),
            tags$hr(),

            radioButtons("disp", "Display",
                         choices = c(All = "all", Head = "head"),
                         selected = "all"),

            uiOutput("select_X"),

            uiOutput("select_Y")
        ),

        mainPanel(
            tabsetPanel(id = "dataset",
                        tabPanel("FILE", DT::dataTableOutput("rendered_file"),
                                 downloadButton("downloadData", "Download"),
                                 downloadButton("downloadData2", "Download2")),
                        tabPanel("Converted GPS coordinates",
                                 DT::dataTableOutput("df_conv")),
                        tabPanel("Map", leafletOutput("map")))
        )
    )
)

# Server logic
server <- function(input, output, session) {
    df <- reactive({
    req(input$uploaded_file)
    read_excel(input$uploaded_file$datapath,
               col_names = input$header,
               sheet = 1)
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

    df_sel <- reactive({
      req(input$select_X)
      req(input$select_Y)
      df_sel <- df() %>%
          select(input$select_X, input$select_Y)
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
        p1 <- SpatialPointsDataFrame(df_sel(), data = df_sel(), proj4string = inPutGPS)
        p2 <- spTransform(p1, outPutGPS)
        colnames(p2@coords) <- c(paste("Longitude", input$gpsto, sep = " "), paste("Latitude", input$gpsto, sep = " "))
        p2@coords

    })

  # Generate table of converted values
    output$df_conv <- DT::renderDataTable({
        if(input$disp == "head") {
            head(df_conv())
        } else {
            df_conv()
        }
    })


  # Generate map from converted values
    output$map <- renderLeaflet({
      leaflet() %>%
          addProviderTiles(providers$OpenStreetMap,
                           options = providerTileOptions(noWrap = TRUE)) %>%
          addMarkers(data = df_conv())
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
          write.table(cbind(df(), df_conv()), file, row.names = FALSE, fileEncoding = "UTF-8", sep = "\t")

      },
      contentType = "text/csv")
}

# Start and run the shiny app
shinyApp(ui, server)
