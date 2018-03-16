library(shiny)
library(DT)
library(tidyverse)
library(readxl)
library(leaflet)
library(rgdal)

SWEREF99TM <- CRS("+init=epsg:3006")
RT90 <- CRS("+init=epsg:4124")
WGS84 <- CRS("+init=epsg:4326")

# UI
ui <- fluidPage(

  # Title
  titlePanel(title = "Convert GPS data"),

  # Sidebar layout with input and output definitions
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(

      # Input: Select a file
      fileInput("uploaded_file", "Choose excel File that holds GPS data",
                multiple = TRUE,
                accept = c(".xls", ".xlsx")),

      # Horizontal line
      tags$hr(),

      # Input: Checkbox if file has header
      checkboxInput("header", "Header", TRUE),

      # Input: Select GPS system
      selectInput("gpsfrom", "Input GPS reference system",
                            c("RT90", "SWEREF99", "WGS84")),
                selectInput("gpsto", "Output GPS reference system",
                            c("RT90", "SWEREF99", "WGS84")),


      tags$hr(),

      # Input: Select number of rows to display
      radioButtons("disp", "Display", 
                  choices = c(All = "all",
                               Head = "head"),
                   selected = "all"),

      # Select variables to display
      uiOutput("select_X"),

      uiOutput("select_Y")
      ),

    # Main panel with output
    mainPanel(

      tabsetPanel(
        id = "dataset",
        tabPanel("FILE", DT::dataTableOutput("rendered_file"),
                 #add download botton
                 downloadButton("downloadData", "Download")

),
        tabPanel("Converted GPS coordinates", DT::dataTableOutput("df_conv")),
        tabPanel("Map", leafletOutput("map"))
        )
      )

  )
)

# Server logic
server <- function(input, output, session) {

  # Import file
  df <- reactive({
    req(input$uploaded_file)
    read_excel(input$uploaded_file$datapath,
               col_names = input$header,
               sheet = 1)
  })
    
  # Dynamically generate UI input appears after file is loaded
  output$select_X <- renderUI({
    selectInput(inputId = "select_X",
                       label = "Select Longitude Column",
                       choices = names(df()))
  })

  output$select_Y <- renderUI({
    selectInput(inputId = "select_Y",
                       label = "Select Latitude Column",
                       choices = names(df()))
  })

  # Select columns
  df_sel <- reactive({
      req(input$select_X)
      req(input$select_Y)
      df_sel <- df() %>%
          select(input$select_X, input$select_Y)
  })

  # Generate screen table
  output$rendered_file <- DT::renderDataTable({
    if(input$disp == "head") {
      head(df_sel())
    } else {
      df_sel()
    }
  })

  # Convert GPS coordinates  
  df_conv <- reactive({
        p <- df_sel() %>% select(2, 1)
        p1 <- SpatialPointsDataFrame(p, data = p, proj4string = SWEREF99TM)
        p2 <- spTransform(p1, WGS84)
        colnames(p2@coords) <- c("Longitude WGS84", "Latitude WGS84")
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
          paste(input$uploaded_file, ".csv", sep = "")
      },
      content = function(file) {
          write.csv(df_sel(), file, row.names = FALSE)
      })

}

# Start and run the shiny app
shinyApp(ui, server)
