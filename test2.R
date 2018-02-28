library(shiny)
library(DT)
library(tidyverse)
library(readxl)
library(leaflet)
library(rgdal)

SWEREF99TM <- CRS("+init=epsg:3006")
RT90 <- CRS("+init=epsg:4124")
WGS84 <- CRS("+init=epsg:4326")

# Define UI for data upload app ----
ui <- fluidPage(

  # App title ----
  titlePanel(title = h1("Convert GPS data", align = "center")),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Select a file ----
      fileInput("uploaded_file", "Choose excel File that holds GPS data",
                multiple = TRUE,
                accept = c(".xls", ".xlsx")),

      # Horizontal line ----
      tags$hr(),

      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),

      # Input: Select GPS system ----
      selectInput("gpsfrom", "Input GPS reference system",
                            c("RT90", "SWEREF99", "WGS84")),
                selectInput("gpsto", "Output GPS reference system",
                            c("RT90", "SWEREF99", "WGS84")),

      # Horizontal line ----
      tags$hr(),

      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(All = "all",
                               Head = "head"),
                   selected = "all"),

      # Select variables to display ----
      uiOutput("checkbox")

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      tabsetPanel(
        id = "dataset",
        tabPanel("FILE", DT::dataTableOutput("rendered_file")),
        tabPanel("Converted GPS coordinates", DT::dataTableOutput("df_conv")),
        tabPanel("Map", leafletOutput("map"))
        )
      )

  )
)

# Define server logic to read selected file ----
server <- function(input, output, session) {

  # Read file ----
  df <- reactive({
    req(input$uploaded_file)
    read_excel(input$uploaded_file$datapath,
               col_names = input$header,
               sheet = 1)
    })
  # Dynamically generate UI input when data is uploaded ----
  output$checkbox <- renderUI({
    checkboxGroupInput(inputId = "select_var",
                       label = "Select variables",
                       choices = names(df()))
  })

  # Select columns to print ----
  df_sel <- reactive({
    req(input$select_var)
    df_sel <- df() %>% select(input$select_var)
  })

  # Print data table ----
  output$rendered_file <- DT::renderDataTable({
    if(input$disp == "head") {
      head(df_sel())
    } else {
      df_sel()
    }
  })

    df_conv <- reactive({
        p1 <- SpatialPointsDataFrame(df_sel()[,c(2,1)], data = df_sel(), proj4string = SWEREF99TM)
        p2 <- spTransform(p1, WGS84)
        colnames(p2@coords) <- c("Longitude WGS84", "Latitude WGS84")
        p2@coords

    })

 # Generate converted values and print values on new tab
output$df_conv <- DT::renderDataTable({
    if(input$disp == "head") {
        head(df_conv())
    } else {
        df_conv()
    }
 })

    output$map <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$OpenStreetMap,
                             options = providerTileOptions(noWrap = TRUE)
                             ) %>%
            addMarkers(data = df_conv())
        })


}

# Create Shiny app ----
shinyApp(ui, server)
