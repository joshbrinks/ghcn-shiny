library(shiny)
library(bslib)
library(leaflet)
library(plotly)
library(dplyr)
library(ghcn)  # Your custom package
library(DT)

# Define the API check function
check_api_on_startup <- function() {
  base_url <- "https://www.ncei.noaa.gov/cdo-web/api/v2"
  
  tryCatch({
    response <- httr::GET(
      url = file.path(base_url, "stations"),
      query = list(
        datasetid = "GHCND",
        limit = 5
      ),
      httr::add_headers("token" = "jNgLeoBbPesCsXNbybBUwfTKPuXktBGS"),
      httr::timeout(20)
    )
    
    if (httr::status_code(response) != 200) {
      stop(paste("API request failed with status code:", httr::status_code(response)))
    }
    
  }, error = function(e) {
    error_message <- if (inherits(e, "timeout")) {
      "The server was unresponsive and timed out after 20 seconds."
    } else {
      paste("The server failed with the following error:", e$message)
    }
    
    # Store the error message to be displayed later
    options(api_error_message = error_message)
  })
}

ui <- bslib::page_sidebar(
    title = shiny::tags$span(style = "color: white; font-weight: bold;", "iSci GHCN-Daily Data Portal"),
    theme = bslib::bs_theme(
        bg = "#FFFFFF",
        fg = "#000000",
        primary = "#3398cb"
    ),
    sidebar = bslib::sidebar(
        width = 300,
        
        # Sidebar inputs
        shiny::numericInput("radius", "Search Radius (km)", value = 10, min = 1, max = 500),
        shiny::numericInput("limit", "Max Number of Stations", value = 10, min = 1, max = 50),
        shiny::actionButton("search_stations", "Search Stations", class = "btn-custom"),
        shiny::br(),
        shiny::br(),
        shiny::dateRangeInput("date_range", "Date Range", 
                              start = Sys.Date() - 365, end = Sys.Date()),
        shiny::selectizeInput("variables", "Select Variables", 
                              choices = NULL,
                              multiple = TRUE,
                              options = list(maxItems = 5)),
        shiny::actionButton("fetch_data", "Fetch Data", class = "btn-custom")
    ),
    
    # Main content
    bslib::layout_column_wrap(
        width = 1,
        heights_equal = "row",
        # Map and Station Info Card
        bslib::layout_columns(
            col_widths = c(8, 4),
            bslib::card(
                full_screen = TRUE,
                bslib::card_header("Station Map: Click to Establish Search Radius"),
                leaflet::leafletOutput("station_map", height = 800)
            ),
            bslib::card(
                full_screen = TRUE,
                bslib::card_header("Station Information"),
                shiny::uiOutput("station_info")
            )
        ),
        # Plots and Data
        bslib::navset_card_tab(
            full_screen = TRUE,
            bslib::nav_panel("Plots", plotly::plotlyOutput("data_plot", height = 800)),
            bslib::nav_panel("Data", DT::dataTableOutput("data_table"))
        )
    ),
    
    # Custom CSS
    tags$style(HTML("
    .bslib-page-sidebar > .navbar { background-color: #3398cb !important; }
    .bslib-sidebar-layout > .sidebar { background-color: #3398cb !important; }
    .bslib-sidebar-layout > .sidebar { color: white !important; }
    .bslib-sidebar-layout > .sidebar .form-control { color: black !important; background-color: white !important; }
    .bslib-sidebar-layout > .sidebar .selectize-input { color: black !important; background-color: white !important; }
    .btn-custom { 
      background-color: #ed5535 !important; 
      border-color: #ed5535 !important; 
      color: white !important;
    }
    .btn-custom:hover {
      background-color: #d64a2e !important;
      border-color: #d64a2e !important;
    }
  "))
)

server <- function(input, output, session) {
  
  # server status startup check
  observe({
    if (!is.null(getOption("api_error_message"))) {
      shinyalert::shinyalert(
        title = "API Error",
        text = getOption("api_error_message"),
        type = "error"
      )
      # Clear the error message after displaying
      options(api_error_message = NULL)
    }
  })
    
    # Reactive values for storing the clicked point and selected station
    clicked_point <- shiny::reactiveVal(NULL)
    selected_station <- shiny::reactiveVal(NULL)
    
    # Initialize the map
    output$station_map <- leaflet::renderLeaflet({
        leaflet::leaflet() %>%
            leaflet::addTiles() %>%
            leaflet::addAwesomeMarkers(lng = -98.5795, lat = 39.8283, 
                                       icon = leaflet::makeAwesomeIcon(icon = 'flag', markerColor = 'red', iconColor = 'white'),
                                       label = "Click to select a location") %>%
            leaflet::setView(lng = -98.5795, lat = 39.8283, zoom = 4)  # Center on US
    })
    
    # Update clicked point when map is clicked
    shiny::observeEvent(input$station_map_click, {
        click <- input$station_map_click
        clicked_point(c(lat = click$lat, lng = click$lng))
        
        leaflet::leafletProxy("station_map") %>%
            leaflet::clearMarkers() %>%
            leaflet::addAwesomeMarkers(lng = click$lng, lat = click$lat, 
                                       icon = leaflet::makeAwesomeIcon(icon = 'flag', markerColor = 'red', iconColor = 'white'),
                                       label = "Selected location")
    })
    
    # Reactive expression for getting stations
    stations <- shiny::eventReactive(input$search_stations, {
      point <- clicked_point()

      shiny::req(point, input$radius, input$limit)
      
      result <- ghcn::get_ghcn_daily_stations(
        lat = point["lat"],
        lon = point["lng"],
        radius = input$radius,
        limit = input$limit,
        token = "jNgLeoBbPesCsXNbybBUwfTKPuXktBGS"
      )
      
      # Sort stations by distance
      result %>%
        dplyr::mutate(distance = sqrt((latitude - point["lat"]) ^
                                        2 + (longitude - point["lng"]) ^ 2)) %>%
        dplyr::arrange(distance)
      
    })
    
    # Update map with station markers
    shiny::observeEvent(stations(), {
        req(stations())
        leaflet::leafletProxy("station_map") %>%
            leaflet::clearMarkers() %>%
            leaflet::addAwesomeMarkers(data = stations(),
                                       ~longitude, ~latitude,
                                       icon = leaflet::makeAwesomeIcon(icon = 'info-sign', markerColor = 'blue', iconColor = 'white'),
                                       popup = ~paste(name, "<br>", id),
                                       label = ~name,
                                       layerId = ~id) %>%
            leaflet::addAwesomeMarkers(lng = clicked_point()["lng"], lat = clicked_point()["lat"], 
                                       icon = leaflet::makeAwesomeIcon(icon = 'flag', markerColor = 'red', iconColor = 'white'),
                                       label = "Selected location")
    })
    
    # Update selected station when a marker is clicked
    shiny::observeEvent(input$station_map_marker_click, {
        click <- input$station_map_marker_click
        if (!is.null(click$id)) {
            selected_station(stations() %>% dplyr::filter(id == click$id))
            # Update the variables selectInput
            available_vars <- unlist(strsplit(selected_station()$available_vars, ", "))
            shiny::updateSelectizeInput(session, "variables", choices = available_vars, selected = character(0))
        }
    })
    
    # Render station information card
    output$station_info <- shiny::renderUI({
      station <- selected_station()
      if (is.null(station)) {
        return(shiny::p("'Search Stations' after selecting a map location. Then click on a station marker to view its information."))
      }
      
      # Split available variables
      available_vars <- strsplit(station$available_vars, ", ")[[1]]
      
      shiny::tagList(
        shiny::h3(station$name),
        shiny::tags$div(
          style = "line-height: 1.2; margin-bottom: 10px;",
          shiny::p(shiny::strong("ID: "), station$id),
          shiny::p(shiny::strong("Date Range: "), paste(station$mindate, "to", station$maxdate)),
          shiny::p(shiny::strong("Distance: "), sprintf("%.2f km", station$distance * 111)),  # Approximate conversion to km
          shiny::p(shiny::strong("Latitude: "), station$latitude),
          shiny::p(shiny::strong("Longitude: "), station$longitude),
          shiny::p(shiny::strong("Elevation: "), station$elevation, " m")
        ),
        shiny::h4("Available Variables:"),
        shiny::tags$div(
          style = "max-height: 200px; overflow-y: auto; border: 1px solid #ddd; padding: 10px;",
          shiny::tags$ul(
            style = "padding-left: 20px; margin-top: 5px;",
            lapply(available_vars, function(var) {
              description <- ghcn::ghcn_daily_datatypes$description[ghcn::ghcn_daily_datatypes$datatype == var]
              shiny::tags$li(
                style = "margin-bottom: 5px;",
                shiny::strong(var), ": ", description
              )
            })
          )
        )
      )
    })
    
    # Reactive expression for getting data
    station_data <- shiny::eventReactive(input$fetch_data, {
        shiny::req(selected_station(), input$variables, input$date_range)
        station <- selected_station()
        
        # Check if selected date range is within the station's data range
        station_start <- as.Date(station$mindate)
        station_end <- as.Date(station$maxdate)
        selected_start <- input$date_range[1]
        selected_end <- input$date_range[2]
        
        if (selected_start < station_start || selected_end > station_end) {
            shiny::showModal(shiny::modalDialog(
                title = "Date Range Error",
                "The selected date range is outside the available data range for this station. 
        Please adjust your date selection or choose a different station.",
                easyClose = TRUE,
                footer = NULL
            ))
            return(NULL)
        }
        
        ghcn::get_ghcn_daily_data(station_id = station$id,
                                  start_date = selected_start,
                                  end_date = selected_end,
                                  token = "jNgLeoBbPesCsXNbybBUwfTKPuXktBGS",
                                  datatype = input$variables)
    })
    
    # Render the data plot
    output$data_plot <- plotly::renderPlotly({
        shiny::req(station_data(), selected_station())
        p <- ghcn::visualize_ghcn_daily_data(station_data(), selected_station()$name)
        plotly::ggplotly(p)
    })
    
    # Render the data table
    output$data_table <- DT::renderDataTable({
        shiny::req(station_data())
        DT::datatable(station_data(), 
                      options = list(pageLength = 25,
                                     order = list(list(0, 'desc')),  # Sort by first column (date) in descending order
                                     columnDefs = list(list(
                                         targets = "_all",
                                         render = DT::JS(
                                             "function(data, type, row, meta) {",
                                             "return type === 'display' && data != null && data !== ''",
                                             "  ? '<span style=\"font-size:0.7em;\">' + data + '</span>'",
                                             "  : data;",
                                             "}"
                                         )
                                     ))))
    })
}

shiny::shinyApp(ui, server, onStart = check_api_on_startup)