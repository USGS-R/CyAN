library(shinydashboard)
library(shiny)
library(shinyFiles)
library(leaflet)
library(htmltools)
library(dplyr)
library(fs)

options(shiny.maxRequestSize=2000*1024^2)

load(system.file("shiny", "cyan-app", "database", "locationIndex.RData", package = "CyAN"))

null_if_blank_as_num <- function(x) {
  if(x == "") {
    return(NULL)
  } else {
    return(as.numeric(x))
  }
}

ui <- dashboardPage(

  dashboardHeader(title = "CyAN"),

  dashboardSidebar(
    sidebarMenu(id = "sidebar",
      menuItem("Configure", tabName = "db_configuration"),
      menuItem("Map", tabName = "map_screen"),
      menuItem("Bivariate Plot", tabName = "bivariate_plot"),
      conditionalPanel("input.sidebar == 'bivariate_plot'",
        uiOutput("bivariate_parameter_controls"),
        checkboxInput("biv_map_limit", label = "Limit to map bounds", value=TRUE),
        sliderInput("biv_years", label = "Years:", min = 1975, max = 2019,
                    value = c(1975, 2016), sep = ""),
        selectInput("biv_color", "Highlight", choices=c("Parameter 1 methods" = "METHOD_ID.1",
                                                       "Parameter 2 methods" = "METHOD_ID.2")),
        uiOutput("method_highlight_controls"),
        checkboxGroupInput("log_biv", "Log Scale", choices=c("x", "y"))
      )
    )

  ),

  dashboardBody(
    tabItems(
      tabItem(tabName = "db_configuration",
        h3("Connect to database"),
        shinyFilesButton('db_file', label = "Database file", title = "CyAN Database", multiple = FALSE)
      ),
      tabItem(tabName = "map_screen",
        div(class = "outer",
          tags$head(
            includeCSS("styles.css"),
            includeScript("gomap.js")
          ),
          leafletOutput("map", width = "100%", height = "100%"),
          absolutePanel(id="controls", class="panel panel-default", fixed=TRUE,
                        draggable=TRUE, top=60, left="auto", right=20, bottom="auto",
                        width=330, height="auto",

                        helpText(" "),
                        radioButtons("parm_logic", "Parameters:", choices=c("At least one", "All of")),
                        helpText(" "),
                        uiOutput("filter_points_parameter"),
                        radioButtons("tiles", "View layer:", choices=c("NHD", "LandsatLook", "Streets"),
                                     selected="Streets", inline=TRUE),
                        actionButton("show_points", "Show points")
          ),
          absolutePanel(id="querycontrols", class="panel panel-default", fixed=TRUE,
                        draggable=TRUE, top=60, left=20, right="auto", bottom="auto",
                        width=400, height="auto",
                        textInput("download_filename", "Output file name (no extension"),
                        checkboxInput("fill_bounds", "Fill bounds from map", value = FALSE),
                        fluidRow(
                          column(4),
                          column(4, uiOutput("n_lat_box")),
                          column(4)
                        ),
                        fluidRow(
                          column(1),
                          column(4, uiOutput("w_long_box")),
                          column(2),
                          column(4, uiOutput("e_long_box")),
                          column(1)
                        ),
                        fluidRow(
                          column(4),
                          column(4, uiOutput("s_lat_box")),
                          column(4)
                        ),
                        fluidRow(
                          column(1),
                          column(10,
                                 sliderInput("years", label = "Years:", min = 1975, max = 2019,
                                             value = c(1975, 2016), sep = ""),
                                 uiOutput("parameter_choices")
                          ),
                          column(1)
                        ),
                        fluidRow(
                          column(5, textInput("tier", "Tier:", value = "4.0")),
                          column(1),
                          column(5, uiOutput("state_choices"))
                        ),
                        fluidRow(
                          column(6, checkboxInput("add_GMT", "Add GMT datetime"),
                                 value = FALSE),
                          column(6, checkboxInput("add_solar_noon", "Add solar noon flag",
                                                  value = FALSE))
                        ),
                        downloadButton("download_data")

          )

        )
      ),
      tabItem(tabName = "bivariate_plot",
        box(
          plotOutput("bivariate_plot", brush = brushOpts(id = "zoom_brush", resetOnNew = FALSE),
                     height="700px"),
        width = 6),
        box(
          plotOutput("zoomed_bivariate_plot", brush = brushOpts(id = "flag_brush", resetOnNew = FALSE),
                     height = "700px")
        )
      )
    )
  )


)
####################################################################################################
server <- function(input, output) {

  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  shinyFileChoose(input, "db_file", filetypes = c("", "db"), roots = volumes)

  db_path <- reactive({
    parseFilePaths(volumes, input$db_file)
  })

  cyan_connection <- reactive({

    file <- db_path()
    if(nrow(file) == 0)
      return(NULL)

    db_path <- file$datapath
    cyan <- connect_cyan(db_path)
    cyan
  })

  parameter_index <- reactive({

    if(is.null(cyan_connection()))
      return(NULL)

    parameters <- generate_parameter_index(cyan_connection())
    parameters

  })

  location_index <- reactive({
    index
  })

  output$filter_points_parameter <- renderUI({

    if(is.null(cyan_connection()))
      return(NULL)

    choices <- parameter_index()$PARAMETER_ID
    names(choices) <- parameter_index()$SHORT_NAME

    selectizeInput("parms_s", label = NULL, choices = choices, selected = NULL, multiple = TRUE)
  })

  output$map <- renderLeaflet({

    pts_init <- data.frame(LOCATION_NAME = "KSWSC", LATITUDE = 38.0, LONGITUDE = -95.0)

    leaflet() %>%
      addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
               attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
      addCircles(data = pts_init, layerId = "KSWSC") %>%
      clearShapes() %>%
      fitBounds(-141.855, 23.483, -57.48, 53.801)

  })

  zoomWindow <- reactive({
    if(is.null(input$map_bounds))
      return(c("", "", "", ""))
    bounds <- input$map_bounds
    zw <- as.character(c(bounds$north, bounds$east, bounds$south, bounds$west))
    zw
  })

  mapData <- reactive({

    input$show_points
    isolate({
      selected_parameters <- input$parms_s
      zoom <- as.numeric(zoomWindow())
      points <- location_index()
    })

    points <- location_index()

    if(!is.null(selected_parameters)) {
      if(input$parm_logic == "All of") {
        s <- lapply(selected_parameters, function(x, ind) {
          sts <- filter(ind, PARAMETER_ID == x) %>%
            select(-PARAMETER_ID)
        }, ind = points)
        mapData <- Reduce(intersect, s)
      } else {
        mapData <- filter(points, PARAMETER_ID %in% selected_parameters) %>%
          distinct()
      }
    } else {
      mapData <- select(points, LOCATION_NAME, LATITUDE, LONGITUDE) %>%
        distinct
    }

    if(!all(is.na(zoom))) {
      mapData <- filter(mapData,
                        LATITUDE >= zoom[3], LATITUDE <= zoom[1],
                        LONGITUDE <= zoom[2], LONGITUDE >= zoom[4])
    }

    mapData

  })

  observe({

    if(input$tiles=="NHD") {
      leafletProxy("map") %>% clearTiles() %>% addWMSTiles(
        "http://basemap.nationalmap.gov/arcgis/services/USGSHydroCached/MapServer/WMSServer?",
        # "http://services.nationalmap.gov/arcgis/services/nhd/mapserver/WMSServer?",
        layers = "0",
        options = WMSTileOptions(format = "image/bmp", transparent = FALSE),
        attribution = "")
    } else if(input$tiles=="LandsatLook") {
      leafletProxy("map") %>% clearTiles() %>% addWMSTiles(
        "https://landsatlook.usgs.gov/arcgis/services/LandsatLook/ImageServer/WMSServer?",
        options = WMSTileOptions(format="image/png", transparaent = TRUE),
        layers = "0")
    } else if(input$tiles=="Streets") {
      leafletProxy("map") %>% clearTiles() %>% addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>')
    }

  })

  observe({

    leafletProxy("map") %>% clearShapes() %>%
      addCircles(data=mapData(), popup = ~htmlEscape(LOCATION_NAME), color='orangered',
                 fillColor='orangered', fillOpacity=0.9, opacity=0.9, radius=15)
  })

  output$n_lat_box <- renderUI({

    if(input$fill_bounds) {
      v <- zoomWindow()[1]
    } else {
      v <- ""
    }
    textInput("n_lat", "North Latitude", value=v)

  })

  output$w_long_box <- renderUI({

    if(input$fill_bounds) {
      v <- zoomWindow()[4]
    } else {
      v <- ""
    }
    textInput("w_long", "West Longitude", value=v)

  })

  output$e_long_box <- renderUI({

    if(input$fill_bounds) {
      v <- zoomWindow()[2]
    } else {
      v <- ""
    }
    textInput("e_long", "East Longitude", value=v)

  })

  output$s_lat_box <- renderUI({

    if(input$fill_bounds) {
      v <- zoomWindow()[3]
    } else {
      v <- ""
    }
    textInput("s_lat", "South Latitude", value=v)

  })

  output$parameter_choices <- renderUI({

    if(is.null(parameter_index()))
      return(NULL)

    choices <- parameter_index()$PARAMETER_ID
    names(choices) <- parameterIndex$SHORT_NAME

    selectInput("parms", "Parameters:",  choices = choices, multiple = TRUE)

  })

  output$state_choices <- renderUI({

    states <- state.abb[!(state.abb %in% c("AK", "HI"))]
    states <- c("All", states)
    selectInput("state", "State:", choices = states,
                selectize = TRUE, multiple = TRUE, selected = "All")


  })

  output$download_data <- downloadHandler(
    filename = function() {
      paste0(input$download_filename, ".csv")
    },
    content = function(file) {

      n_lat <- null_if_blank_as_num(input$n_lat)
      s_lat <- null_if_blank_as_num(input$s_lat)
      e_long <- null_if_blank_as_num(input$e_long)
      w_long <- null_if_blank_as_num(input$w_long)
      years <- input$years[1]:input$years[2]
      parameters <- input$parms
      minimum_tier <- null_if_blank_as_num(input$tier)

      if("All" %in% input$states) {
        states <- NULL
      } else {
        states <- input$states[input$states != "All"]
      }

      download_notification <- showNotification("Preparing data...", duration = NULL)

      output <- get_cyan_data(cyan_connection = cyan_connection(),
                              collect = TRUE,
                              north_latitude = n_lat, south_latitude = s_lat,
                              east_longitude = e_long, west_longitude = w_long,
                              years = years,
                              parameters = parameters)

      if(input$add_GMT) {
        showNotification("Adding GMT...", id = download_notification, duration = NULL)
        output <- add_GMT_time(output)
      }

      if(input$add_solar_noon) {
        showNotification("Adding solar noon...", id = download_notification, duration = NULL)
        output <- add_solar_noon(output)
      }

      removeNotification(id = download_notification)

      write.csv(output, file)

    }
  )

  output$bivariate_parameter_controls <- renderUI({

    if(is.null(parameter_index()))
      return(NULL)

    choices <- parameter_index()$PARAMETER_ID
    names(choices) <- parameterIndex$SHORT_NAME
    tagList(
      selectInput("biv_parm_1", "Parameter 1 (x-axis):", choices = c("None" = "None", choices)),
      selectInput("biv_parm_2", "Parameter 2 (y-axis):", choices = c("None" = "None", choices))
    )
  })

  output$method_highlight_controls <- renderUI({
    if(is.null(input$biv_parm_1))
      return(NULL)

    if(input$biv_parm_1 != "None" & input$biv_parm_2 != "None") {
      methods <- unique(bivariate_data()[,input$biv_color])
      selectizeInput("method_highlight", "Methods", choices=c("None", methods), multiple=TRUE, selected="None")
    }
  })

  bivariate_data <- reactive({

    if(is.null(input$biv_parm_1) || is.null(input$biv_parm_2))
      return(NULL)
    if("None" %in% c(input$biv_parm_1, input$biv_parm_2))
      return(NULL)

    if(input$biv_map_limit) {
      north_latitude <- null_if_blank_as_num(input$n_lat)
      south_latitude <- null_if_blank_as_num(input$s_lat)
      west_longitude <- null_if_blank_as_num(input$w_long)
      east_longitude <- null_if_blank_as_num(input$e_long)
    } else {
      north_latitude <- south_latitude <- east_longitude <- west_longitude <- NULL
    }

    data_notification <- showNotification("Getting data...", type = "message", duration = NULL)

    data <- get_bivariate(cyan_connection(), input$biv_parm_1, input$biv_parm_2,
                          collect = TRUE,
                          north_latitude = north_latitude, south_latitude = south_latitude,
                          west_longitude = west_longitude, east_longitude = east_longitude,
                          years = input$biv_years)

    removeNotification(id = data_notification)

    data

  })

  bivariate_flagged <- reactive({

    flagged <- find_flagged(cyan_connection(), "MANBIV")
    flagged

  })

  output$bivariate_plot <- renderPlot({

    if(is.null(bivariate_data()))
      return(NULL)

    log_1 <- "x" %in% input$log_biv
    log_2 <- "y" %in% input$log_biv
    method_highlight <- input$method_highlight
    flagged_results <- bivariate_flagged()

    print(log_1)
    print(log_2)
    print(method_highlight)
    print(head(flagged_results))
    print(head(bivariate_data()))

    plot_notification <- showNotification("Plotting...", duration = NULL)

    plot <- plot_bivariate(bivariate_data(),
                           log_1 = log_1, log_2 = log_2,
                           method_highlight = method_highlight,
                           flagged_results = flagged_results)

    removeNotification(id = plot_notification)

    plot

  })

  zoom_range <- reactiveValues(x = NULL, y = NULL)

  observe({
    brush <- input$zoom_brush
    if (!is.null(brush)) {
      zoom_range$x <- c(brush$xmin, brush$xmax)
      zoom_range$y <- c(brush$ymin, brush$ymax)

    } else {
      zoom_range$x <- NULL
      zoom_range$y <- NULL
    }
  })

  output$zoomed_bivariate_plot <- renderPlot({

    if(is.null(bivariate_data()))
      return(NULL)

    log_1 <- "x" %in% input$log_biv
    log_2 <- "y" %in% input$log_biv
    method_highlight <- input$method_highlight
    flagged_results <- bivariate_flagged()
    range_1 <- zoom_range$x
    range_2 <- zoom_range$y

    plot <- plot_bivariate(bivariate_data(),
                           log_1 = log_1, log_2 = log_2,
                           method_highlight = method_highlight,
                           flagged_results = flagged_results,
                           range_1 = range_1, range_2 = range_2)
    plot

  })

  flag_range <- reactiveValues(x = NULL, y = NULL)

  observe({
    brush <- input$flag_brush
    if (!is.null(brush)) {
      flag_range$x <- c(brush$xmin, brush$xmax)
      flag_range$y <- c(brush$ymin, brush$ymax)

    } else {
      flag_range$x <- NULL
      flag_range$y <- NULL
    }
  })

}
#-------------------------------------------------------------------------------------------
# Run the application
shinyApp(ui = ui, server = server)

