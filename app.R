required_packages <- c("leaflet", "rerddap", "shiny", "surveyjoin")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Missing required packages: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

erddap_url <- "https://coastwatch.pfeg.noaa.gov/erddap/"
dataset_id <- "ncdcOisst21Agg_LonPM180"
sst_variable <- "sst"

get_sst_info <- local({
  cache <- NULL

  function() {
    if (is.null(cache)) {
      cache <<- rerddap::info(dataset_id, url = erddap_url)
    }

    cache
  }
})

get_nwfsc_combo_grid <- local({
  cache <- NULL

  function() {
    if (is.null(cache)) {
      cache <<- subset(
        surveyjoin::nwfsc_grid,
        survey == "NWFSC.Combo",
        select = c(
          "lon",
          "lat",
          "depth_m",
          "survey_domain_year",
          "split_state"
        )
      )
    }

    cache
  }
})

get_available_dates <- function() {
  time_metadata <- get_sst_info()$alldata$time
  actual_range <- time_metadata$value[
    time_metadata$attribute_name == "actual_range"
  ][1]

  if (is.na(actual_range) || !nzchar(actual_range)) {
    stop("Unable to determine the SST date range from ERDDAP metadata.")
  }

  time_bounds <- as.numeric(trimws(strsplit(actual_range, ",")[[1]]))
  date_bounds <- as.Date(
    as.POSIXct(time_bounds, origin = "1970-01-01", tz = "UTC")
  )

  seq.Date(date_bounds[1], date_bounds[2], by = "day")
}

normalize_sst_grid <- function(sst_grid) {
  names(sst_grid) <- tolower(names(sst_grid))

  lat_name <- intersect(c("latitude", "lat"), names(sst_grid))[1]
  lon_name <- intersect(c("longitude", "lon"), names(sst_grid))[1]

  if (is.na(lat_name) || is.na(lon_name) || !sst_variable %in% names(sst_grid)) {
    stop("Unexpected SST response format returned by ERDDAP.")
  }

  normalized <- data.frame(
    lon = as.numeric(sst_grid[[lon_name]]),
    lat = as.numeric(sst_grid[[lat_name]]),
    sst = as.numeric(sst_grid[[sst_variable]])
  )

  normalized[!is.na(normalized$sst), , drop = FALSE]
}

nearest_index <- function(values, targets) {
  vapply(
    targets,
    function(target) {
      which.min(abs(values - target))
    },
    integer(1)
  )
}

join_sst_to_grid <- function(grid_points, sst_grid) {
  lat_values <- sort(unique(sst_grid$lat))
  lon_values <- sort(unique(sst_grid$lon))

  if (length(lat_values) == 0 || length(lon_values) == 0) {
    stop("No SST values were returned for the selected date.")
  }

  matched_lat <- lat_values[nearest_index(lat_values, grid_points$lat)]
  matched_lon <- lon_values[nearest_index(lon_values, grid_points$lon)]

  sst_lookup <- sst_grid
  sst_lookup$key <- paste(sst_lookup$lon, sst_lookup$lat, sep = "|")

  grid_points$sst_lon <- matched_lon
  grid_points$sst_lat <- matched_lat
  grid_points$key <- paste(grid_points$sst_lon, grid_points$sst_lat, sep = "|")
  grid_points$sst <- sst_lookup$sst[match(grid_points$key, sst_lookup$key)]
  grid_points$key <- NULL

  grid_points
}

fetch_sst_for_date <- function(selected_date) {
  survey_grid <- get_nwfsc_combo_grid()
  sst_grid <- rerddap::griddap(
    get_sst_info(),
    url = erddap_url,
    time = rep(as.character(as.Date(selected_date)), 2),
    latitude = range(survey_grid$lat, na.rm = TRUE),
    longitude = range(survey_grid$lon, na.rm = TRUE),
    fields = sst_variable,
    fmt = "csv"
  )

  join_sst_to_grid(survey_grid, normalize_sst_grid(sst_grid))
}

ui <- shiny::fluidPage(
  shiny::titlePanel("NWFSC Survey Grid SST"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::uiOutput("date_selector"),
      shiny::textOutput("mean_sst"),
      shiny::textOutput("status")
    ),
    shiny::mainPanel(
      leaflet::leafletOutput("map", height = 700)
    )
  )
)

server <- function(input, output, session) {
  available_dates <- shiny::reactive({
    tryCatch(
      {
        list(dates = get_available_dates(), error = NULL)
      },
      error = function(err) {
        list(dates = NULL, error = conditionMessage(err))
      }
    )
  })

  output$date_selector <- shiny::renderUI({
    date_result <- available_dates()

    if (is.null(date_result$dates)) {
      return(shiny::helpText("Available ERDDAP dates could not be loaded."))
    }

    shiny::dateInput(
      "selected_date",
      "SST date",
      value = max(date_result$dates, na.rm = TRUE),
      min = min(date_result$dates, na.rm = TRUE),
      max = max(date_result$dates, na.rm = TRUE),
      format = "yyyy-mm-dd"
    )
  })

  sst_points <- shiny::reactive({
    if (is.null(input$selected_date)) {
      return(list(data = NULL, error = NULL))
    }

    tryCatch(
      {
        list(data = fetch_sst_for_date(input$selected_date), error = NULL)
      },
      error = function(err) {
        list(data = NULL, error = conditionMessage(err))
      }
    )
  })

  output$mean_sst <- shiny::renderText({
    if (is.null(input$selected_date)) {
      return("Mean SST: unavailable")
    }

    result <- sst_points()

    if (is.null(result$data)) {
      return("Mean SST: unavailable")
    }

    paste0(
      "Mean SST on ",
      as.character(as.Date(input$selected_date)),
      ": ",
      sprintf("%.2f", mean(result$data$sst, na.rm = TRUE)),
      " \u00b0C"
    )
  })

  output$status <- shiny::renderText({
    date_result <- available_dates()

    if (!is.null(date_result$error)) {
      return(paste("Unable to load SST metadata:", date_result$error))
    }

    result <- sst_points()

    if (is.null(result$error)) {
      return("")
    }

    paste("Unable to load SST data:", result$error)
  })

  output$map <- leaflet::renderLeaflet({
    result <- sst_points()
    map <- leaflet::leaflet() |>
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
      leaflet::setView(lng = -124.5, lat = 44.5, zoom = 5)

    if (is.null(result$data)) {
      return(map)
    }

    palette <- leaflet::colorNumeric(
      palette = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"),
      domain = result$data$sst,
      na.color = "#808080"
    )

    leaflet::addCircleMarkers(
      map,
      data = result$data,
      lng = ~lon,
      lat = ~lat,
      radius = 4,
      stroke = FALSE,
      fillOpacity = 0.85,
      fillColor = ~palette(sst),
      popup = ~paste0(
        "<strong>Date:</strong> ", as.Date(input$selected_date),
        "<br><strong>SST:</strong> ", sprintf("%.2f", sst), " \u00b0C",
        "<br><strong>Depth:</strong> ", depth_m, " m",
        "<br><strong>Survey year:</strong> ", survey_domain_year,
        "<br><strong>Split state:</strong> ", split_state
      )
    ) |>
      leaflet::addLegend(
        position = "bottomright",
        pal = palette,
        values = result$data$sst,
        title = "SST (\u00b0C)"
      )
  })
}

shiny::shinyApp(ui, server)
