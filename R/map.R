#' @export
mapUI <- function(id,
                  title = "Place",
                  geo_data,
                  group_vars,
                  geo_lab = "Geo boundaries",
                  groups_lab = "Group data by") {
  ns <- shiny::NS(id)

  geo_levels <- purrr::set_names(
    names(geo_data),
    unname(purrr::map_chr(geo_data, "level_name"))
  )

  bslib::card(
    min_height = 200,
    full_screen = TRUE,
    bslib::card_header(
      class = "d-flex justify-content-start align-items-center",
      tags$span(shiny::icon("globe-africa"), title, class = "pe-2"),
      shinyWidgets::dropMenu(
        actionButton(ns("dropdown"), icon = shiny::icon("sliders"), label = "options", class = "btn-sm"),
        options = dropMenuOptions(flip = TRUE),
        shinyWidgets::radioGroupButtons(
          ns("geo_level"),
          label = geo_lab,
          size = "sm",
          status = "outline-dark",
          choices = geo_levels
        ),
        tags$br(),
        selectInput(
          ns("var"),
          label = groups_lab,
          choices = c("N patients" = "n", group_vars),
          multiple = FALSE,
          selectize = FALSE,
          width = 200
        )
      )
    ),
    bslib::card_body(
      padding = 0,
      leaflet::leafletOutput(ns("map"))
    )
  )
}

#' @export
mapServer <- function(
    id,
    df_data,
    geo_data
  ) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      # ==========================================================================
      # DATA
      # ==========================================================================

      df_mod <- reactive({
        force_reactive(df_data)
      })

      geo_select <- reactive({
        if (length(geo_data) == 1) {
          geo_data[[1]]
        } else {
          geo_data[[input$geo_level]]
        }
      })

      rv <- reactiveValues()

      observe({
        geo_join <- geo_select()$join_by
        geo_col <- unname(geo_join)
        geo_col_sym <- rlang::sym(geo_col)
        geo_name_col <- geo_select()$name_var
        geo_name_col_sym <- rlang::sym(geo_name_col)
        geo_level_name <- geo_select()$level_name
        map_var <- input$var
        map_var_sym <- rlang::sym(map_var)
        sf <- geo_select()$sf

        # save as reactive values
        rv$geo_join <- geo_join
        rv$geo_col <- geo_col
        rv$geo_col_sym <- geo_col_sym
        rv$geo_name_col <- geo_name_col
        rv$geo_name_col_sym <- geo_name_col_sym
        rv$geo_level_name <- geo_level_name
        rv$map_var <- map_var
        rv$map_var_sym <- map_var_sym
        rv$sf <- sf
      })

      df_geo_counts <- reactive({
        if (input$var == "n") {
          var_lab <- "Patients"
          df_counts <- df_mod() %>%
            dplyr::count(!!rv$geo_col_sym, name = var_lab) %>%
            dplyr::mutate(total = .data[[var_lab]])
        } else {
          df_counts <- df_mod() %>%
            dplyr::mutate(!!rv$map_var_sym := forcats::fct_na_value_to_level(!!rv$map_var_sym, "(Missing)")) %>%
            janitor::tabyl(!!rv$geo_col_sym, !!rv$map_var_sym, show_missing_levels = FALSE) %>%
            janitor::adorn_totals("col", name = "total")
        }

        sf::st_drop_geometry(rv$sf) %>%
          dplyr::select(pcode, name = rv$geo_name_col, lon, lat) %>%
          dplyr::inner_join(df_counts, by = rv$geo_join) %>%
          dplyr::mutate(across(where(is.numeric), as.double)) %>%
          dplyr::mutate(across(where(is.double), ~if_else(is.na(.x), 0, .x)))
      })

      # ==========================================================================
      # MAP
      # ==========================================================================
      output$map <- leaflet::renderLeaflet({
        bbox <- sf::st_bbox(geo_data[[1]]$sf)

        leaflet::leaflet() %>%
          leaflet::fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]]) %>%
          leaflet::addMapPane(name = "boundaries", zIndex = 300) %>%
          leaflet::addMapPane(name = "choropleth", zIndex = 310) %>%
          leaflet::addMapPane(name = "circles", zIndex = 410) %>%
          leaflet::addMapPane(name = "region_highlight", zIndex = 420) %>%
          leaflet::addMapPane(name = "place_labels", zIndex = 320) %>%
          leaflet::addProviderTiles("CartoDB.PositronNoLabels", group = "Light") %>%
          leaflet::addProviderTiles(
            "CartoDB.PositronOnlyLabels",
            group = "Light",
            options = leaflet::leafletOptions(pane = "place_labels")
          ) %>%
          leaflet::addProviderTiles("OpenStreetMap", group = "OSM") %>%
          leaflet::addProviderTiles("OpenStreetMap.HOT", group = "OSM HOT") %>%
          leaflet::addScaleBar(position = "bottomright", options = leaflet::scaleBarOptions(imperial = FALSE)) %>%
          leaflet.extras::addResetMapButton() %>%
          leaflet::addLayersControl(
            baseGroups = c("Light", "OSM", "OSM HOT"),
            overlayGroups = c("Boundaries", "Taux d'attaque"),
            position = "topleft"
          ) %>%
          leaflet::addMiniMap(toggleDisplay = TRUE, minimized = FALSE, position = "bottomleft")
      })

      observe({
        boundaries <- rv$sf
        leaflet::leafletProxy("map", session) %>%
          leaflet::clearGroup("Boundaries") %>%
          leaflet::addPolygons(
            data = boundaries,
            stroke = TRUE,
            color = "grey",
            weight = 1,
            fillOpacity = 0,
            label = boundaries[[rv$geo_name_col]],
            group = "Boundaries",
            options = leaflet::pathOptions(pane = "boundaries")
          )
      }) %>% bindEvent(rv$sf)

      # map circles/pies ===========================================
      observe({
        df_map <- df_geo_counts()

        leaflet::leafletProxy("map", session) %>% clearMinicharts()

        if (isTruthy(nrow(df_map) > 0)) {
          chartData <- df_map %>% select(-pcode, -name, -lon, -lat, -total)
          pie_width <- 60 * sqrt(df_map$total) / sqrt(max(df_map$total))

          leaflet::leafletProxy("map", session) %>%
            addMinicharts(
              df_map$lon,
              df_map$lat,
              layerId = df_map$name,
              chartdata = chartData,
              opacity = .9,
              # fillColor = epi_pals()$pal10[1],
              # colorPalette = epi_pals()$pal10,
              legend = TRUE,
              showLabels = TRUE,
              type = "pie",
              width = pie_width
            )
        }
      }) %>% bindEvent(df_geo_counts())
    }
  )
}
