#' Place module
#'
#' Visualise geographical distribution across multiple administrative boundaries on an interactive leaflet map.
#'
#' @rdname place
#'
#' @param id Module id. Must be the same in both the UI and server function to link the two.
#' @param geo_data A list of spatial sf dataframes with information for different geographical levels.
#' @param group_vars named character vector of categorical variables for the data grouping input. Names are used as variable labels.
#' @param title The title for the card.
#' @param geo_lab The label for the geographical level selection.
#' @param groups_lab The label for the group data by selection.
#' @param n_lab The label for the raw count variable.
#' @param full_screen Add button to card to with the option to enter full screen mode?
#'
#' @return A [bslib::card] UI element with options and download button and a leaflet map.
#' @export
#' @example inst/examples/docs/app.R
place_ui <- function(
    id,
    geo_data,
    group_vars,
    title = "Place",
    geo_lab = "Geo boundaries",
    groups_lab = "Group data by",
    n_lab = "N patients",
    full_screen = TRUE
) {
  ns <- shiny::NS(id)

  geo_levels <- purrr::set_names(
    names(geo_data),
    unname(purrr::map_chr(geo_data, "level_name"))
  )

  tagList(
    use_epishiny(),
    bslib::card(
      # min_height = 300,
      full_screen = full_screen,
      bslib::card_header(
        class = "d-flex justify-content-start align-items-center",
        tags$span(shiny::icon("globe-africa"), title, class = "pe-2"),
        shinyWidgets::dropMenu(
          actionButton(
            ns("dropdown"),
            icon = shiny::icon("sliders"),
            label = "options",
            class = "btn-sm pe-2 me-2"
          ),
          options = shinyWidgets::dropMenuOptions(flip = TRUE),
          bslib::layout_columns(
            col_widths = 12,
            shinyWidgets::radioGroupButtons(
              ns("geo_level"),
              label = geo_lab,
              size = "sm",
              status = "outline-dark",
              choices = geo_levels
            ),
            selectInput(
              ns("var"),
              label = groups_lab,
              choices = c(purrr::set_names("n", n_lab), group_vars),
              multiple = FALSE,
              selectize = FALSE,
              width = 200
            ),
            sliderInput(
              ns("circle_size_mult"),
              label = "Circle size multiplyer",
              min = 1,
              max = 10,
              value = 6,
              step = 1,
              width = 200
            )
          )
        ),
        downloadButton(
          ns("dl"),
          label = "Download",
          icon = shiny::icon("camera"),
          class = "btn-sm pe-2 me-2"
        )
      ),
      bslib::card_body(
        padding = 0,
        leaflet::leafletOutput(ns("map"))
      )
    )
  )
}

#' @param df_ll Data frame or tibble of patient level linelist data. Can be either a shiny reactive or static dataset.
#' @param export_width The width of the exported map image.
#' @param export_height The height of the exported map image.
#' @param filter_info If contained within an app using [filter_server()], supply the `filter_info` element
#'   returned by that function here as a shiny reactive to add filter information to chart exports.
#'
#' @rdname place
#'
#' @return The server function returns the leaflet map's shape click information as a list.
#'
#' @export
place_server <- function(
    id,
    df_ll,
    geo_data,
    group_vars,
    n_lab = "N patients",
    export_width = 1200,
    export_height = 650,
    filter_info = shiny::reactiveVal()
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # loading spinner for map export
      w_map <- waiter::Waiter$new(
        id = ns("map"),
        html = waiter::spin_3(),
        color = waiter::transparent(alpha = 0)
      )

      # ==========================================================================
      # DATA
      # ==========================================================================

      df_mod <- reactive({
        force_reactive(df_ll)
      })

      geo_select <- reactive({
        gd <- force_reactive(geo_data)
        if (length(gd) == 1) {
          gd[[1]]
        } else {
          gd[[input$geo_level]]
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
        var_list <- c(purrr::set_names("n", n_lab), group_vars)
        map_var_lab <- names(var_list[var_list == map_var])

        # save as reactive values
        rv$geo_join <- geo_join
        rv$geo_col <- geo_col
        rv$geo_col_sym <- geo_col_sym
        rv$geo_name_col <- geo_name_col
        rv$geo_name_col_sym <- geo_name_col_sym
        rv$geo_level_name <- geo_level_name
        rv$map_var <- map_var
        rv$map_var_sym <- map_var_sym
        rv$map_var_lab <- map_var_lab
        rv$sf <- sf
      })

      # ==========================================================================
      # MAP
      # ==========================================================================
      output$map <- leaflet::renderLeaflet({
        bbox <- sf::st_bbox(geo_data[[1]]$sf)
        leaf_basemap(bbox, miniMap = TRUE)
      })

      # observe({
      #   leaflet::leafletProxy("map", session) %>%
      #     leaflet::removeControl("var-lab") %>%
      #     leaflet::addControl(
      #       html = tags$div(tag_map_title, shiny::HTML(rv$map_var_lab)),
      #       className = "map-title",
      #       position = "topright",
      #       layerId = "var-lab"
      #     )
      # })

      observe({
        boundaries <- rv$sf
        req(nrow(boundaries) > 0)
        leaflet::leafletProxy("map", session) %>%
          leaflet::clearGroup("Boundaries") %>%
          leaflet.minicharts::clearMinicharts() %>%
          leaflet::addPolygons(
            data = boundaries,
            stroke = TRUE,
            color = "grey",
            weight = 1,
            fillOpacity = 0,
            label = boundaries[[rv$geo_name_col]],
            group = "Boundaries",
            options = leaflet::pathOptions(pane = "boundaries")
          ) %>%
          leaflet.minicharts::addMinicharts(
            boundaries$lon,
            boundaries$lat,
            layerId = boundaries$pcode,
            chartdata = 1,
            width = 0,
            height = 0
          )
      }) %>% bindEvent(rv$sf)

      # map circles/pies ===========================================

      df_geo_counts <- reactive({
        if (rv$map_var == "n") {
          var_lab <- "Patients"
          df_counts <- df_mod() %>%
            dplyr::count(.data[[rv$geo_col]], name = var_lab) %>%
            dplyr::mutate(total = .data[[var_lab]])
        } else {
          df_counts <- df_mod() %>%
            janitor::tabyl(.data[[rv$geo_col]], .data[[rv$map_var]], show_missing_levels = FALSE) %>%
            janitor::adorn_totals("col", name = "total")
        }

        sf::st_drop_geometry(rv$sf) %>%
          dplyr::select(pcode, name = rv$geo_name_col, lon, lat) %>%
          dplyr::left_join(df_counts, by = rv$geo_join) %>%
          dplyr::mutate(dplyr::across(dplyr::where(is.numeric), as.double)) %>%
          dplyr::mutate(dplyr::across(dplyr::where(is.double), ~ dplyr::if_else(is.na(.x), 0, .x)))
      }) %>% bindEvent(df_mod(), rv$sf, rv$map_var)

      observe({
        df_map <- df_geo_counts()

        if (isTruthy(nrow(df_map) > 0)) {
          chart_data <- df_map %>% dplyr::select(-pcode, -name, -lon, -lat, -total)
          pie_width <- (input$circle_size_mult * 10) * (sqrt(df_map$total) / sqrt(max(df_map$total)))

          leaflet::leafletProxy("map", session) %>%
            leaflet.minicharts::updateMinicharts(
              layerId = df_map$pcode,
              chartdata = chart_data,
              opacity = .7,
              fillColor = epi_pals()$d310[1],
              colorPalette = epi_pals()$d310,
              legend = TRUE,
              showLabels = TRUE,
              labelStyle = htmltools::css(font_family = "'Roboto Mono',sans-serif"),
              type = "pie",
              width = pie_width
            )
        } else {
          leaflet::leafletProxy("map", session) %>%
            leaflet.minicharts::updateMinicharts(
              layerId = df_map$pcode,
              chartdata = 1,
              width = 0,
              height = 0
            )
        }
      }) %>% bindEvent(df_geo_counts(), input$circle_size_mult)

      # Map image export ==========================================================
      output$dl <- downloadHandler(
        filename = function() {
          glue::glue("EPI-MAP-{time_stamp()}.png")
        },
        content = function(file) {
          # show loading spinner
          w_map$show()
          on.exit(w_map$hide())

          # rebuild current map shown on dashboard
          boundaries <- rv$sf
          df_map <- df_geo_counts()
          chart_data <- df_map %>% dplyr::select(-pcode, -name, -lon, -lat, -total)

          # * 7 instead of * 10 like in the app map because
          # circles are coming out larger in the image export
          pie_width <- (input$circle_size_mult * 7) * (sqrt(df_map$total) / sqrt(max(df_map$total)))

          leaf_out <- leaflet::leaflet() %>%
            leaflet::fitBounds(
              input$map_bounds$east,
              input$map_bounds$south,
              input$map_bounds$west,
              input$map_bounds$north
            ) %>%
            leaflet::addMapPane(name = "boundaries", zIndex = 300) %>%
            leaflet::addMapPane(name = "choropleth", zIndex = 310) %>%
            leaflet::addMapPane(name = "circles", zIndex = 410) %>%
            leaflet::addMapPane(name = "region_highlight", zIndex = 420) %>%
            leaflet::addMapPane(name = "place_labels", zIndex = 320) %>%
            leaflet::addMiniMap(toggleDisplay = FALSE, position = "topleft") %>%
            leaflet::addControl(
              html = tags$b(rv$map_var_lab),
              position = "topright"
            ) %>%
            leaflet::addScaleBar(
              position = "bottomright",
              options = leaflet::scaleBarOptions(imperial = FALSE)
            ) %>%
            leaflet::addControl(
              html = shiny::HTML(filter_info()),
              className = "leaflet-control-attribution",
              position = "bottomleft"
            ) %>%
            leaflet::addPolygons(
              data = boundaries,
              stroke = TRUE,
              color = "grey",
              weight = 1,
              fillOpacity = 0,
              label = boundaries[[rv$geo_name_col]],
              group = "Boundaries",
              options = leaflet::pathOptions(pane = "boundaries")
            ) %>%
            leaflet.minicharts::addMinicharts(
              lng = boundaries$lon,
              lat = boundaries$lat,
              layerId = df_map$pcode,
              chartdata = chart_data,
              opacity = .8,
              fillColor = epi_pals()$d310[1],
              colorPalette = epi_pals()$d310,
              legend = TRUE,
              showLabels = TRUE,
              labelStyle = htmltools::css(font_family = "'Roboto Mono',sans-serif"),
              type = "pie",
              width = pie_width
            )

          tiles <- dplyr::recode(
            input$map_groups[[1]],
            "Light" = "CartoDB.PositronNoLabels",
            "OSM" = "OpenStreetMap",
            "OSM HOT" = "OpenStreetMap.HOT"
          )

          if (tiles == "CartoDB.PositronNoLabels") {
            leaf_out <- leaf_out %>%
              leaflet::addProviderTiles(tiles) %>%
              leaflet::addProviderTiles(
                "CartoDB.PositronOnlyLabels",
                options = leaflet::leafletOptions(pane = "place_labels")
              )
          } else {
            leaf_out <- leaf_out %>% leaflet::addProviderTiles(tiles)
          }

          mapview::mapshot2(
            leaf_out,
            file = file,
            remove_controls = c(
              "zoomControl",
              "layersControl",
              "homeButton",
              "drawToolbar",
              "easyButton"
            ),
            selfcontained = FALSE,
            vwidth = export_width,
            vheight = export_height,
            zoom = 2,
            delay = 0.5
          )
        }
      )

      # return map shape click information to main app
      shiny::reactive({
        input$map_shape_click
      })
    }
  )
}
